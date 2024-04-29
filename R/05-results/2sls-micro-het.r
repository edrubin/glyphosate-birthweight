# Script to plot predicted BW and month of birth heterogeneity
library(pacman)
p_load(
  here, data.table, fst, fixest, ggplot2,
  stringr, qs, collapse, janitor
)
# Some setup work -------------------------------------------------------------  
  # Table to turn splits into percentiles 
  cut_dt = data.table(
    pctl = seq(0,1,by = 0.01),
    pred_q5 = cut(
      x = seq(0,1,by = 0.01),
      breaks = 5,
      labels = 1:5, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q4 = cut(
      x = seq(0,1,by = 0.01),
      breaks = 4,
      labels = 1:4, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q3 = cut(
      x = seq(0,1,by = 0.01),
      breaks = 3,
      labels = 1:3, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q10 = cut(
      x = seq(0,1,by = 0.01),
      breaks = 10,
      labels = 1:10, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q14 = cut(
      x = seq(0,1,by = 0.01),
      breaks = c(0,0.01,0.05,seq(0.1,0.9,0.1),0.95, 0.99,1),
      labels = 1:14, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q20 = cut(
      x = seq(0,1,by = 0.01),
      breaks = 20,
      labels = 1:20, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  ) |>
  melt(id.vars = 'pctl')
  # Adding sex x predq5 interaction
  cut_dt = 
    merge(
      CJ(sex = c('M','F'), value = as.character(1:5)),
      cut_dt[variable == 'pred_q5'], 
      by = 'value', 
      allow.cartesian = TRUE
    )[,.(
      pctl, 
      variable = 'pred_q5_sex',
      value = paste0(value,'.',sex)  
    )] |>
    rbind(cut_dt[variable != 'pred_q5_sex'])
  # Calculating annual mean for outcomes and GLY 
  # TODO: Use the main estimating table for this rather than county-level table
  # TODO: Integrate with sample specific means
  comb_cnty_health_dt  = 
    read.fst(
      'data/clean/comb-cnty-health-dt.fst',
      as.data.table = TRUE
    )
  mean_dt = 
    comb_cnty_health_dt[
      !is.na(tot_inf_births) & rural == TRUE,
      .(glyph_km2 = fmean(glyph_km2, w = tot_inf_births),
        dbwt = fmean(mean_birth_wt, w = tot_inf_births), 
        i_lbw = fmean(low_birth_wt/tot_inf_births, w = tot_inf_births),
        i_vlbw = fmean(v_low_birth_wt/tot_inf_births, w = tot_inf_births),
        gestation = fmean(mean_gest,  w = tot_inf_births),
        i_preterm = fmean(preterm/tot_inf_births, w = tot_inf_births),
        c_section = fmean(tot_c_section/tot_inf_births, w = tot_inf_births),
        any_anomaly = fmean(any_anomaly/tot_inf_births, w = tot_inf_births),
        tot_births = fsum(tot_inf_births),
        glyph_km2_sd = fsd(glyph_km2, w = tot_inf_births)
      ),
      keyby = year
    ] |>
    melt(id.var = 'year')

# Function to extract estimates -----------------------------------------------
extract_pred_bw_effects = function(mod_path){
  print(paste0('starting', mod_path))
  mod = qread(mod_path)
  if(class(mod) == 'fixest_multi'){
    nmod = length(mod)
    mod_1 = mod[[1]]
    raw_coeftable = data.table(coeftable(mod))
  }else if(class(mod) == 'fixest'){
    nmod = 1
    mod_1 = mod
    raw_coeftable = coeftable(mod) |>
      data.table(keep.rownames = TRUE) |>
      setnames('rn','coefficient')
  }
  pred_bw_dt = 
    cbind(
      raw_coeftable,
      data.table(confint(mod))[,.(ci_l = `2.5 %`, ci_h = `97.5 %`)]
    ) |>
    clean_names()
  rm(raw_coeftable)
  # Adding info from filename 
  pred_bw_dt[,':='(
    cluster = str_extract(mod_path, '(?<=_cl-).*(?=_glynl)'),
    county_subset = str_extract(mod_path, '(?<=_county-)[a-zA-Z-]+(?=_)'),
    spatial = str_extract(mod_path, '(?<=_spatial-)[ ;a-zA-Z-]+(?=_)'),
    trt = str_extract(mod_path, '(?<=iv-).*(?=_cl)')
  )]
  # Adding missing columns if they don't exist
  if (!exists('sample', where = pred_bw_dt)) {
      pred_bw_dt[, sample := NA_character_]
  }
  if (!exists('sample_var', where = pred_bw_dt)) {
    pred_bw_dt[, sample_var := NA_character_]
  }
  if (!exists('rhs', where = pred_bw_dt)) {
    pred_bw_dt[, rhs := as.character(mod_1$fml_all$linear)[3]]
  }
  if (!exists('fixef', where = pred_bw_dt)) {
    pred_bw_dt[, fixef := as.character(mod_1$fml_all$fixef)[2]]
  }
  if (!exists('lhs', where = pred_bw_dt)) {
    pred_bw_dt[, lhs := as.character(mod_1$fml_all$linear)[2]]
  }
  if (!exists('id', where = pred_bw_dt) & nmod == 1) {
    pred_bw_dt[, id := 1]
  }
  # Some cleaning of fixef names
  pred_bw_dt[,':='(
    fixef_num = fcase(
      fixef == 'year_month + fips_res + fips_occ', "No Add'l FEs",
      fixef == 'year_month + fips_res + fips_occ + sex + mage + mrace + mhisp + meduc + mar + birth_facility + restatus + total_birth_order', 'Mother FEs',
      fixef == 'year_month + fips_res + fips_occ + fage + fhisp + frace', 'Father FEs',
      fixef == 'year_month + fips_res + fips_occ + sex + mage + mrace + mhisp + meduc + mar + birth_facility + restatus + total_birth_order + fage + fhisp + frace', 'Mother and Father FEs'
    ) |> factor(levels = c("No Add'l FEs",'Mother FEs','Father FEs','Mother and Father FEs')),
    control_num = fcase(
      str_detect(rhs, 'atrazine') & str_detect(rhs, 'unemployment'), 'Pesticides and Unemployment',
      str_detect(rhs, 'atrazine'), 'Pesticides',
      str_detect(rhs, 'unemployment'), 'Unemployment',
      default = 'None'
    ) |> factor(levels = c('None','Pesticides','Unemployment','Pesticides and Unemployment')),
    lhs = lhs |> str_remove('^c\\(') |> str_remove('\\)$')
  )]
  pred_bw_dt[,':='(
    var_of_interest = str_detect(coefficient, 'fit_glyph_km2'),
    trt_name = fcase(
      trt == 'allyielddiffgmo500', 'Attainable Yield, GM Avg Split at Median',
      trt == 'allyielddiffpercentilegmo', 'Attainable Yield, GM Avg Percentile',
      trt == 'allyielddiffpercentilegmomax', 'Attainable Yield, GM Max Percentile',
      trt == 'e100myielddiffpercentilegmo', 'Attainable Yield, GM Avg Percentile',
      trt == 'percentilegmacres','1990-1995 GM Acreage Percentile'
    ),
    county_subset = fcase(
      spatial == 'rural' & trt == 'e100myielddiffpercentilegmo', 'Rural, Eastern US',
      spatial == 'rural' & county_subset == 'mw-ne', 'Rural, Midwest and Northeast',
      spatial == 'rural' & county_subset == 'south', 'Rural, South',
      spatial == 'rural' & county_subset == 'south-nofl', 'Rural, South, no FL',
      spatial == 'urban res; urban occ', 'Non-rural',
      spatial == 'rural res; rural occ', 'Rural residence & occurrence',
      default = 'Rural'
    ) |> 
    factor(levels = c(
      "Rural", "Rural residence & occurrence", "Rural, Midwest and Northeast", 
      "Rural, Eastern US", "Rural, South", "Rural, South, no FL", 
      "Non-rural"
    ))
  )]
  return(pred_bw_dt)
}

# Plotting the results --------------------------------------------------------
plot_predbw_results = function(
  outcome_in, pctl_est_dt, print = TRUE, width_in = 6, height_in = 4
){
  y_lab = fcase(
    outcome_in == 'dbwt', 'Birthweight (g)',
    outcome_in == 'any_anomaly', 'Any Anomaly (%)',
    outcome_in == 'c_section', 'C Section (%)',
    outcome_in == 'dbwt_pctl_pre', 'Birthweight Pctl (%)',
    outcome_in == 'dbwt_pred', 'Pred Birthweight (g)',
    outcome_in == 'gestation', 'Gestation (weeks)',
    outcome_in == 'i_lbw', 'LBW (%)',
    outcome_in == 'i_vlbw', 'VLBW (%)',
    outcome_in == 'i_preterm', 'Preterm (%)',
    default = 'Estimate'
  )
  if(str_detect(y_lab, '%')){
    y_labels = scales::label_percent()
  } else {
    y_labels = scales::label_comma()
  }
  # First the main plot---with just deciles 
  decile_p = 
    ggplot(
      data = pctl_est_dt[
        lhs == outcome_in & 
        trt == 'allyielddiffpercentilegmo' &
        sample_var == 'pred_q10' & 
        var_of_interest == TRUE & 
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment',
        -'pctl'
      ] |> unique(), 
      aes(x = as.integer(sample), y = estimate, ymin = ci_l, ymax = ci_h),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(
      name = 'Predicted Birthweight Decile',
      breaks = 1:10
    ) +
    scale_y_continuous(
      name = paste0('Maringal Effect on ',y_lab),
      labels = y_labels
    ) + 
    theme(panel.grid.minor = element_blank())
  if(print) print(decile_p)
  ggsave(
    plot = decile_p,
    filename = here(paste0(
      'figures/micro/2sls/deciles-',outcome_in,'.jpeg'
    )),
    width = width_in, height = height_in*0.85
  )
  # Effect at mean for deciles 
  decile_effatmean_p = 
    ggplot(
      data = pctl_est_dt[
        lhs == outcome_in & 
        trt == 'allyielddiffpercentilegmo' &
        sample_var == 'pred_q10' & 
        var_of_interest == TRUE & 
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment',
        -'pctl'
      ] |> unique(), 
      aes(
        x = as.integer(sample), 
        y = effect_at_mean, ymin = effect_at_mean_l, ymax = effect_at_mean_h
      ),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(
      name = 'Predicted Birthweight Decile',
      breaks = 1:10
    ) +
    scale_y_continuous(
      name = paste0('Effect at Mean on ',y_lab),
      labels = y_labels
    ) + 
    theme(panel.grid.minor = element_blank())
  if(print) print(decile_effatmean_p)
  ggsave(
    plot = decile_effatmean_p,
    filename = here(paste0(
      'figures/micro/2sls/deciles-at-mean-',outcome_in,'.jpeg'
    )),
    width = width_in, height = height_in*0.85
  )
  # Now a plot with all three splits 
  all_splits_p = 
    ggplot(
      data = pctl_est_dt[
        lhs == outcome_in & 
        var_of_interest == TRUE & 
        trt == 'allyielddiffpercentilegmo' &
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' & 
        sample_var != 'month' & 
        sample_var != 'pred_q14'
      ], 
      aes(
        x = pctl, y = estimate, ymin = ci_l, ymax = ci_h,
        color = str_remove(sample_var, 'pred_q')|> as.integer() |> as.factor(), 
        fill  = str_remove(sample_var, 'pred_q')|> as.integer() |> as.factor()
      ),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_line() +
    scale_color_brewer(
      'Number of Bins', 
      palette = "Dark2",
      aesthetics = c('color','fill')
    ) +
    scale_x_continuous(
      name = 'Predicted Birthweight Percentile',
      labels = scales::label_percent()
    ) +
    scale_y_continuous(
      name = paste0('Maringal Effect on ',y_lab),
      labels = y_labels
    ) + 
    theme(panel.grid.minor = element_blank())
  if(print) print(all_splits_p)
  ggsave(
    plot = all_splits_p,
    filename = here(paste0(
      'figures/micro/2sls/all-splits-',outcome_in,'.jpeg'
    )),
    width = width_in*1.25, height = height_in
  )
  # Robustness in decile plot to fixef and controls 
  decile_cntrls_p = 
    ggplot(
      data = pctl_est_dt[
        lhs == outcome_in &
        trt == 'allyielddiffpercentilegmo' &
        var_of_interest == TRUE & 
        sample_var == 'pred_q10', 
        -'pctl'
      ] |> unique(), 
      aes(
        x = as.integer(sample), y = estimate, ymin = ci_l, ymax = ci_h,
        color = control_num, fill = control_num
      ),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_point() + 
    geom_line() +
    scale_color_brewer(
      'Controls', palette = "Dark2",
      aesthetics = c('color','fill')
    ) +
    scale_x_continuous(
      name = 'Predicted Birthweight Decile',
      breaks = 1:10
    ) +
    scale_y_continuous(
      name = paste0('Maringal Effect on ',y_lab),
      labels = y_labels
    ) + 
    facet_grid(cols = vars(fixef_num)) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = 'bottom'
    ) 
  if(print) print(decile_cntrls_p)
  ggsave(
    plot = decile_cntrls_p,
    filename = here(paste0(
      'figures/micro/2sls/deciles-robust-cntrl-',outcome_in,'.jpeg'
    )),
    width = width_in*1.25, height = height_in
  )
  # Robustness to alt treatment 
  quintile_trt_p = 
    ggplot(
      data = pctl_est_dt[
        lhs == outcome_in &
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' &
        var_of_interest == TRUE & 
        sample_var == 'pred_q5', 
        -'pctl'
      ] |> unique(), 
      aes(
        x = as.integer(sample), y = estimate, ymin = ci_l, ymax = ci_h,
        color = trt_name, fill = trt_name
      ),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_line() +
    geom_point() + 
    scale_color_brewer(
      'Instrument', palette = "Dark2",
      aesthetics = c('color','fill')
    ) +
    scale_x_continuous(
      name = 'Predicted Birthweight Quintile',
      breaks = 1:10
    ) +
    scale_y_continuous(
      name = paste0('Maringal Effect on ',y_lab),
      labels = y_labels
    ) + 
    theme(
      panel.grid.minor = element_blank(),
      legend.position = 'bottom'
    ) +
    guides(color = guide_legend(nrow = 2))   
  if(print) print(quintile_trt_p)
  ggsave(
    plot = quintile_trt_p,
    filename = here(paste0(
      'figures/micro/2sls/quintile-robust-trt-',outcome_in,'.jpeg'
    )),
    width = width_in*1.25, height = height_in*1.25
  )
  # Now for heterogeneity by month of birth 
  month_p = 
    ggplot(
      data = pctl_est_dt[
        lhs == outcome_in & 
        trt == 'allyielddiffpercentilegmo' &
        sample_var == 'month' & 
        var_of_interest == TRUE & 
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' & 
        sample != 'Full sample'
      ], 
      aes(x = as.integer(sample), y = estimate, ymin = ci_l, ymax = ci_h),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(
      name = 'Month of Birth',
      breaks = 1:12
    ) +
    scale_y_continuous(
      name = paste0('Maringal Effect on ',y_lab),
      labels = y_labels
    ) + 
    theme(panel.grid.minor = element_blank())
  if(print) print(month_p)
  ggsave(
    plot = month_p,
    filename = here(paste0(
      'figures/micro/2sls/month-',outcome_in,'.jpeg'
    )),
    width = width_in, height = height_in*0.85
  )
  # Robustness in month plot to fixef and controls 
  month_cntrls_p = 
    ggplot(
      data = pctl_est_dt[
        lhs == outcome_in & 
        trt == 'allyielddiffpercentilegmo' &
        sample_var == 'month' & 
        var_of_interest == TRUE & 
        sample != 'Full sample'
      ], 
      aes(
        x = as.integer(sample), y = estimate, ymin = ci_l, ymax = ci_h,
        color = control_num, fill = control_num
      ),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_point() + 
    geom_line() +
    scale_color_brewer(
      'Controls', palette = "Dark2",
      aesthetics = c('color','fill')
    ) +
    scale_x_continuous(
      name = 'Month of Birth',
      breaks = 1:12
    ) +
    scale_y_continuous(
      name = paste0('Maringal Effect on ',y_lab),
      labels = y_labels
    ) + 
    facet_grid(cols = vars(fixef_num)) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = 'bottom'
    ) 
  if(print) print(month_cntrls_p)
  ggsave(
    plot = month_cntrls_p,
    filename = here(paste0(
      'figures/micro/2sls/month-robust-cntrl-',outcome_in,'.jpeg'
    )),
    width = width_in*1.25, height = height_in
  )
  # Quintile by sex plots 
  quintile_sex_p = 
    ggplot(
      data = pctl_est_dt[
        lhs == outcome_in &
        trt == 'allyielddiffpercentilegmo' &
        var_of_interest == TRUE & 
        sample_var == 'pred_q5_sex' & 
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' & 
        sample != 'Full sample', 
        -'pctl'
      ] |> unique(), 
      aes(
        x = as.integer(str_extract(sample,'\\d')), y = estimate, ymin = ci_l, ymax = ci_h,
        color = str_extract(sample, 'M|F'), fill = str_extract(sample, 'M|F')
      ),
    ) + 
    geom_hline(yintercept = 0) +
    #geom_ribbon(alpha = 0.3, color = NA) + 
    geom_pointrange(position = position_dodge(width = 0.25), linewidth = 1.1) + 
    #geom_line() +
    scale_color_brewer(
      'Sex', palette = "Dark2",
      aesthetics = c('color','fill')
    ) +
    scale_x_continuous(
      name = 'Predicted Birthweight Quintile',
      breaks = 1:5
    ) +
    scale_y_continuous(
      name = paste0('Maringal Effect on ',y_lab),
      labels = y_labels
    ) + 
    theme(
      panel.grid.minor = element_blank(),
      legend.position = 'bottom'
    ) 
  if(print) print(quintile_sex_p)
  ggsave(
    plot = quintile_sex_p,
    filename = here(paste0(
      'figures/micro/2sls/predq5-sex-',outcome_in,'.jpeg'
    )),
    width = width_in, height = height_in
  )

}  

# Function to make plots with all outcomes that are faceted -------------------
plot_predbw_results_all_outcome = function(
  pctl_est_dt, print = TRUE, width_in = 6, height_in = 4
){
  all_splits_p = 
    ggplot(
      data = pctl_est_dt[
        var_of_interest == TRUE & 
        trt == 'allyielddiffpercentilegmo' &
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' & 
        sample_var != 'month' & 
        sample_var != 'pred_q14' &
        lhs != 'dbwt_pred'
      ], 
      aes(
        x = pctl, y = estimate, ymin = ci_l, ymax = ci_h,
        color = str_remove(sample_var, 'pred_q')|> as.integer() |> as.factor(), 
        fill  = str_remove(sample_var, 'pred_q')|> as.integer() |> as.factor()
      ),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_line() +
    scale_color_brewer(
      'Number of Pred BW Bins', 
      palette = "Dark2",
      aesthetics = c('color','fill')
    ) +
    scale_x_continuous(
      name = 'Predicted Birthweight Percentile',
      labels = scales::label_percent()
    ) +
    scale_y_continuous(name = 'Maringal Effect') + 
    theme(
      panel.grid.minor = element_blank(),
      legend.position = 'bottom',
      strip.text = element_text(size = 16)
    ) + 
    facet_wrap(~lhs_name, ncol = 2, scales = 'free_y')
  if(print) print(all_splits_p)
  ggsave(
    plot = all_splits_p,
    filename = here(paste0(
      'figures/micro/2sls/all-splits-all-outcomes.jpeg'
    )),
    width = width_in*1.5, height = height_in*2.75
  )
  # Now for heterogeneity by month of birth 
  month_p = 
    ggplot(
      data = pctl_est_dt[
        lhs != 'dbwt_pred' & 
        trt == 'allyielddiffpercentilegmo' &
        sample_var == 'month' & 
        var_of_interest == TRUE & 
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' & 
        sample != 'Full sample'
      ], 
      aes(x = as.integer(sample), y = estimate, ymin = ci_l, ymax = ci_h),
    ) + 
    geom_hline(yintercept = 0) +
    geom_ribbon(alpha = 0.3, color = NA) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(
      name = 'Month of Birth',
      breaks = 1:12
    ) +
    scale_y_continuous(name = 'Maringal Effect') + 
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 16)
    ) +
    facet_wrap(~lhs_name, scales = 'free_y', ncol = 2)
  if(print) print(month_p)
  ggsave(
    plot = month_p,
    filename = here(paste0(
      'figures/micro/2sls/month-all-outcomes.jpeg'
    )),
    width = width_in*1.5, height = height_in*2.75
  )
  # Quintile by sex plots 
  quintile_sex_p = 
    ggplot(
      data = pctl_est_dt[
        lhs != 'dbwt_pred' &
        trt == 'allyielddiffpercentilegmo' &
        var_of_interest == TRUE & 
        sample_var == 'pred_q5_sex' & 
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' & 
        sample != 'Full sample', 
        -'pctl'
      ] |> unique(), 
      aes(
        x = as.integer(str_extract(sample,'\\d')), y = estimate, ymin = ci_l, ymax = ci_h,
        color = str_extract(sample, 'M|F'), fill = str_extract(sample, 'M|F')
      ),
    ) + 
    geom_hline(yintercept = 0) +
    #geom_ribbon(alpha = 0.3, color = NA) + 
    geom_pointrange(position = position_dodge(width = 0.25), linewidth = 1.1) + 
    #geom_line() +
    scale_color_brewer(
      'Sex', palette = "Dark2",
      aesthetics = c('color','fill')
    ) +
    scale_x_continuous(
      name = 'Predicted Birthweight Quintile',
      breaks = 1:5
    ) +
    scale_y_continuous(
      name = 'Maringal Effect'
    ) + 
    theme(
      panel.grid.minor = element_blank(),
      legend.position = 'bottom'
    ) +
    facet_wrap(~lhs_name, scales = 'free_y', ncol = 2)
  if(print) print(quintile_sex_p)
  ggsave(
    plot = quintile_sex_p,
    filename = here(paste0(
      'figures/micro/2sls/predq5-sex-all-outcomes.jpeg'
    )),
     width = width_in*1.5, height = height_in*2.75
  )
}

# Making all of the plots! ----------------------------------------------------
theme_set(theme_minimal(base_size = 12))
# Loading the data from different pred splits 
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_2sls_outcome'
  ) |>
  str_subset('het-pred|het-month')
pred_bw_dt = 
  lapply(mod_paths, extract_pred_bw_effects) |> 
  rbindlist(use.names = TRUE, fill = TRUE)
# Merging with the estimates so we can plot with pctl on x axis 
pctl_est_dt = 
  merge(
    pred_bw_dt[sample_var != 'month'], 
    cut_dt, 
    by.x = c('sample_var','sample'),
    by.y = c('variable','value'),
    allow.cartesian = TRUE
  ) |>
  rbind(
    pred_bw_dt[sample_var == 'month'],
    use.names=TRUE , fill = TRUE 
  )
# Calculating effect at mean 
pctl_est_dt[,':='(
  effect_at_mean_l = ci_l*mean_dt[year == 2012 & variable == 'glyph_km2']$value,
  effect_at_mean = estimate*mean_dt[year == 2012 & variable == 'glyph_km2']$value,effect_at_mean_h = ci_h*mean_dt[year == 2012 & variable == 'glyph_km2']$value,
  lhs_name = fcase(
    lhs == 'dbwt', 'Birthweight (g)',
    lhs == 'any_anomaly', 'Pr(Any Anomaly)',
    lhs == 'c_section', 'Pr(C Section)',
    lhs == 'dbwt_pctl_pre', 'Birthweight Percentile',
    lhs == 'dbwt_pred', 'Predicted Birthweight (g)',
    lhs == 'gestation', 'Gestation (weeks)',
    lhs == 'i_lbw', 'Pr(Low Birthweight)',
    lhs == 'i_vlbw', 'Pr(Very Low Birthweight)',
    lhs == 'i_preterm', 'Pr(Preterm)',
    lhs == 'i_female', 'Pr(Female)',
    lhs == 'i_m_black', 'Pr(Mother Black)',
    lhs == 'i_m_nonwhite', 'Pr(Mother Non-white)',
    lhs == 'i_m_hispanic', 'Pr(Mother Hispanic)',
    lhs == 'i_m_married', 'Pr(Mother Married)'
  ) 
)]
# Making the plots 
lapply(
  unique(pctl_est_dt$lhs),
  plot_predbw_results,
  pctl_est_dt = pctl_est_dt, 
  print = FALSE, 
  width_in = 6*1.05, 
  height_in = 4*1.05
)
plot_predbw_results_all_outcome(pctl_est_dt)


# Getting avg bwt by pred bwt decile 
summary_dt = read_fst(
  here('data/clean/prediction/summaries/pctlpred-ruralres-ecdfres.fst'),
  as.data.table = TRUE
)
summary_dt[,
  dbwt_decile := dbwt_pred_rnd_pctl_ruralres_pre |> as.character() |> str_sub(1,3)
]
summary_dt[,
  dbwt_decile := fcase(
    dbwt_decile == '0', '0.0',
    dbwt_decile == '1', '0.9',
    str_detect(dbwt_decile, '0\\.\\d'), dbwt_decile
  )
]
p_load(collapse)
summary_dt |>
fselect(dbwt, dbwt_decile, n) |>
gby(dbwt_decile) |>
fmean(w = n)

# Now making the spec charts --------------------------------------------------
source(here('R/functions/spec_chart_function.R'))
# Getting coefficients  
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_2sls_outcome'
  ) |>
  str_subset('percentilegmacres', negate = TRUE) 
pred_bw_dt = 
  lapply(mod_paths, extract_pred_bw_effects) |> 
  rbindlist(use.names = TRUE, fill = TRUE)
# Setup for spec chart 
spec_chart_outcome = function(
  pred_bw_dt, 
  outcome_in,
  order_in = 'decreasing'
){
  print(outcome_in)
  y_lab = fcase(
    outcome_in == 'dbwt', 'Birthweight (g)',
    outcome_in == 'any_anomaly', 'Any Anomaly (%)',
    outcome_in == 'c_section', 'C Section (%)',
    outcome_in == 'dbwt_pctl_pre', 'Birthweight Pctl (%)',
    outcome_in == 'dbwt_pred', 'Pred Birthweight (g)',
    outcome_in == 'gestation', 'Gestation (weeks)',
    outcome_in == 'i_lbw', 'LBW (%)',
    outcome_in == 'i_vlbw', 'VLBW (%)',
    outcome_in == 'i_preterm', 'Preterm (%)',
    default = 'Estimate'
  )
  if(str_detect(y_lab, '%')){
    y_labels = scales::label_percent()
  } else {
    y_labels = scales::label_comma()
  }
  spec_dt_robust = 
    pred_bw_dt[
      lhs == outcome_in & 
      var_of_interest == TRUE & 
      county_subset == 'Rural' & 
      is.na(sample_var) 
      #| (sample_var == 'i_m_nonwhite' & sample != 'Full sample'))
    ]
  spec_dt_spatial = 
    pred_bw_dt[
      lhs == outcome_in & 
      var_of_interest == TRUE & 
      trt == 'allyielddiffpercentilegmo' &
      fixef_num == 'Mother and Father FEs' & 
      control_num == 'Pesticides and Unemployment' & 
      is.na(sample_var) & 
      county_subset != 'Rural residence & occurrence'
    ]
  spec_dt_mrace = 
    pred_bw_dt[
      lhs == outcome_in & 
      var_of_interest == TRUE & 
      sample_var == 'i_m_nonwhite'
    ]
  spec_dt_mrace[,
    sample := fcase(
      sample == 1, 'Mother non-white', 
      sample == 0, 'Mother white',
      sample == 'Full sample', 'Full sample'
    ) |> factor(levels = c('Full sample','Mother white','Mother non-white'))
  ]
  # Getting values for each group of options
  control_num_v = unique(spec_dt_robust$control_num)
  fixef_num_v = unique(spec_dt_robust$fixef_num)
  trt_name_v = unique(spec_dt_robust$trt_name)
  county_subset_v = unique(spec_dt_spatial$county_subset)
  # Adding indicator variables for each option
  spec_dt_robust[,
    (paste0("i_control_num_",make_clean_names(control_num_v))) := 
      lapply(control_num_v,\(x){control_num == x})
  ]
  spec_dt_robust[,
    (paste0("i_fixef_num_",make_clean_names(fixef_num_v))) := 
      lapply(fixef_num_v,\(x){fixef_num == x})
  ]
  spec_dt_robust[,
    (paste0("i_trt_name_",make_clean_names(trt_name_v))) := 
      lapply(trt_name_v,\(x){trt_name == x})
  ]
  # Selecting coef/std error and dummy colummns for options
  cols_robust = c(
    "estimate","std_error", 
    str_subset(colnames(spec_dt_robust),"^i_")
  )
  # Creating new data frame with just the columns needed
  spec_df_robust = spec_dt_robust[,..cols_robust] |> as.data.frame()
  # Creating labels 
  labels_robust = list(
    "Controls" = control_num_v |> as.character() |> str_replace(' and ', '+'), 
    "Fixed Effects" = fixef_num_v |> as.character(),
    "Attainable Yield Measure" = trt_name_v |> as.character() |> str_remove('Attainable Yield, ')
  )
  # Finding main spec to highlight
  hl = spec_dt_robust[
    control_num == 'Pesticides and Unemployment' & 
    fixef_num == 'Mother and Father FEs' & 
    trt_name == 'Attainable Yield, GM Avg Percentile' & 
    county_subset == 'Rural' & 
    is.na(sample),
    which = TRUE
  ]
  # First the Robustness chart
  jpeg(
    filename = here(paste0(
      "figures/micro/2sls/spec-chart-robust-",outcome_in,".jpeg")),
    quality = 100, res = 300, 
    width = 3000, height = 3000
  )
  # Setting margins
  par(oma=c(1,0,1,1))
  schart(
    spec_df_robust, 
    labels_robust, 
    highlight = hl,
    order = order_in,
    col.est=c("grey60","#e64173"), 
    col.est2=c("grey70","#e64173"),
    col.dot=c("grey60","grey95","grey95","#e64173"),
    fonts=c(2,3),
    ci=c(.95)
  )
  dev.off()
  # Now spatial subsets
  spatial_subset_p =
    ggplot(spec_dt_spatial, aes(x = county_subset)) +
    geom_point(aes(y = estimate)) + 
    geom_linerange(aes(ymin = ci_l, ymax = ci_h)) +
    geom_hline(yintercept = 0, linetype = 'dashed') + 
    scale_y_continuous(name = y_lab, labels = y_labels) + 
    scale_x_discrete(name = '', guide = guide_axis(n.dodge=2))+
    theme_minimal() 
  ggsave(
    spatial_subset_p, 
    filename = here(paste0(
      "figures/micro/2sls/spec-chart-spatial-",outcome_in,".jpeg"
    )), 
    width = 5, height = 3, bg = 'white'
  )
  # Finally mother's race
  mrace_p = 
    ggplot(
      data = spec_dt_mrace,
      aes(
        x = control_num, 
        y = estimate, 
        ymin = ci_l, ymax = ci_h, 
        color = sample
      )
    )  +
    geom_hline(yintercept = 0, linetype = 'solid', linewidth = 0.2) +
    geom_point(position = position_dodge(width = 0.4), size = 2) + 
    geom_linerange(position = position_dodge(width = 0.4), linewidth = 0.8) + 
    facet_grid(cols = vars(fixef_num)) + 
    scale_y_continuous(name = y_lab, labels = y_labels) +
    scale_x_discrete(name = 'Controls') + 
    scale_color_brewer(
      palette = 'Dark2', 
      name = ''
    ) +
    theme_minimal() 
  ggsave(
    mrace_p, 
    filename = here(paste0(
      "figures/micro/2sls/spec-chart-mrace-",outcome_in,".jpeg"
    )), 
    width = 7.5, height = 3.5, bg = 'white'
  )
}

lapply(
  unique(pred_bw_dt$lhs)[1:9],
  spec_chart_outcome,
  pred_bw_dt = pred_bw_dt,
  order_in = 'asis'
)

# Make spatial subset plot for all outcomes 
pred_bw_dt[,
  lhs_name := fcase(
    lhs == 'dbwt', 'Birthweight (g)',
    lhs == 'any_anomaly', 'Pr(Any Anomaly)',
    lhs == 'c_section', 'Pr(C Section)',
    lhs == 'dbwt_pctl_pre', 'Birthweight Percentile',
    lhs == 'dbwt_pred', 'Predicted Birthweight (g)',
    lhs == 'gestation', 'Gestation (weeks)',
    lhs == 'i_lbw', 'Pr(Low Birthweight)',
    lhs == 'i_vlbw', 'Pr(Very Low Birthweight)',
    lhs == 'i_preterm', 'Pr(Preterm)',
    lhs == 'i_female', 'Pr(Female)',
    lhs == 'i_m_black', 'Pr(Mother Black)',
    lhs == 'i_m_nonwhite', 'Pr(Mother Non-white)',
    lhs == 'i_m_hispanic', 'Pr(Mother Hispanic)',
    lhs == 'i_m_married', 'Pr(Mother Married)'
  )
] 
  # Now spatial subsets
  spatial_subset_p =
    ggplot(
      pred_bw_dt[
        lhs != 'dbwt_pred' & 
        county_subset != 'Non-rural'&
        var_of_interest == TRUE & 
        trt == 'allyielddiffpercentilegmo' &
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' & 
        is.na(sample_var) & 
        county_subset != 'Rural residence & occurrence'
      ], 
      aes(x = county_subset)
    ) +
    geom_point(aes(y = estimate)) + 
    geom_linerange(aes(ymin = ci_l, ymax = ci_h)) +
    geom_hline(yintercept = 0, linetype = 'dashed') + 
    scale_y_continuous(name = 'Marginal Effect') + 
    scale_x_discrete(name = '', guide = guide_axis(n.dodge=2))+
    theme_minimal() + 
    facet_wrap(~lhs_name, ncol = 2, scales = 'free_y')
  ggsave(
    spatial_subset_p, 
    filename = here(
      "figures/micro/2sls/spec-chart-spatial-all-outcomes.jpeg"
    ), 
    width = 6*1.5, height = 4*2.75,
    bg = 'white'
  )
