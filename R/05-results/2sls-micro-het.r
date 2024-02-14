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
    )
  ) |>
  melt(id.vars = 'pctl')
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
        tot_births = fsum(tot_inf_births)
      ),
      keyby = year
    ] |>
    melt(id.var = 'year')

# Function to extract estimates -----------------------------------------------
extract_pred_bw_effects = function(mod_path){
  mod = qread(mod_path)
  pred_bw_dt = 
      cbind(
        data.table(coeftable(mod)),
        data.table(confint(mod))[,.(ci_l = `2.5 %`, ci_h = `97.5 %`)]
      ) |>
      clean_names()
  # Adding info from filename 
  pred_bw_dt[,':='(
    spatial = str_extract(mod_path, '(?<=_spatial-).*(?=_het)'),
    cluster = str_extract(mod_path, '(?<=_cl-).*(?=_glynl)')
  )]
  # Some cleaning of fixef names
  pred_bw_dt[,':='(
    fixef_num = fcase(
      fixef == 'year_month + fips_res + fips_occ', "No FEs",
      fixef == 'year_month + fips_res + fips_occ + sex + mage + mrace + mhisp + meduc + mar + birth_facility + restatus + total_birth_order', 'Mother FEs',
      fixef == 'year_month + fips_res + fips_occ + fage + fhisp + frace', 'Father FEs',
      fixef == 'year_month + fips_res + fips_occ + sex + mage + mrace + mhisp + meduc + mar + birth_facility + restatus + total_birth_order + fage + fhisp + frace', 'Mother and Father FEs'
    ) |> factor(levels = c('No FEs','Mother FEs','Father FEs','Mother and Father FEs')),
    control_num = fcase(
      str_detect(rhs, 'atrazine') & str_detect(rhs, 'unemployment'), 'Pesticides and Unemployment',
      str_detect(rhs, 'atrazine'), 'Pesticides',
      str_detect(rhs, 'unemployment'), 'Unemployment',
      default = 'None'
    ) |> factor(levels = c('None','Pesticides','Unemployment','Pesticides and Unemployment')),
    trt = str_extract(mod_path, '(?<=iv-).*(?=_cl)')
  )]
  pred_bw_dt[,':='(
    var_of_interest = str_detect(coefficient, 'fit_glyph_km2'),
    trt_name = fcase(
      trt == 'allyielddiffgmo500', 'Attainable Yield, GM Avg Split at Median',
      trt == 'allyielddiffpercentilegmo', 'Attainable Yield, GM Avg Percentile',
      trt == 'allyielddiffpercentilegmomax', 'Attainable Yield, GM Max Percentile',
      trt == 'e100myielddiffpercentilegmo', 'Attainable Yield, GM Avg Percentile, Eastern US',
      trt == 'percentilegmacres','1990-1995 GM Acreage Percentile'
    )
  )]
  return(pred_bw_dt)
}

# Plotting the results --------------------------------------------------------
plot_predbw_results = function(
  outcome_in, pctl_est_dt, print = TRUE, width_in = 6, height_in = 3
){
  y_lab = fcase(
    outcome_in == 'dbwt', 'Birthweight (g)',
    outcome_in == 'any_anomaly', 'Any Anomaly (%)',
    outcome_in == 'c_section', 'C Section (%)',
    outcome_in == 'dbwt_pctl_pre', 'Birthweight Pctl (%)',
    outcome_in == 'dbwt_pred', 'Predicted Birthweight (g)',
    outcome_in == 'gestation', 'Gestation (weeks)',
    outcome_in == 'i_lbw', 'Low Birthweight (%)',
    outcome_in == 'i_vlbw', 'Very Low Birthweight (%)',
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
    width = width_in, height = height_in
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
    width = width_in, height = height_in
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
        sample_var != 'month'
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
    width = width_in*1.25, height = height_in*1.25
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
    width = width_in, height = height_in
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
    width = width_in*1.25, height = height_in*1.25
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
pred_bw_dt = lapply(mod_paths, extract_pred_bw_effects) |> rbindlist()
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
  effect_at_mean = estimate*mean_dt[year == 2012 & variable == 'glyph_km2']$value,effect_at_mean_h = ci_h*mean_dt[year == 2012 & variable == 'glyph_km2']$value
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
