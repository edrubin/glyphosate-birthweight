# Main results ----------------------------------------------------------------
library(pacman)
p_load(
  here, data.table, fst, fixest, qs,
  stringr, janitor, ggplot2, magrittr
)

# Function to extract first stage from 2SLS models ----------------------------
fs_coeftable = function(i, mod_2sls){
  # First getting coefs and confint 
  mod_dt_raw = 
    cbind(
      data.table(
        coeftable(mod_2sls[[i]]$iv_first_stage$glyph_km2), 
        keep.rownames = TRUE
      ),
      data.table(
        confint(mod_2sls[[i]]$iv_first_stage$glyph_km2)
      )[,.(ci_l = `2.5 %`, ci_h = `97.5 %`)]
    ) |>
    clean_names()
  # Now adding the model info 
  mod_dt = mod_dt_raw[,.(
    id = i, 
    fixef = mod_2sls[[i]]$model_info$fixef, 
    lhs = mod_2sls[[i]]$model_info$lhs, 
    rhs = paste0(
      mod_2sls[[i]]$iv_inst_names, " + ",
      #mod_2sls[[i]]$model_info$rhs
      as.character(mod_2sls[[i]]$fml_all$linear)[3]
    ),
    sample_var = ifelse(
      is.null(mod_2sls[[i]]$model_info$sample), 
      NA_character_,
      mod_2sls[[i]]$model_info$sample$var
    ), 
    sample = ifelse(
      is.null(mod_2sls[[i]]$model_info$sample), 
      NA_character_,
      mod_2sls[[i]]$model_info$sample$value
    ),
    coefficient = rn,
    estimate, std_error, t_value, pr_t, ci_l, ci_h
  )]
  return(mod_dt)
}

# Function to extract data from event study models ----------------------------
extract_event_study_coefs = function(mod_path){
  print(paste('starting', mod_path))
  # Loading the model 
  mod = qread(mod_path)
  # Getting coefficients---depends on rf model or first stage
  if(str_detect(mod_path, 'est_rf')){
    # Extracting coefficients and ci for reduced form model
    mod_dt = 
      cbind(
        data.table(coeftable(mod)),
        data.table(confint(mod))[,.(ci_l = `2.5 %`, ci_h = `97.5 %`)]
      ) |>
      clean_names()
    mod_dt[,type:='rf']
    # Adding sample var and sample if not there
    if (!exists('sample', where = mod_dt)) {
      mod_dt[, 'sample' := NA_character_]
    }
    if (!exists('sample_var', where = mod_dt)) {
      mod_dt[, 'sample_var' := NA_character_]
    }
    if (!exists('rhs', where = mod_dt)) {
      mod_dt[, 'rhs' := as.character(mod[[1]]$fml_all$linear)[3]]
    }
    if (!exists('fixef', where = mod_dt)) {
      mod_dt[, 'fixef' := as.character(mod[[1]]$fml_all$fixef)[2]]
    }
    if (!exists('lhs', where = mod_dt)) {
      mod_dt[, 'lhs' := as.character(mod[[1]]$fml_all$linear)[2]]
    }
  }else if(str_detect(mod_path, 'est_2sls_outcome')){
    # Getting first stage coefs from 2SLS model
    mod_dt = lapply(
      1:length(mod),
      fs_coeftable, 
      mod_2sls = mod
    ) |> rbindlist()
    mod_dt[,type:='2sls-fs']
    if (!exists('rhs', where = mod_dt)) {
      mod_dt[, 'rhs' := as.character(mod[[1]]$fml_all$linear)[3]]
    }
    if (!exists('fixef', where = mod_dt)) {
      mod_dt[, 'fixef' := as.character(mod[[1]]$fml_all$fixef)[2]]
    }
    if (!exists('lhs', where = mod_dt)) {
      mod_dt[, 'lhs' := as.character(mod[[1]]$fml_all$linear)[2]]
    }
  } else {
    stop('not yet able to extract model coefs')
  }
  # Add reference coefficient 
  mod_dt %<>%
    rbind(
      mod_dt[,.(
        type,
        id, fixef, lhs, rhs, 
        sample_var, sample,
        coefficient = paste0(
          'year::1995:',
          str_extract(rhs, '(?<=i\\(year, ).*(?=, ref = 1995\\))')
        ),
        estimate = 0, ci_l = 0, ci_h = 0
        )
      ]|> unique(),
      use.names = TRUE, fill = TRUE
    )
  # Adding info from filename 
  mod_dt[,':='(
    spatial = str_extract(mod_path, '(?<=_spatial-)[a-zA-Z-]+(?=_)'),
    cluster = str_extract(mod_path, '(?<=_cl-)[a-zA-Z-]+(?=_)'),
    county_subset = str_extract(mod_path, '(?<=_county-)[a-zA-Z-]+(?=_)')
  )]
  # Some cleaning of fixef names
  mod_dt[,':='(
    year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.integer(),
    fixef_num = fcase(
      fixef == 'year_month + fips_res + fips_occ', "No Add'l FEs",
      fixef == 'year_month + fips_res + fips_occ + sex + mage + mrace + mhisp + meduc + mar + birth_facility + restatus + total_birth_order', 'Mother FEs',
      fixef == 'year_month + fips_res + fips_occ + fage + fhisp + frace', 'Father FEs',
      fixef == 'year_month + fips_res + fips_occ + sex + mage + mrace + mhisp + meduc + mar + birth_facility + restatus + total_birth_order + fage + fhisp + frace', 'Add Family FEs'
    ) |> factor(levels = c("No Add'l FEs",'Mother FEs','Father FEs','Add Family FEs')),
    control_num = fcase(
      str_detect(rhs, 'atrazine') & str_detect(rhs, 'unemployment'), 'Pesticides and Unemployment',
      str_detect(rhs, 'atrazine'), 'Pesticides',
      str_detect(rhs, 'unemployment'), 'Unemployment',
      default = 'None'
    ) |> factor(levels = c('None','Pesticides','Unemployment','Pesticides and Unemployment')),
    trt = str_extract(rhs, '(?<=i\\(year, ).*(?=, ref = 1995\\))')
  )]
  mod_dt[,':='(
    var_of_interest = str_detect(coefficient, trt),
    trt_name = fcase(
      trt == 'all_yield_diff_gmo_50_0', 'Attainable Yield, GM Avg Split at Median',
      trt == 'all_yield_diff_percentile_gmo', 'Attainable Yield, GM Avg Percentile',
      trt == 'all_yield_diff_percentile_gmo_max', 'Attainable Yield, GM Max Percentile',
      trt == 'e100m_yield_diff_percentile_gmo', 'Attainable Yield, GM Avg Percentile, Eastern US',
      trt == 'percentile_gm_acres','1990-1995 GM Acreage Percentile'
    ),
    county_subset = fcase(
      county_subset == 'mw-ne', 'Midwest and Northeast',
      county_subset == 'south', 'South',
      county_subset == 'south-nofl', 'South, no FL'
    ),
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
  # return the results 
  return(mod_dt)
}

# Function to plot the event study --------------------------------------------
plot_reduced_form = function(
  outcome_in, mod_dt, print = FALSE, width_in = 6, height_in = 3, pink = '#e64173'
){
  print(paste('Starting', outcome_in))
  # Making outcome labels and scales 
  y_lab = fcase(
    outcome_in == 'dbwt', 'Birthweight (g)',
    outcome_in == 'any_anomaly', 'Any Anomaly',# (%)',
    outcome_in == 'c_section', 'C Section',# (%)',
    outcome_in == 'dbwt_pctl_pre', 'Birthweight Pctl',# (%)',
    outcome_in == 'dbwt_pred', 'Predicted Birthweight (g)',
    outcome_in == 'gestation', 'Gestation (weeks)',
    outcome_in == 'i_lbw', 'Low Birthweight',# (%)',
    outcome_in == 'i_vlbw', 'Very Low Birthweight',# (%)',
    outcome_in == 'i_preterm', 'Preterm',# (%)',
    outcome_in == 'i_female', 'Female',# (%)',
    outcome_in == 'i_m_black', 'Mother Black',# (%)',
    outcome_in == 'i_m_nonwhite', 'Mother Non-white',# (%)',
    outcome_in == 'i_m_hispanic', 'Mother Hispanic',# (%)',
    outcome_in == 'i_m_married', 'Mother Married',# (%)',
    default = 'Estimate'
  )
  # Main plot--first stage 
  fs_event_main_dt = 
    mod_dt[
      type == '2sls-fs' & 
      var_of_interest == TRUE & 
      lhs == outcome_in &
      fixef_num == 'Add Family FEs' & 
      control_num == 'Pesticides and Unemployment' &
      trt == 'all_yield_diff_percentile_gmo' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset)
    ]
  if(nrow(fs_event_main_dt) > 0){
    fs_event_p = 
      ggplot(
        data = fs_event_main_dt,
        aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
      ) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
      geom_ribbon(fill = pink, alpha = 0.5) +
      geom_point(color = pink, size = 2.5) +
      geom_line(linewidth = 0.3, color = pink) +
      scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
      scale_x_continuous(
        name = 'Year', 
        limits = c(1990,2015),
        breaks = seq(1990, 2015, 5), 
        minor_breaks = NULL
      ) 
    if(print) print(fs_event_p)
    ggsave(
      fs_event_p, 
      filename = here(paste0(
        'figures/micro/fs-event/main-',outcome_in,'.jpeg'
      )), 
      width = width_in, height = height_in
    )
  }
  # Main plot--reduced form
  rf_event_main_dt = 
    mod_dt[
      type == 'rf' & 
      var_of_interest == TRUE & 
      lhs == outcome_in &
      fixef_num == 'Add Family FEs' & 
      control_num == 'Pesticides and Unemployment' &
      trt == 'all_yield_diff_percentile_gmo' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset)
    ]
  if(nrow(rf_event_main_dt)>0){
    rf_event_p = 
      ggplot(
        data = rf_event_main_dt,
        aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
      ) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
      geom_ribbon(fill = pink, alpha = 0.5) +
      geom_point(color = pink, size = 2.5) +
      geom_line(linewidth = 0.3, color = pink) +
      scale_y_continuous(name = y_lab, minor_breaks = NULL) +
      scale_x_continuous(
        name = 'Year', 
        limits = c(1990,2015),
        breaks = seq(1990, 2015, 5), 
        minor_breaks = NULL
      ) 
    if(print) print(rf_event_p)
    ggsave(
      rf_event_p, 
      filename = here(paste0(
        'figures/micro/rf-event/main-',outcome_in,'.jpeg'
      )), 
      width = width_in, height = height_in
    )
  }
  # Reduced form with no controls/fe's
  rf_event_nc_dt = 
    mod_dt[
      type == 'rf' & 
      var_of_interest == TRUE & 
      lhs == outcome_in &
      fixef_num == "No Add'l FEs" & 
      control_num == 'None' &
      trt == 'all_yield_diff_percentile_gmo' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA'& 
      is.na(county_subset)
    ]
  if(nrow(rf_event_nc_dt)>0){
    rf_event_nc_p = 
      ggplot(
        data = rf_event_nc_dt,
        aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
      ) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
      geom_ribbon(fill = pink, alpha = 0.5) +
      geom_point(color = pink, size = 2.5) +
      geom_line(linewidth = 0.3, color = pink) +
      scale_y_continuous(name = y_lab, minor_breaks = NULL) +
      scale_x_continuous(
        name = 'Year', 
        limits = c(1990,2015),
        breaks = seq(1990, 2015, 5), 
        minor_breaks = NULL
      ) 
    if(print) print(rf_event_p)
    ggsave(
      rf_event_nc_p, 
      filename = here(paste0(
        'figures/micro/rf-event/no-cntrl-fe-',outcome_in,'.jpeg'
      )), 
      width = width_in, height = height_in
    )
  }
  # Now robustness to different definitions of trt 
  fs_event_trt_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Add Family FEs' & 
        control_num == 'Pesticides and Unemployment' &
        trt != 'percentile_gm_acres' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = str_remove(trt_name, 'Attainable Yield, '), 
        fill = str_remove(trt_name, 'Attainable Yield, ')
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_brewer(
      name = 'Attainable Yield Measure', palette = 'Dark2',
      aesthetics = c('color','fill')
    ) 
  if(print) print(fs_event_trt_p)
  ggsave(
    fs_event_trt_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-trt-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  rf_event_trt_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & var_of_interest == TRUE & lhs == outcome_in &
        fixef_num == 'Add Family FEs' & control_num == 'Pesticides and Unemployment' &
        trt != 'percentile_gm_acres' & spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = str_remove(trt_name, 'Attainable Yield, '), 
        fill = str_remove(trt_name, 'Attainable Yield, ')
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_brewer(
      name = 'Attainable Yield Measure', palette = 'Dark2',
      aesthetics = c('color','fill')
    ) 
  if(print) print(rf_event_trt_p)
  ggsave(
    rf_event_trt_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-trt-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  # Now for control and FE robustness
  fs_event_cntrl_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo' &
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = control_num, 
        fill = control_num
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_brewer(
      name = 'Controls', palette = 'Dark2',
      aesthetics = c('color','fill')
    ) +
    facet_grid(rows = vars(fixef_num))
  if(print) print(fs_event_cntrl_p)
  ggsave(
    fs_event_cntrl_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-cntrl-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in*2
  )
  rf_event_cntrl_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo' &
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = control_num, 
        fill = control_num
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_brewer(
      name = 'Controls', palette = 'Dark2',
      aesthetics = c('color','fill')
    ) +
    facet_grid(rows = vars(fixef_num))
  if(print) print(rf_event_cntrl_p)
  ggsave(
    rf_event_cntrl_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-cntrl-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in*2
  )
  # Now for County Subset robustness
  fs_event_cntysub_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Add Family FEs' & 
        control_num == 'Pesticides and Unemployment' &
        trt == 'all_yield_diff_percentile_gmo' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' & 
        !is.na(county_subset)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = county_subset, 
        fill = county_subset
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_brewer(
      name = 'County Subset', palette = 'Dark2',
      aesthetics = c('color','fill')
    )
  if(print) print(fs_event_cntysub_p)
  ggsave(
    fs_event_cntysub_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-cntysub-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  rf_event_cntysub_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Add Family FEs' & 
        control_num == 'Pesticides and Unemployment' &
        trt == 'all_yield_diff_percentile_gmo' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' 
        #& !is.na(county_subset)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = ifelse(is.na(county_subset),'Full Sample', county_subset), 
        fill = ifelse(is.na(county_subset),'Full Sample', county_subset)
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_brewer(
      name = 'County Subset', palette = 'Dark2',
      aesthetics = c('color','fill')
    ) 
  if(print) print(rf_event_cntysub_p)
  ggsave(
    rf_event_cntysub_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-cntysub-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  # Now for Heterogeneity by pred bw
  fs_event_het_predbw_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Add Family FEs' & 
        control_num == 'Pesticides and Unemployment' &
        trt == 'all_yield_diff_percentile_gmo' & 
        spatial == 'rural' &
        sample_var == 'pred_q5' & 
        sample != 'Full sample' & 
        is.na(county_subset)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = sample, 
        fill = sample
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      name = 'Predicted BW Quintile',
      aesthetics = c('color','fill')
    ) 
  if(print) print(fs_event_het_predbw_p)
  ggsave(
    fs_event_het_predbw_p, 
    filename = here(paste0(
      'figures/micro/fs-event/het-predbw-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  rf_event_het_predbw_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & lhs == outcome_in &
        fixef_num == 'Add Family FEs' & 
        control_num == 'Pesticides and Unemployment' &
        trt == 'all_yield_diff_percentile_gmo' & 
        spatial == 'rural' &
        sample_var == 'pred_q5' & 
        sample != 'Full sample' & 
        is.na(county_subset)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = sample, 
        fill = sample
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      name = 'Predicted BW Quintile',
      aesthetics = c('color','fill')
    ) 
  if(print) print(rf_event_het_predbw_p)
  ggsave(
    rf_event_het_predbw_p, 
    filename = here(paste0(
      'figures/micro/rf-event/het-predbw-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  return(paste('Done', outcome_in))
}

# Function to plot many outcomes at once --------------------------------------
plot_results_faceted_outcomes = function(mod_dt, print = FALSE, width_in = 6, height_in = 3, pink = '#e64173'){
  # First the main results
  rf_event_main_dt = 
    mod_dt[
      type == 'rf' & 
      lhs != 'dbwt_pred' &
      var_of_interest == TRUE & 
      fixef_num == 'Add Family FEs' & 
      control_num == 'Pesticides and Unemployment' &
      trt == 'all_yield_diff_percentile_gmo' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset )
    ]
  if(nrow(rf_event_main_dt)>0){
    rf_event_p = 
      ggplot(
        data = rf_event_main_dt,
        aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
      ) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
      geom_ribbon(fill = pink, alpha = 0.5) +
      geom_point(color = pink, size = 2.5) +
      geom_line(linewidth = 0.3, color = pink) +
      scale_y_continuous(name = '', minor_breaks = NULL) +
      scale_x_continuous(
        name = 'Year', 
        limits = c(1990,2015),
        breaks = seq(1990, 2015, 5), 
        minor_breaks = NULL
      ) +
      facet_wrap(~lhs_name,ncol = 2, scales = 'free_y') + 
      theme(strip.text = element_text(size = 16))
    if(print) print(rf_event_p)
    ggsave(
      rf_event_p, 
      filename = here(paste0(
        'figures/micro/rf-event/main-all-other-lhs.jpeg'
      )), 
      width = width_in*1.5, height = height_in*2.75
    )
  }
  # Now the demographic outcomes
  rf_event_demog_dt = 
    mod_dt[
      type == 'rf' & 
      lhs %in% c(
        'dbwt_pred','i_m_black','i_m_hispanic','i_m_married','i_m_nonwhite','i_female'
      ) &
      var_of_interest == TRUE & 
      fixef_num == "No Add'l FEs" & 
      control_num == 'None' &
      trt == 'all_yield_diff_percentile_gmo' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset)
    ]
  if(nrow(rf_event_demog_dt)>0){
    rf_event_demog_p = 
      ggplot(
        data = rf_event_demog_dt,
        aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
      ) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
      geom_ribbon(fill = pink, alpha = 0.5) +
      geom_point(color = pink, size = 2.5) +
      geom_line(linewidth = 0.3, color = pink) +
      scale_y_continuous(name = '', minor_breaks = NULL) +
      scale_x_continuous(
        name = 'Year', 
        limits = c(1990,2015),
        breaks = seq(1990, 2015, 5), 
        minor_breaks = NULL
      ) +
      facet_wrap(~lhs_name,ncol = 2, scales = 'free_y') +
      theme(strip.text = element_text(size = 16))
    if(print) print(rf_event_demog_p)
    ggsave(
      rf_event_demog_p, 
      filename = here(paste0(
        'figures/micro/rf-event/no-cntrl-all-demog.jpeg'
      )), 
      width = width_in*1.5, height = height_in*2
    )
  }
  # Estimating white and non-white separately 
  rf_het_white_dt = 
    mod_dt[
      type == 'rf' & 
      lhs != 'dbwt_pred' &
      var_of_interest == TRUE & 
      fixef_num == 'Add Family FEs' & 
      control_num == 'Pesticides and Unemployment' &
      trt == 'all_yield_diff_percentile_gmo' & 
      spatial == 'rural' &
      sample_var == 'i_m_nonwhite' &
      sample != 'Full sample' & 
      is.na(county_subset)
    ]
  if(nrow(rf_het_white_dt)>0){
    rf_het_white_p = 
      ggplot(
        data = rf_het_white_dt,
        aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h, color = sample, fill = sample)
      ) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
      #geom_ribbon(alpha = 0.5, color = NA) +
      geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
      geom_linerange(position = position_dodge(width = 0.63)) +
      #geom_line(linewidth = 0.3) +
      scale_color_brewer(
        palette = 'Dark2', 
        name = '',
        labels = c('Mother White', 'Mother Non-white'),
        aesthetics = c('fill','color')
      ) +
      scale_y_continuous(name = '', minor_breaks = NULL) +
      scale_x_continuous(
        name = 'Year', 
        limits = c(1990,2015),
        breaks = seq(1990, 2015, 5), 
        minor_breaks = NULL
      ) +
      facet_wrap(~lhs_name,ncol = 2, scales = 'free_y') + 
      theme(
        strip.text = element_text(size = 16),
        legend.position = 'bottom'
      )
    if(print) print(rf_het_white_p)
    ggsave(
      rf_het_white_p, 
      filename = here(paste0(
        'figures/micro/rf-event/het-i_m_nonwhite-all-outcomes.jpeg'
      )), 
      width = width_in*1.5, height = height_in*2.75
    )
  }
}

# Now we can run the functions ------------------------------------------------
theme_set(
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_blank())
)
# Running for all models EXCEPT shift share
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_2sls_ss|est_ols|est_water_rf', 
    negate = TRUE
  )
mod_dt = lapply(mod_paths, extract_event_study_coefs) |> 
  rbindlist(use.names = TRUE, fill = TRUE)
# Running for all outcomes 
lapply(
  unique(mod_dt$lhs),
  plot_reduced_form, 
  mod_dt = mod_dt
)
plot_results_faceted_outcomes(mod_dt)
