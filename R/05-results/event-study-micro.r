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
      mod_2sls[[i]]$model_info$rhs
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
    if (!exists('sample_var', where = mod_dt)) {
      mod_dt[, 'sample_var' := NA_character_]
    }
    if (!exists('sample', where = mod_dt)) {
      mod_dt[, 'sample' := NA_character_]
    }
  }else if(str_detect(mod_path, 'est_2sls_outcome')){
    # Getting first stage coefs from 2SLS model
    mod_dt = lapply(
      1:length(mod),
      fs_coeftable, 
      mod_2sls = mod
    ) |> rbindlist()
    mod_dt[,type:='2sls-fs']
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
    spatial = str_extract(mod_path, '(?<=_spatial-).*(?=_het)'),
    cluster = str_extract(mod_path, '(?<=_cl-).*(?=_glynl)')
  )]
  # Some cleaning of fixef names
  mod_dt[,':='(
    year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.integer(),
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
    )
  )]
  # return the results 
  return(mod_dt)
}

# Function to plot the event study --------------------------------------------
plot_reduced_form = function(
  outcome_in, mod_dt, print = FALSE, width_in = 6, height_in = 3, pink = '#e64173'
){
  # Making outcome labels and scales 
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
  # Main plot--first stage 
  fs_event_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Mother and Father FEs' & control_num == 'Pesticides and Unemployment' &
        trt == 'all_yield_diff_percentile_gmo' & spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'
      ],
      aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    geom_ribbon(fill = pink, alpha = 0.5) +
    geom_point(color = pink, size = 2.5) +
    geom_line(linewidth = 0.3, color = pink) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
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
  # Main plot--reduced form
  rf_event_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Mother and Father FEs' & control_num == 'Pesticides and Unemployment' &
        trt == 'all_yield_diff_percentile_gmo' & spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'
      ],
      aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    geom_ribbon(fill = pink, alpha = 0.5) +
    geom_point(color = pink, size = 2.5) +
    geom_line(linewidth = 0.3, color = pink) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
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
  # Now robustness to different definitions of trt 
  fs_event_trt_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & var_of_interest == TRUE & lhs == outcome_in &
        fixef_num == 'Mother and Father FEs' & control_num == 'Pesticides and Unemployment' &
        trt != 'percentile_gm_acres' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = str_remove(trt_name, 'Attainable Yield, '), 
        fill = str_remove(trt_name, 'Attainable Yield, ')
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
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
        fixef_num == 'Mother and Father FEs' & control_num == 'Pesticides and Unemployment' &
        trt != 'percentile_gm_acres' & spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = str_remove(trt_name, 'Attainable Yield, '), 
        fill = str_remove(trt_name, 'Attainable Yield, ')
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
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
        paste0(sample_var, sample) == 'NANA'
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = control_num, 
        fill = control_num
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
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
        paste0(sample_var, sample) == 'NANA'
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = control_num, 
        fill = control_num
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
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
  # Now for Heterogeneity by pred bw
  fs_event_het_predbw_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & lhs == outcome_in &
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' &
        trt == 'all_yield_diff_percentile_gmo' & 
        spatial == 'rural' &
        sample_var == 'pred_q5' & 
        sample != 'Full sample'
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = sample, 
        fill = sample
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
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
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Unemployment' &
        trt == 'all_yield_diff_percentile_gmo' & 
        spatial == 'rural' &
        sample_var == 'pred_q5' & 
        sample != 'Full sample'
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = sample, 
        fill = sample
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
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


# Now we can run the functions ------------------------------------------------
theme_set(
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_blank())
)
# Running for all models EXCEPT shift share
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_2sls_ss', 
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