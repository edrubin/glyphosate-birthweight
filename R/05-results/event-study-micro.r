# Main results ----------------------------------------------------------------
library(pacman)
p_load(
  here, data.table, fst, fixest, qs,
  stringr, janitor, ggplot2, magrittr
)

# Function to extract data from event study models ----------------------------
extract_event_study_coefs = function(mod_path){
  # Loading the model 
  mod = qread(mod_path)
  # Extracting coefficients and ci 
  mod_dt = 
    cbind(
      data.table(coeftable(mod)),
      data.table(confint(mod))[,.(ci_l = `2.5 %`, ci_h = `97.5 %`)]
    ) |>
    clean_names()
  # Adding sample var and sample if not there
  if (!exists('sample_var', where = mod_dt)) {
    mod_dt[, 'sample_var' := NA]
  }
  if (!exists('sample', where = mod_dt)) {
    mod_dt[, 'sample' := NA]
  }
  # Add reference coefficient 
  mod_dt %<>%
    rbind(
      mod_dt[,.(
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
      str_detect(rhs, 'atrazine') & str_detect(rhs, 'unemployment'), 'Pesticides and Economic',
      str_detect(rhs, 'atrazine'), 'Pesticides',
      str_detect(rhs, 'unemployment'), 'Economic',
      default = 'None'
    ) |> factor(levels = c('None','Pesticides','Economic','Pesticides and Economic')),
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
  )
  ]
  # return the results 
  return(mod_dt)
}

# Running for all models 
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_rf'
  )
mod_dt = lapply(mod_paths, extract_event_study_coefs) |> 
  rbindlist(use.names = TRUE, fill = TRUE)


# Function to plot the event study --------------------------------------------
outcome_in = 'dbwt'
fixef_num_in = 'Mother and Father FEs'
control_num_in = 'Pesticides and Economic'
trt_in = 'all_yield_diff_percentile_gmo'
spatial_in = 'rural'
sample_in = 'NANA'
pink       = '#e64173'
red_pink   = '#e64173'
turquoise  = '#20B2AA'
orange     = '#FFA500'
red        = '#fb6107'
blue       = '#648FFF'
dark_blue  = '#3b3b9a'
green      = '#8bb174'
grey_light = 'grey70'
grey_mid   = 'grey50'
grey_dark  = 'grey20'
purple     = '#6A5ACD'
slate      = '#314f4f'
# Setting height and width 


theme_set(
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_blank())
)

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
  # Main plot
  rf_event_p = 
    ggplot(
      data = mod_dt[
        var_of_interest == TRUE & lhs == outcome_in &
        fixef_num == 'Mother and Father FEs' & control_num == 'Pesticides and Economic' &
        trt == 'all_yield_diff_percentile_gmo' & spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'
      ],
      aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    geom_ribbon(fill = pink, alpha = 0.5) +
    geom_point(color = pink, size = 2.5) +
    geom_line(size = 0.3, color = pink) +
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
      'figures/micro/rf-event-main-',outcome_in,'.jpeg'
    )), 
    width = width_in, height = height_in
  )
  # Now robustness to different definitions of trt 
  rf_event_trt_p = 
    ggplot(
      data = mod_dt[
        var_of_interest == TRUE & lhs == outcome_in &
        fixef_num == 'Mother and Father FEs' & control_num == 'Pesticides and Economic' &
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
      'figures/micro/rf-event-robust-trt-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  # Now for control and FE robustness
  rf_event_cntrl_p = 
    ggplot(
      data = mod_dt[
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
      'figures/micro/rf-event-robust-cntrl-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in*2
  )
  # Now for Heterogeneity by pred bw
  rf_event_het_predbw_p = 
    ggplot(
      data = mod_dt[
        var_of_interest == TRUE & lhs == outcome_in &
        fixef_num == 'Mother and Father FEs' & 
        control_num == 'Pesticides and Economic' &
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
      'figures/micro/rf-event-het-predbw-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  return(paste('Done', outcome_in))
}


# Running for all outcomes 
lapply(
  unique(mod_dt$lhs),
  plot_reduced_form, 
  mod_dt = mod_dt
)

