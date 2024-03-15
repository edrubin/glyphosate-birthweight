# Plotting event studies for water results 
library(pacman)
p_load(
  here, data.table, fst, fixest, qs,
  stringr, janitor, ggplot2, magrittr
)

# Function to extract water event study coefs
extract_event_study_coefs = function(mod_path){
  print(paste('starting', mod_path))
  # Loading the model 
  mod = qread(mod_path)
  # Extracting coefficients and ci for reduced form model
  mod_dt = 
    cbind(
      data.table(coeftable(mod)),
      data.table(confint(mod))[,.(ci_l = `2.5 %`, ci_h = `97.5 %`)]
    ) |>
    clean_names()
  # Add reference coefficient 
  mod_dt %<>%
    rbind(
      mod_dt[
        str_detect(coefficient, 'year::')
        ,.(
        id, lhs,
        coefficient = str_replace(
          coefficient, 
          '(?<=year::)\\d{4}(?=:)',
          '1995'
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
    county_subset = str_extract(mod_path, '(?<=_county-)[a-zA-Z-]+(?=_)'),
    fixef = str_extract(mod_path, '(?<=_fe-)\\d+(?=_)'),
    rhs = str_extract(mod_path, '(?<=_controls-)\\d+(?=_)'),
    soil = str_detect(mod_path, '(?<=est_water_rf-bins-)soil')
  )]
  # Some cleaning of fixef names
  mod_dt[,':='(
    year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.integer(),
    trt = str_extract(coefficient, '(?<=year::\\d{4}:).*') |> str_remove('_d\\d{2,3}'),
    fixef_num = fcase(
      fixef == '0', "No Add'l FEs",
      fixef == '1', 'Mother FEs',
      fixef == '2', 'Father FEs',
      fixef == '3', 'Add Family FEs'
    ) |> factor(levels = c("No Add'l FEs",'Mother FEs','Father FEs','Add Family FEs')),
    control_num = fcase(
      rhs == '0', 'None',
      rhs == '1', 'Pesticides',
      rhs == '2', 'Unemployment',
      rhs == '3', 'Pesticides and Unemployment'
    ) |> factor(levels = c('None','Pesticides','Unemployment','Pesticides and Unemployment')),
    dist_bin = str_extract(coefficient, '(?<=_d)\\d{2,3}') |> as.integer()
  )]
  mod_dt[,':='(
    var_of_interest = str_detect(coefficient, trt),
    trt_name = fcase(
      str_detect(trt, 'all_yield_diff_gmo_50_0'), 'Attainable Yield, GM Avg Split at Median',
      str_detect(trt, 'all_yield_diff_percentile_gmo'), 'Attainable Yield, GM Avg Percentile',
      str_detect(trt, 'all_yield_diff_percentile_gmo_max'), 'Attainable Yield, GM Max Percentile',
      str_detect(trt, 'e100m_yield_diff_percentile_gmo'), 'Attainable Yield, GM Avg Percentile, Eastern US',
      str_detect(trt, 'percentile_gm_acres'),'1990-1995 GM Acreage Percentile'
    ),
    high_kls_ppt = str_detect(coefficient, 'high_kls'),
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
  # Cleanup
  rm(mod)
  gc()
  # return the results 
  return(mod_dt)
}

# Plotting the results  
plot_event_study_water = function(outcome_in, mod_dt, print = FALSE, width_in = 6, height_in = 3, pink = '#e64173'){
  # First simple one 
  rf_water_simple = 
      ggplot(
        data = mod_dt[
          lhs == outcome_in & 
          soil == FALSE &
          var_of_interest == TRUE & 
          !is.na(dist_bin)
        ],
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
      facet_wrap(~dist_bin, ncol = 2, scales = 'free_y') + 
      theme(strip.text = element_text(size = 16))
    if(print) print(rf_water_simple)
    ggsave(
      rf_water_simple, 
      filename = here(paste0(
        'figures/micro/rf-event/water-bins-simple-',outcome_in,'.jpeg'
      )), 
      width = width_in*1.5, height = height_in*2.75
    )
  # Now for high vs low erodibility 
  rf_water_soil = 
      ggplot(
        data = mod_dt[
          lhs == outcome_in & 
          soil == TRUE &
          #high_kls_ppt == TRUE &
          var_of_interest == TRUE & 
          !is.na(dist_bin)
        ],
        aes(
          x = year, y = estimate, 
          ymin = ci_l, ymax = ci_h,
          color = high_kls_ppt, fill = high_kls_ppt
        )
      ) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
      #geom_ribbon(alpha = 0.5, color = NA) +
      geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
      #geom_line(linewidth = 0.3) +
      geom_linerange(linewidth = 1, position = position_dodge(width = 0.63)) +
      scale_y_continuous(name = '', minor_breaks = NULL) +
      scale_x_continuous(
        name = 'Year', 
        limits = c(1990,2015),
        breaks = seq(1990, 2015, 5), 
        minor_breaks = NULL
      ) +
      scale_color_brewer(
        palette = 'Dark2', 
        name = 'Erodibility and Rainfall', 
        labels = c('Low','High'), 
        aesthetics = c('color','fill')
      ) +
      facet_wrap(~dist_bin, ncol = 2, scales = 'free_y') + 
      theme(
        strip.text = element_text(size = 16),
        legend.position = 'bottom'
      )
    if(print) print(rf_water_soil)
    ggsave(
      rf_water_soil, 
      filename = here(paste0(
        'figures/micro/rf-event/water-bins-soil-',outcome_in,'.jpeg'
      )), 
      width = width_in*1.5, height = height_in*2.8
    )
}


# Running everything ----------------------------------------------------------
theme_set(
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_blank())
)
# Getting coefs for water models 
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_water_rf-bins'
  )
mod_dt = lapply(mod_paths, extract_event_study_coefs) |> rbindlist()
lapply(
  unique(mod_dt$lhs), 
  plot_event_study_water, 
  mod_dt = mod_dt
)
