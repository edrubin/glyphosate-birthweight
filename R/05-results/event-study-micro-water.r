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
  # Adding info from filename 
  mod_id = str_extract(mod_path, '\\d*\\.qs')
  info_ls = 
    qread(here(
      str_extract(mod_path, '.*micro-new(?=/)'),
      paste0('info_',mod_id)
    ))
  path_fe = str_extract(mod_path, '(?<=_fe).*(?=_spatial)')
  path_fe_special = str_extract(mod_path, '(?<=_fe).{1,2}(?=_\\d{10}\\.qs)')
  mod_dt[,':='(
    mod_id = mod_id,
    spatial = info_ls$spatial_subset, 
    cluster = paste(info_ls$clustering, collapse = ' '),
    county_subset = ifelse(
      is.null(info_ls$county_subset_name), 
      'All', 
      info_ls$county_subset_name
    ),
    #lhs = paste0(info_ls$lhs, collapse = ' '), 
    fixef_num = fcase(
      path_fe_special == '0' , 
        'Family demographics, county, and year by month',
      path_fe_special == '1' , 
        'Family demographics, county, year, and month',
      path_fe_special == '2a', 
        'Family demographics, county, year by Census Region, and month by Census Region',
      path_fe_special == '2b', 
        'Family demographics, county, year by month by Census Region',
      path_fe_special == '2c', 
        'Family demographics, county, year by Census Division, and month by Census Division',
      path_fe_special == '2d', 
        'Family demographics, county, year by month by Census Division',
      path_fe_special == '3a', 
        'Family demographics, county, year by Farm Region, and month by Farm Region',
      path_fe_special == '3b', 
        'Family demographics, county, year by month by Farm Region',
      path_fe_special == '4a', 
        'Family demographics, county, year by state, and month by state',
      path_fe_special == '4b', 
        'Family demographics, county, year by month by state',
      path_fe_special == '5a', 
        'Family demographics, county, year by month by Census Region and month by Ag District',
      path_fe_special == '5b', 
        'Family demographics, county, year by month by Farm Region and month by Ag District',
      path_fe_special == '6a', 
        'Family demographics, county, year by Ag District and month by Ag District',
      path_fe_special == '6b', 
        'Family demographics, county, year by month by Ag District', 
      path_fe == '-yearmonth-fipsres-fipsocc-dem-dad_dem-fe_dad-fe', 
        'Family demographics, county, and year by month',
      str_detect(path_fe, '-yearmonth-fipsres-fipsocc-.*dad-fe'),
        'Family demographics, county, and year by month',
      path_fe == '-yearmonth-fipsres-fipsocc--', 
        'County and year by month'
    ) |> factor(levels = c(
      'County and year by month',
      'Family demographics, county, and year by month',
      'Family demographics, county, year, and month',
      'Family demographics, county, year by Census Region, and month by Census Region',
      'Family demographics, county, year by month by Census Region',
      'Family demographics, county, year by Census Division, and month by Census Division',
      'Family demographics, county, year by month by Census Division',
      'Family demographics, county, year by Farm Region, and month by Farm Region',
      'Family demographics, county, year by month by Farm Region',
      'Family demographics, county, year by state, and month by state',
      'Family demographics, county, year by month by state',
      'Family demographics, county, year by month by Census Region and month by Ag District',
      'Family demographics, county, year by month by Farm Region and month by Ag District',
      'Family demographics, county, year by Ag District and month by Ag District',
      'Family demographics, county, year by month by Ag District'
    ))
  )]
  # Cleaning other things: Year, Controls, trt variable (instrument) 
  mod_dt[,':='(
    year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.integer(),
    control_num = fcase(
      str_detect(rhs, 'acres_km2') & 
        str_detect(rhs, 'atrazine'), 
        'Acres and all others',
      str_detect(rhs, 'acres_km2'), 
        'Acres',
      str_detect(rhs, 'atrazine') & 
        str_detect(rhs, 'unempl') &  
        str_detect(rhs, 'empl') &  
        str_detect(rhs, 'inc_per_cap') &  
        str_detect(rhs, 'pop_all') &  
        str_detect(rhs, 'shr_age') &  
        str_detect(rhs, 'shr_race') &  
        str_detect(rhs, 'p_commercial_km2'), 
        'All',
      str_detect(rhs, 'p_commercial_km2'), 
        'Fertilizers',
      str_detect(rhs, 'shr_age') &  
        str_detect(rhs, 'shr_race'), 
        'Age and Race Shares',
      str_detect(rhs, 'shr_age') , 
        'Age Shares', 
      #str_detect(rhs, 'unempl') &  
      str_detect(rhs, 'farm_empl') &  
        str_detect(rhs, 'inc_per_cap') &  
        str_detect(rhs, 'pop_all'), 
        'Employment, Income, and Population', 
      #str_detect(rhs, 'unempl') &  
      str_detect(rhs, 'farm_empl') &  
        str_detect(rhs, 'inc_per_cap'),
        'Employment and Income', 
      str_detect(rhs, 'unempl') &  
        str_detect(rhs, 'farm_empl'),
        'Employment',
      str_detect(rhs, 'atrazine') & 
        str_detect(rhs, 'unempl_rate'), 
        'Pesticides and Unemployment',
      str_detect(rhs, 'atrazine'), 'Pesticides',
      str_detect(rhs, 'unempl_rate'), 'Unemployment',
      default = 'None'
    ) |> factor(levels = c(
      'None',
      'Pesticides and Unemployment',
      'Employment',
      'Employment and Income' , 
      'Employment, Income, and Population', 
      'Age Shares', 
      'Age and Race Shares',
      'Fertilizers',
      'Acres',
      'All', 
      'Acres and all others'
    )),
    trt = 'all_yield_diff_percentile_gmo_max'
  )]
  mod_dt[,':='(
    var_of_interest = str_detect(coefficient, trt),
    trt_name = fcase(
      trt == 'all_yield_diff_gmo_50_0', 'Attainable Yield, GM Avg Split at Median',
      trt == 'all_yield_diff_gmo_max_50_0', 'Attainable Yield, GM Max Split at Median',
      trt == 'all_yield_diff_percentile_gmo', 'Attainable Yield, GM Avg Percentile',
      trt == 'all_yield_diff_percentile_gmo_max', 'Attainable Yield, GM Max Percentile',
      trt == 'e100m_yield_diff_percentile_gmo', 'Attainable Yield, GM Avg Percentile, Eastern US',
      trt == 'all_yield_diff_gmo_max_50_50', 'Attainable Yield, GM Max Top vs Bottom Quartile',
      trt == 'percentile_gm_acres','1990-1995 GM Acreage Percentile',
      trt == 'percentile_gm_acres_pct_cnty', '1990-1995 GM Acreage Percentile',
      trt == 'percentile_gm_yield_max', '1990-1995 GM Max Yield Percentile',
      trt == 'percentile_gm_acres_pct_cnty_e100m', '1990-1995 GM Acreage Percentile, Eastern US',
      trt == 'percentile_gm_yield_max_e100m', '1990-1995 GM Max Yield Percentile, Eastern US',
      trt == 'e100m_yield_diff_percentile_gmo_max', 'Attainable Yield, GM Max Percentile, Eastern US'
    ),
    county_subset = fcase(
      county_subset == 'mw-ne', 'Midwest and Northeast',
      county_subset == 'south', 'South',
      county_subset == 'south-nofl', 'South, no FL', 
      county_subset == 'agshr95', 'Censor Ag Share, 95th percentile', 
      county_subset == 'agshr90', 'Censor Ag Share, 90th percentile', 
      county_subset == 'agshr75', 'Censor Ag Share, 75th percentile'
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
      lhs == 'i_m_married', 'Pr(Mother Married)',
      lhs == 'i_m_college', 'Pr(Mother College)',
      lhs == 'i_m_hs', 'Pr(Mother HS)',
      lhs == 'mage', "Mother's Age",
      lhs == 'index', 'Health Index'
    ) 
  )]
  # Some cleaning water specific stuff
  mod_dt[,':='(
    soil = str_detect(mod_path, '(?<=est_water_rf-bins-)soil'),
    dist_bin = str_extract(coefficient, '(?<=_d)\\d{2,3}') |> as.integer(),
    high_kls_ppt = str_detect(coefficient, 'high_kls')
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
          control_num == 'All' &
          !is.na(dist_bin) & 
          sample == 'Full sample'
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
          control_num == 'All' & 
          var_of_interest == TRUE & 
          !is.na(dist_bin) & 
          sample == 'Full sample'
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
      scale_color_viridis_d(
        option ='magma', 
        begin = 0.2, end = 0.9,
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
    list.files(here('data/results/micro-new'), full.names = TRUE),
    'est_water_rf-bins'
  ) |>
  str_subset('gestation-index')
mod_dt = lapply(mod_paths, extract_event_study_coefs) |> rbindlist()
lapply(
  unique(mod_dt$lhs), 
  plot_event_study_water, 
  mod_dt = mod_dt
)
