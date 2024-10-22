# Main results ----------------------------------------------------------------
library(pacman)
p_load(
  here, data.table, fst, fixest, qs,
  stringr, janitor, ggplot2, magrittr
)

# Function to extract first stage from 2SLS models ----------------------------
fs_coeftable = function(i, mod_2sls){
  if(class(mod_2sls) == 'fixest_multi'){
    mod_in = mod_2sls[[i]]
  }else if(class(mod_2sls) == 'fixest'){
    mod_in = mod_2sls
  }else{
    stop("can't figure out what type of fixest object this is.")
  }
  # First getting coefs and confint 
  mod_dt_raw = 
    cbind(
      data.table(
        coeftable(mod_in$iv_first_stage$glyph_km2), 
        keep.rownames = TRUE
      ),
      data.table(
        confint(mod_in$iv_first_stage$glyph_km2)
      )[,.(ci_l = `2.5 %`, ci_h = `97.5 %`)]
    ) |>
    clean_names()
  # Now adding the model info 
  mod_dt = mod_dt_raw[,.(
    id = i, 
    fixef = mod_in$model_info$fixef, 
    lhs = mod_in$model_info$lhs, 
    rhs = paste0(
      mod_in$iv_inst_names, " + ",
      #mod_in$model_info$rhs
      as.character(mod_in$fml_all$linear)[3]
    ),
    sample_var = ifelse(
      is.null(mod_in$model_info$sample), 
      NA_character_,
      mod_in$model_info$sample$var
    ), 
    sample = ifelse(
      is.null(mod_in$model_info$sample), 
      NA_character_,
      mod_in$model_info$sample$value
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
  if(class(mod) == 'fixest_multi'){
    nmod = length(mod)
    mod_1 = mod[[1]]
  }else if(class(mod) == 'fixest'){
    nmod = 1
    mod_1 = mod
  }
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
      mod_dt[, sample := NA_character_]
    }
    if (!exists('sample_var', where = mod_dt)) {
      mod_dt[, sample_var := NA_character_]
    }
    if (!exists('rhs', where = mod_dt)) {
      mod_dt[, rhs := as.character(mod_1$fml_all$linear)[3]]
    }
    if (!exists('fixef', where = mod_dt)) {
      mod_dt[, fixef := as.character(mod_1$fml_all$fixef)[2]]
    }
    if (!exists('lhs', where = mod_dt)) {
      mod_dt[, lhs := as.character(mod_1$fml_all$linear)[2]]
    }
    if (!exists('id', where = mod_dt) & nmod == 1) {
      mod_dt[, id := 1]
    }
  }else if(str_detect(mod_path, 'est_2sls_outcome')){
    # Getting first stage coefs from 2SLS model
    mod_dt = lapply(
      1:nmod,
      fs_coeftable, 
      mod_2sls = mod
    ) |> rbindlist()
    mod_dt[,type:='2sls-fs']
    if (!exists('rhs', where = mod_dt)) {
      mod_dt[, rhs := as.character(mod_1$fml_all$linear)[3]]
    }
    if (!exists('fixef', where = mod_dt)) {
      mod_dt[, fixef := as.character(mod_1$fml_all$fixef)[2]]
    }
    if (!exists('lhs', where = mod_dt)) {
      mod_dt[, lhs := as.character(mod_1$fml_all$linear)[2]]
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
    trt = str_extract(rhs, '(?<=i\\(year, ).*(?=, ref = 1995\\))')
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
  # Save the results 
  path = 'data/results/mod-dt'
  dir.create(here(path))
  write.fst(
    mod_dt, 
    here(path,paste0(str_extract(mod_path, '(?<=micro-new/).*(?=\\.qs)'),'.fst'))
  )
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
    outcome_in == 'i_m_college', 'Mother College',
    outcome_in == 'i_m_hs', 'Mother HS',
    outcome_in == 'mage', "Mother's Age",
    outcome_in == 'index', 'Health Index',
    default = 'Estimate'
  )
  # Main plot--first stage 
  fs_event_main_dt = 
    mod_dt[
      type == '2sls-fs' & 
      var_of_interest == TRUE & 
      lhs == outcome_in &
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'All' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset) & 
      mod_id == '1726169143.qs'
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
  fs_event_main_policy_dt = 
    mod_dt[
      type == '2sls-fs' & 
      var_of_interest == TRUE & 
      lhs == outcome_in &
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'None' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset) & 
      mod_id == '1726169143.qs'
    ]
  if(nrow(fs_event_main_policy_dt) > 0){
    fs_event_policy_p = 
      ggplot(
        data = fs_event_main_policy_dt,
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
    if(print) print(fs_event_policy_p)
    ggsave(
      fs_event_policy_p, 
      filename = here(paste0(
        'figures/micro/fs-event/main-policy-',outcome_in,'.jpeg'
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
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'All' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset) & 
      mod_id == '1726169143.qs'
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
  rf_event_main_policy_dt = 
    mod_dt[
      type == 'rf' & 
      var_of_interest == TRUE & 
      lhs == outcome_in &
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'None' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset) & 
      mod_id == '1726169143.qs'
    ]
  if(nrow(rf_event_main_policy_dt)>0){
    rf_event_policy_p = 
      ggplot(
        data = rf_event_main_policy_dt,
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
    if(print) print(rf_event_policy_p)
    ggsave(
      rf_event_policy_p, 
      filename = here(paste0(
        'figures/micro/rf-event/main-policy-',outcome_in,'.jpeg'
      )), 
      width = width_in, height = height_in
    )
  }
  # Reduced form with no controls/fe's: We don't estimate w/o family FEs anymore
  # rf_event_nc_dt = 
  #   mod_dt[
  #     type == 'rf' & 
  #     var_of_interest == TRUE & 
  #     lhs == outcome_in &
  #     fixef_num == 'Family demographics, county, and year by month' & 
  #     control_num == 'None' &
  #     trt == 'all_yield_diff_percentile_gmo_max' & 
  #     spatial == 'rural' &
  #     paste0(sample_var, sample) == 'NANA'& 
  #     is.na(county_subset)
  #   ]
  # if(nrow(rf_event_nc_dt)>0){
  #   rf_event_nc_p = 
  #     ggplot(
  #       data = rf_event_nc_dt,
  #       aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h)
  #     ) +
  #     geom_hline(yintercept = 0) +
  #     geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
  #     geom_ribbon(fill = pink, alpha = 0.5) +
  #     geom_point(color = pink, size = 2.5) +
  #     geom_line(linewidth = 0.3, color = pink) +
  #     scale_y_continuous(name = y_lab, minor_breaks = NULL) +
  #     scale_x_continuous(
  #       name = 'Year', 
  #       limits = c(1990,2015),
  #       breaks = seq(1990, 2015, 5), 
  #       minor_breaks = NULL
  #     ) 
  #   if(print) print(rf_event_p)
  #   ggsave(
  #     rf_event_nc_p, 
  #     filename = here(paste0(
  #       'figures/micro/rf-event/no-cntrl-fe-',outcome_in,'.jpeg'
  #     )), 
  #     width = width_in, height = height_in
  #   )
  # }
  # Now robustness to different definitions of trt 
  fs_event_trt_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        str_detect(trt, 'percentile_gm_acres', negate = TRUE) & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset) & 
        mod_id != '1728516819.qs' & 
        str_detect(trt, 'e100', negate = TRUE)
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
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.8)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Attainable Yield Measure', 
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
  fs_event_trt_e100m_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        str_detect(trt, 'percentile_gm_acres', negate = TRUE) & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset) & 
        mod_id != '1728516819.qs' & 
        (str_detect(trt, 'e100') | trt == 'all_yield_diff_percentile_gmo_max')
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Attainable Yield Measure', 
      aesthetics = c('color','fill')
    ) 
  if(print) print(fs_event_trt_e100m_p)
  ggsave(
    fs_event_trt_e100m_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-trt-e100m-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  rf_event_trt_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        str_detect(trt, 'percentile_gm_acres', negate = TRUE) & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset) & 
        mod_id != '1728516819.qs' & 
        str_detect(trt, 'e100', negate = TRUE)
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
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.8)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Attainable Yield Measure', 
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
  rf_event_trt_e100m_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        str_detect(trt, 'percentile_gm_acres', negate = TRUE) & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset) & 
        mod_id != '1728516819.qs' & 
        (str_detect(trt, 'e100') | trt == 'all_yield_diff_percentile_gmo_max')
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Attainable Yield Measure', 
      aesthetics = c('color','fill')
    ) 
  if(print) print(rf_event_trt_e100m_p)
  ggsave(
    rf_event_trt_e100m_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-trt-e100m-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  # TRT robustness without any controls (Policy effect)
  fs_event_trt_policy_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'None' &
        str_detect(trt, 'percentile_gm_acres', negate = TRUE) & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset) & 
        mod_id != '1728516819.qs' & 
        str_detect(trt, 'e100', negate = TRUE)
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
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.8)) +
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Attainable Yield Measure', 
      aesthetics = c('color','fill')
    ) 
  if(print) print(fs_event_trt_policy_p)
  ggsave(
    fs_event_trt_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-trt-policy-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  fs_event_trt_e100m_policy_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'None' &
        str_detect(trt, 'percentile_gm_acres', negate = TRUE) & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset) & 
        mod_id != '1728516819.qs' & 
        (str_detect(trt, 'e100') | trt == 'all_yield_diff_percentile_gmo_max')
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Attainable Yield Measure', 
      aesthetics = c('color','fill')
    ) 
  if(print) print(fs_event_trt_e100m_policy_p)
  ggsave(
    fs_event_trt_e100m_policy_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-trt-policy-e100m',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  rf_event_trt_policy_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'None' &
        str_detect(trt, 'percentile_gm_acres', negate = TRUE) & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset) & 
        mod_id != '1728516819.qs' & 
        str_detect(trt, 'e100', negate = TRUE)
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
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.8)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Attainable Yield Measure', 
      aesthetics = c('color','fill')
    ) 
  if(print) print(rf_event_trt_policy_p)
  ggsave(
    rf_event_trt_policy_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-trt-policy-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  rf_event_trt_e100m_policy_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'None' &
        str_detect(trt, 'percentile_gm_acres', negate = TRUE) & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA'& 
        is.na(county_subset) & 
        mod_id != '1728516819.qs' & 
        (str_detect(trt, 'e100') | trt == 'all_yield_diff_percentile_gmo_max')
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Attainable Yield Measure', 
      aesthetics = c('color','fill')
    ) 
  if(print) print(rf_event_trt_e100m_policy_p)
  ggsave(
    rf_event_trt_e100m_policy_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-trt-policy-e100m-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  # Now for control robustness
  fs_event_cntrl_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo_max' &
        fixef_num == 'Family demographics, county, and year by month' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset) & 
        mod_id %in% c('1726169143.qs','1729395724.qs') & 
        control_num %in% c(
          'None', 
          'Employment', 
          'Employment and Income', 
          'Employment, Income, and Population', 
          'Age Shares', 
          'Age and Race Shares'
        )
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Controls', 
      aesthetics = c('color','fill')
    )#+
    #theme(legend.position = 'bottom') +
    #guides(color = guide_legend(ncol = 3)) 
  if(print) print(fs_event_cntrl_p)
  ggsave(
    fs_event_cntrl_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-cntrl-empl-shr-',outcome_in,'.jpeg'
    )), 
    width = width_in*2.25, height = height_in*1.25
  )
  fs_event_cntrl_ag_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo_max' &
        fixef_num == 'Family demographics, county, and year by month' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset) & 
        mod_id %in% c('1726169143.qs','1729395724.qs') & 
        control_num %in% c(
          'None', 
          'Fertilizers', 
          'Acres', 
          'Acres and all others', 
          'Pesticides and Unemployment'
        )
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Controls', 
      aesthetics = c('color','fill')
    )#+
    #theme(legend.position = 'bottom') +
    #guides(color = guide_legend(ncol = 3)) 
  if(print) print(fs_event_cntrl_ag_p)
  ggsave(
    fs_event_cntrl_ag_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-cntrl-ag-',outcome_in,'.jpeg'
    )), 
    width = width_in*2.25, height = height_in*1.25
  )
  rf_event_cntrl_ag_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo_max' &
        fixef_num == 'Family demographics, county, and year by month' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset) & 
        mod_id %in% c('1726169143.qs','1729395724.qs') & 
        control_num %in% c(
          'None', 
          'Fertilizers', 
          'Acres', 
          'Acres and all others', 
          'Pesticides and Unemployment'
        )
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Controls', 
      aesthetics = c('color','fill')
    )#+
    #theme(legend.position = 'bottom') +
    #guides(color = guide_legend(ncol = 3)) 
  if(print) print(rf_event_cntrl_ag_p)
  ggsave(
    rf_event_cntrl_ag_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-cntrl-ag-',outcome_in,'.jpeg'
    )), 
    width = width_in*2.25, height = height_in*1.25
  )
  rf_event_cntrl_empl_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo_max' &
        fixef_num == 'Family demographics, county, and year by month' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset) & 
        mod_id %in% c('1726169143.qs','1729395724.qs') & 
        control_num %in% c(
          'None', 
          'Employment', 
          'Employment and Income', 
          'Employment, Income, and Population', 
          'Age Shares', 
          'Age and Race Shares'
        )
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Controls', 
      aesthetics = c('color','fill')
    )#+
    #theme(legend.position = 'bottom') +
    #guides(color = guide_legend(ncol = 3)) 
  if(print) print(rf_event_cntrl_empl_p)
  ggsave(
    rf_event_cntrl_empl_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-cntrl-empl-shr-',outcome_in,'.jpeg'
    )), 
    width = width_in*2.25, height = height_in*1.25
  )
  # Now for FE robustness
  fs_event_fixef_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo_max' &
        spatial == 'rural' &
        control_num == 'All' & 
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset) &
        mod_id != '1726169143.qs' &
        (str_detect(fixef_num, 'state|Ag District', negate = TRUE) |
          fixef_num == 'Family demographics, county, and year by month')
          #&str_detect(fixef_num, 'and month by', negate = TRUE)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = fixef_num |> 
          str_remove('Family demographics, county, ') |>
          str_remove('^and ') |>
          str_replace(',', '') |>
          str_replace('^y','Y')
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.8)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Fixed Effects', 
      aesthetics = c('color','fill')
    )# +
    #theme(legend.position = 'bottom') +
    #guides(color = guide_legend(ncol = 1)) 
  if(print) print(fs_event_fixef_p)
  ggsave(
    fs_event_fixef_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-fixef-',outcome_in,'.jpeg'
    )), 
    width = width_in*2.5, height = height_in*1.5
  )
  fs_event_fixef2_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo_max' &
        spatial == 'rural' &
        control_num == 'All' & 
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset) &
        mod_id != '1726169143.qs' &
        (str_detect(fixef_num, 'state|Ag District') |
          fixef_num == 'Family demographics, county, and year by month')
          #&str_detect(fixef_num, 'year by month by', negate = TRUE)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = fixef_num|> 
          str_remove('Family demographics, county, ') |>
          str_remove('^and ') |>
          str_replace(',', '') |>
          str_replace('^y','Y')
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.8)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Fixed Effects', 
      aesthetics = c('color','fill')
    )# +
    #theme(legend.position = 'bottom') +
    #guides(color = guide_legend(ncol = 1)) 
  if(print) print(fs_event_fixef2_p)
  ggsave(
    fs_event_fixef2_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-fixef2-',outcome_in,'.jpeg'
    )), 
    width = width_in*2.5, height = height_in*1.5
  )
  rf_event_fixef_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo_max' &
        spatial == 'rural' &
        control_num == 'All' & 
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset) &
        mod_id != '1726169143.qs' &
        (str_detect(fixef_num, 'state|Ag District', negate = TRUE) |
          fixef_num == 'Family demographics, county, and year by month')
          #&str_detect(fixef_num, 'and month by', negate = TRUE)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = fixef_num|> 
          str_remove('Family demographics, county, ') |>
          str_remove('^and ') |>
          str_replace(',', '') |>
          str_replace('^y','Y')
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.8)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Fixed Effects', 
      aesthetics = c('color','fill')
    )# +
    #theme(legend.position = 'bottom') +
    #guides(color = guide_legend(ncol = 1)) 
  if(print) print(rf_event_fixef_p)
  ggsave(
    rf_event_fixef_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-fixef-',outcome_in,'.jpeg'
    )), 
    width = width_in*2.5, height = height_in*1.5
  )
  rf_event_fixef2_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        trt == 'all_yield_diff_percentile_gmo_max' &
        spatial == 'rural' &
        control_num == 'All' & 
        paste0(sample_var, sample) == 'NANA' & 
        is.na(county_subset) &
        mod_id != '1726169143.qs' &
        (str_detect(fixef_num, 'state|Ag District') |
          fixef_num == 'Family demographics, county, and year by month')
          #&str_detect(fixef_num, 'year by month by', negate = TRUE)
      ],
      aes(
        x = year, y = estimate, ymin = ci_l, ymax = ci_h,
        color = fixef_num|> 
          str_remove('Family demographics, county, ') |>
          str_remove('^and ') |>
          str_replace(',', '') |>
          str_replace('^y','Y')
      )
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
    #geom_ribbon(alpha = 0.5, color = NA) +
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    geom_linerange(linewidth = 1, position = position_dodge(width = 0.8)) +
    scale_y_continuous(name = y_lab, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'Fixed Effects', 
      aesthetics = c('color','fill')
    ) #+
    #theme(legend.position = 'bottom') +
    #guides(color = guide_legend(ncol = 1)) 
  if(print) print(rf_event_fixef2_p)
  ggsave(
    rf_event_fixef2_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-fixef2-',outcome_in,'.jpeg'
    )), 
    width = width_in*2.5, height = height_in*1.5
  )
  # Now for County Subset robustness
  fs_event_cntysub_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        trt == 'all_yield_diff_percentile_gmo_max' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' &
        (is.na(county_subset) | str_detect(county_subset,'Censor Ag', negate = TRUE)) &
        mod_id != '1726169143.qs'
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
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'County Subset', 
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
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        trt == 'all_yield_diff_percentile_gmo_max' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' &
        (is.na(county_subset) | str_detect(county_subset,'Censor Ag', negate = TRUE)) &
        mod_id != '1726169143.qs'
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'County Subset', 
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
  # Censoring ag share
  fs_event_agshare_p = 
    ggplot(
      data = mod_dt[
        type == '2sls-fs' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        trt == 'all_yield_diff_percentile_gmo_max' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' &
        (is.na(county_subset) | str_detect(county_subset,'Censor Ag')) &
        mod_id != '1726169143.qs'
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
    scale_y_continuous(name = ~GLY/km^2, minor_breaks = NULL) +
    scale_x_continuous(
      name = 'Year', 
      limits = c(1990,2015),
      breaks = seq(1990, 2015, 5), 
      minor_breaks = NULL
    ) + 
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'County Subset', 
      aesthetics = c('color','fill')
    )
  if(print) print(fs_event_agshare_p)
  ggsave(
    fs_event_agshare_p, 
    filename = here(paste0(
      'figures/micro/fs-event/robust-agshr-',outcome_in,'.jpeg'
    )), 
    width = width_in*1.75, height = height_in
  )
  rf_event_agshare_p = 
    ggplot(
      data = mod_dt[
        type == 'rf' & 
        var_of_interest == TRUE & 
        lhs == outcome_in &
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        trt == 'all_yield_diff_percentile_gmo_max' & 
        spatial == 'rural' &
        paste0(sample_var, sample) == 'NANA' &
        (is.na(county_subset) | str_detect(county_subset,'Censor Ag')) &
        mod_id != '1726169143.qs'
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
    scale_color_viridis_d(
      option = 'magma',
      end = 0.9,
      name = 'County Subset', 
      aesthetics = c('color','fill')
    ) 
  if(print) print(rf_event_agshare_p)
  ggsave(
    rf_event_agshare_p, 
    filename = here(paste0(
      'figures/micro/rf-event/robust-agshr-',outcome_in,'.jpeg'
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
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        trt == 'all_yield_diff_percentile_gmo_max' & 
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
      option = 'magma',
      end = 0.9,
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
        fixef_num == 'Family demographics, county, and year by month' & 
        control_num == 'All' &
        trt == 'all_yield_diff_percentile_gmo_max' & 
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
      option = 'magma',
      end = 0.9,
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
  # Now for both rural and non-rural residences
  fs_res_dt = mod_dt[
    type == '2sls-fs' & 
    var_of_interest == TRUE & 
    lhs == outcome_in &
    fixef_num == 'Family demographics, county, and year by month' & 
    control_num == 'All' &
    trt == 'all_yield_diff_percentile_gmo_max' & 
    spatial %in% c('urban res; urban occ','rural res; rural occ')
  ]
  if(nrow(fs_res_dt) > 0){
    fs_event_residence_p = 
      ggplot(
        data = fs_res_dt,
        aes(
          x = year, y = estimate, ymin = ci_l, ymax = ci_h,
          color = factor(spatial, levels = c('urban res; urban occ','rural res; rural occ')), 
          fill = factor(spatial, levels = c('urban res; urban occ','rural res; rural occ'))
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
        option = 'magma',
        begin = 0.2, end = 0.9,
        name = 'Spatial Subset', 
        labels = c('Non-rural','Rural'),
        aesthetics = c('color','fill')
      )
    if(print) print(fs_event_residence_p)
    ggsave(
      fs_event_residence_p, 
      filename = here(paste0(
        'figures/micro/fs-event/robust-residence-',outcome_in,'.jpeg'
      )), 
      width = width_in*1.25, height = height_in
    )
  }
  rf_res_dt = 
    mod_dt[
      type == 'rf' & 
      var_of_interest == TRUE & 
      lhs == outcome_in &
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'All' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
      sample_var == 'rural_res' & 
      sample != 'Full sample'
    ]
  if(nrow(rf_res_dt) > 0){
    rf_event_residence_p = 
      ggplot(
        data = rf_res_dt,
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
        option = 'magma',
        begin = 0.2, end = 0.9,
        name = 'Spatial Subset', 
        labels = c('Non-rural','Rural'),
        aesthetics = c('color','fill')
      ) 
    if(print) print(rf_event_residence_p)
    ggsave(
      rf_event_residence_p, 
      filename = here(paste0(
        'figures/micro/rf-event/robust-residence-',outcome_in,'.jpeg'
      )), 
      width = width_in*1.25, height = height_in
    )
  }
  return(paste('Done', outcome_in))
}

# Function to plot many outcomes at once --------------------------------------
plot_results_faceted_outcomes = function(mod_dt, print = FALSE, width_in = 6, height_in = 3, pink = '#e64173'){
  # First the main results
  rf_event_main_dt = 
    mod_dt[
      type == 'rf' & 
      lhs %in% c('dbwt','gestation','index','i_lbw','i_vlbw','i_preterm') &
      var_of_interest == TRUE & 
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'All' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset ) & 
      mod_id == '1728516819.qs'
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
  # First the main results
  rf_event_main_policy_dt = 
    mod_dt[
      type == 'rf' & 
      lhs %in% c('dbwt','gestation','index','i_lbw','i_vlbw','i_preterm') &
      var_of_interest == TRUE & 
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'None' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
      spatial == 'rural' &
      paste0(sample_var, sample) == 'NANA' & 
      is.na(county_subset ) & 
      mod_id == '1728516819.qs'
    ]
  if(nrow(rf_event_main_policy_dt)>0){
    rf_event_policy_p = 
      ggplot(
        data = rf_event_main_policy_dt,
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
    if(print) print(rf_event_policy_p)
    ggsave(
      rf_event_policy_p, 
      filename = here(paste0(
        'figures/micro/rf-event/main-policy-all-other-lhs.jpeg'
      )), 
      width = width_in*1.5, height = height_in*2.75
    )
  }
  # Now the demographic outcomes
  rf_event_demog_dt = 
    mod_dt[
      type == 'rf' & 
      lhs %in% c(
        'dbwt_pred','i_m_black','i_m_hispanic','i_m_married','i_m_nonwhite', 
        'i_m_college','i_m_hs','mage'
      ) &
      var_of_interest == TRUE & 
      fixef_num == "Family demographics, county, and year by month" & 
      control_num == 'All' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
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
        'figures/micro/rf-event/all-cntrl-all-demog.jpeg'
      )), 
      width = width_in*1.5, height = height_in*2.5
    )
  }
  # Estimating white and non-white separately 
  rf_het_white_dt = 
    mod_dt[
      type == 'rf' & 
      !(lhs %in% c('dbwt_pred','dbwt_pctl_pre','any_anomaly','c_section')) &
      var_of_interest == TRUE & 
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'All' & #'Pesticides and Unemployment' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
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
      scale_color_viridis_d(
        option = 'magma',
        begin = 0.2, end = 0.9,
        name = '',
        aesthetics = c('fill','color'),
        labels = c('Mother White', 'Mother Non-white')
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
  rf_het_hs_dt = 
    mod_dt[
      type == 'rf' & 
      !(lhs %in% c('dbwt_pred','dbwt_pctl_pre','any_anomaly','c_section')) &
      var_of_interest == TRUE & 
      fixef_num == 'Family demographics, county, and year by month' & 
      control_num == 'All' & #'Pesticides and Unemployment' &
      trt == 'all_yield_diff_percentile_gmo_max' & 
      spatial == 'rural' &
      sample_var == 'i_m_hs' &
      sample != 'Full sample' & 
      is.na(county_subset)
    ]
  if(nrow(rf_het_hs_dt)>0){
    rf_het_hs_p = 
      ggplot(
        data = rf_het_hs_dt,
        aes(x = year, y = estimate, ymin = ci_l, ymax = ci_h, color = sample, fill = sample)
      ) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 1995.5, col = 'black', linewidth = 1, alpha = 1, linetype = 'dashed') +
      #geom_ribbon(alpha = 0.5, color = NA) +
      geom_point(size = 2.5, position = position_dodge(width = 0.63)) +
      geom_linerange(position = position_dodge(width = 0.63)) +
      #geom_line(linewidth = 0.3) +
      scale_color_viridis_d(
        option = 'magma',
        begin = 0.2, end = 0.9,
        name = '',
        labels = c('No HS degree', 'HS degree'),
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
    if(print) print(rf_het_hs_p)
    ggsave(
      rf_het_hs_p, 
      filename = here(paste0(
        'figures/micro/rf-event/het-i_m_hs-all-outcomes.jpeg'
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
# List of all RF/2SLS first stage models available
mod_paths = 
  str_subset(
    list.files(here('data/results/micro-new')),
    'est_2sls_ss|est_ols|est_water_rf|info|est_did', 
    negate = TRUE
  )
# Getting list of already-run models 
mods_run = 
  list.files(here('data/results/mod-dt')) |>
  str_replace('fst','qs')
mods_to_run = 
  mod_paths |>
  setdiff(mods_run)
# Running cleaning function for those models 
lapply(
  here('data/results/micro-new', mods_to_run), 
  extract_event_study_coefs
)
# Reading the results 
mod_dt = 
  lapply(
    list.files(here('data/results/mod-dt'), full.names = TRUE), 
    read.fst, 
    as.data.table = TRUE
  ) |> 
  rbindlist(use.names = TRUE, fill = TRUE)
# Running for all outcomes 
lapply(
  unique(mod_dt$lhs),
  plot_reduced_form, 
  mod_dt = mod_dt
)
plot_results_faceted_outcomes(mod_dt)
