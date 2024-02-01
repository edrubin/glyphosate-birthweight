
# Notes ------------------------------------------------------------------------
#   Goal:   Run TWFE analysis for urban areas including surface-water exposure.
#   Time:   9 hours


# Setup ------------------------------------------------------------------------
  # Load packages
  pacman::p_load(fastverse, qs, patchwork, fixest, parallel, magrittr, here)
  fastverse_extend(topics = c('ST', 'VI'))
  # theme_set(hrbrthemes::theme_ipsum())


# Load data --------------------------------------------------------------------
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data', 'clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data
  natality_dt = here(
    'data', 'clean', 'natality-micro.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # City-month (predicted) GLY water exposures dataset
  water_dt = here(
    'data-clean', 'city-water-exposure-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # City water information
  city_dt = here(
    'data-clean', 'city-water-dt.fst'
  ) %>% read_fst(
    as.data.table = TRUE,
    columns = c('city_state', 'share_surface_water')
  )
  # Natality data: Predictions
  prediction_dt = here(
    'data-clean', 'natality-micro-rf-noind.fst'
  ) %>% read_fst(
    as.data.table = TRUE,
    columns = c('row', 'dbwt', 'dbwt_pred')
  )


# Add predictions to main natalitiy dataset ------------------------------------
# NOTE: Also adding true birth weight to check the merge
  setorder(prediction_dt, row)
  set(
    x = natality_dt,
    i = prediction_dt[, row],
    j = 'dbwt_pred',
    value = prediction_dt[['dbwt_pred']]
  )
  set(
    x = natality_dt,
    i = prediction_dt[, row],
    j = 'dbwt_check',
    value = prediction_dt[['dbwt']]
  )
  # Check the merge
  if (natality_dt[, fmean(dbwt == dbwt_check)] < 1) {
    stop('Bad merge between true natality and predicted birthweight.')
  } else {
    natality_dt[, dbwt_check := NULL]
  }
  # Drop years prior to 1992
  natality_dt %<>% .[year >= 1992]
  # Drop prediction dataset
  rm(prediction_dt)
  invisible(gc())


# Key datasets -----------------------------------------------------------------
  # Change names for merge
  setnames(comb_cnty_dt, old = 'GEOID', new = 'fips')
  setnames(natality_dt, old = 'fips_occ', new = 'fips')
  natality_dt[, fips_res := NULL]
  # Key datasets
  setkey(natality_dt, fips, year, month)
  setkey(comb_cnty_dt, fips, year)


# Add percentiles --------------------------------------------------------------
# NOTE: Set 'rural' to a constant (1995 value)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips, rural)],
    by = 'fips',
    all.x = FALSE,
    all.y = FALSE
  )
  # Add percentiles for birthweight (based upon pre-1996, 'rural' births)
  ecdf_pre_rural = natality_dt[(
    rural == TRUE & year < 1996
  ), dbwt %>% ecdf()]
  natality_dt[, dbwt_pctl_rural_pre := ecdf_pre_rural(dbwt)]
  # Repeat using predicted birthweights
  ecdf_pred_pre_rural = natality_dt[(
    rural == TRUE & year < 1996
  ), dbwt_pred %>% ecdf()]
  natality_dt[, dbwt_pred_pctl_rural_pre := ecdf_pred_pre_rural(dbwt_pred)]
  # Add quintiles, quartiles, terciles
  natality_dt[, `:=`(
    pred_q5 = cut(
      x = dbwt_pred_pctl_rural_pre,
      breaks = 5,
      labels = 1:5, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q4 = cut(
      x = dbwt_pred_pctl_rural_pre,
      breaks = 4,
      labels = 1:4, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q3 = cut(
      x = dbwt_pred_pctl_rural_pre,
      breaks = 3,
      labels = 1:3, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  )]


# Function: Run TFWE analysis --------------------------------------------------
  est_twfe = function(
    outcome = 'dbwt',
    iv_local = 'local_e100m_yield_diff_percentile_gmo_max',
    iv_surface = 'pred_glyph_in_water',
    month_het = FALSE,
# TODO Update function for the following arguments
    # het_split = NULL,
    # base_fe = c('year', 'fips', 'month'), # TODO Default to year, city_state, month
    # fes = c(0, 3),
    # controls = c(0, 3),
    # clustering = c('year', 'fips'),
    ...
  ) {
    # Define demographic controls
    dem_controls = c(
      'sex', 'mage', 'mrace', 'mhisp', 'meduc', 'mar',
      'birth_facility', 'restatus', 'total_birth_order'
    )
    f_controls = c('fage', 'fhisp', 'frace')
    # Define pesticide controls
    pest_controls = c(
      'alachlor_km2', 'atrazine_km2', 'cyanazine_km2', 'fluazifop_km2',
      'metolachlor_km2','metribuzin_km2','nicosulfuron_km2'
    )
    # Collecting glyphosate variables 
    glyph_vars = c(
      'glyph_km2',
      names(comb_cnty_dt) |> 
        str_subset('high_kls_ppt_growing_glyph') |> 
        str_subset('local', negate = TRUE)
    )
    
    # Merge the datasets with the requested variables
    est_dt = merge(
      x = natality_dt[, c(
        'fips', 'place_fips', 'year', 'month', outcome, dem_controls, f_controls,
        # 'pred_q3', 'pred_q4', 'pred_q5',
        NULL
      ), with = FALSE],
      y = comb_cnty_dt[, c(
        'fips', 'state_fips', 'year', 'rural',
        glyph_vars,
        pest_controls,
        'unemployment_rate'
      ), with = FALSE],
      by = c('fips', 'year'),
      all = FALSE
    ) |> merge(
      y = water_dt,
      by = c('state_fips', 'place_fips', 'year', 'month'),
      all.x = TRUE,
      all.y = FALSE
    ) |> merge(
      y = city_dt,
      by = 'city_state',
      all = FALSE
    )
# NOTE Drops cities for which we have no 'city_state' (no water intake data)

    # Formula: Reduced form (event study), no water transport
    fml_rf_local = paste(
      outcome,
      '~',
      '1',
      paste0('+ i(year,', iv_local, ', ref = 1995)'),
      # ' + sw0(',
      #   paste(dem_controls, collapse = '+'), ',',
      #   paste(c(dem_controls, f_controls), collapse = '+'), ',',
      #   paste(pest_controls, collapse = '+'), ',',
      #   'unemployment_rate',
      # ')',
      '| year + month + city_state'
    ) %>% as.formula()
    # Formula: Reduced form, only water transport (no local exposure)
    fml_rf_surface = paste(
      outcome,
      '~',
      '1',
      '+ ',
      iv_surface,
      # ' + sw0(',
      #   paste(dem_controls, collapse = '+'), ',',
      #   paste(c(dem_controls, f_controls), collapse = '+'), ',',
      #   paste(pest_controls, collapse = '+'), ',',
      #   'unemployment_rate',
      # ')',
      '| year + month + city_state'
    ) %>% as.formula()
    # Formula: Reduced form, both local and water-based exposure
    fml_rf_both = paste(
      outcome,
      '~',
      '1',
      paste0('+ i(year,', iv_local, ', ref = 1995)'),
      '+',
      iv_surface,
      # ' + sw0(',
      #   paste(dem_controls, collapse = '+'), ',',
      #   paste(c(dem_controls, f_controls), collapse = '+'), ',',
      #   paste(pest_controls, collapse = '+'), ',',
      #   'unemployment_rate',
      # ')',
      '| year + month + city_state'
    ) %>% as.formula()
    # Formula: Reduced form for water exposure, controlling for local glyphosate
    fml_rf_control = paste(
      outcome,
      '~',
      iv_surface,
      '+ glyph_km2',
      # ' + sw0(',
      #   paste(dem_controls, collapse = '+'), ',',
      #   paste(c(dem_controls, f_controls), collapse = '+'), ',',
      #   paste(pest_controls, collapse = '+'), ',',
      #   'unemployment_rate',
      # ')',
      '| year + city_state + fips'
    ) %>% as.formula()
    
    # Estimate!
    est_rf_local = feols(
      data = est_dt[year >= 1992],
      fml = fml_rf_local,
      cluster = ~ year + city_state,
      mem.clean = TRUE,
      lean = TRUE,
      fsplit = ~ group
    )
    est_rf_surface = feols(
      data = est_dt[year >= 1992],
      fml = fml_rf_surface,
      cluster = ~ year + city_state,
      mem.clean = TRUE,
      lean = TRUE,
      fsplit = ~ share_surface_water == 1
    )
    est_rf_both = feols(
      data = est_dt[year >= 1992],
      fml = fml_rf_both,
      cluster = ~ year + city_state,
      mem.clean = TRUE,
      lean = TRUE,
      fsplit = ~ share_surface_water == 1
    )
    est_rf_control = feols(
      data = est_dt[year >= 1992],
      fml = fml_rf_control,
      cluster = ~ year + city_state,
      mem.clean = TRUE,
      lean = TRUE,
      fsplit = ~ share_surface_water == 1
    )

    qsave(
      est_rf,
      here(
        'data-clean', 'results', '20220831',
        paste0('est_rf_',rural_file,'_', iv, '.qs')
      ),
      preset = 'fast'
    )
    rm(est_rf); invisible(gc())
    est_rf_water = feols(
      data = est_dt,
      fml = fml_rf_water,
      cluster = ~ year + city_state,
      mem.clean = TRUE,
      lean = TRUE,
      fsplit = ~month
    )
    qsave(
      est_rf_water,
      here(
        'data-clean', 'results', '20220831',
        paste0('est_rf_water_',rural_file,'_', iv, '.qs')
      ),
      preset = 'fast'
    )
    rm(est_rf_water); invisible(gc())
    est_rf_simple = feols(
      data = est_dt,
      fml = fml_rf_simple,
      cluster = ~ year + city_state,
      mem.clean = TRUE,
      lean = TRUE
    )
    qsave(
      est_rf_simple,
      here(
        'data-clean', 'results', '20220831',
        paste0('est_rf_simple_',rural_file,'_', iv, '.qs')
      ),
      preset = 'fast'
    )
    rm(est_rf_simple); invisible(gc())
    est_rf_simple_water = feols(
      data = est_dt,
      fml = fml_rf_simple_water,
      cluster = ~ year + city_state,
      mem.clean = TRUE,
      lean = TRUE
    )
    qsave(
      est_rf_simple_water,
      here(
        'data-clean', 'results', '20220831',
        paste0('est_rf_simple_water_',rural_file,'_', iv, '.qs')
      ),
      preset = 'fast'
    )
    rm(est_rf_simple_water); invisible(gc())
    est_2sls = feols(
      data = est_dt,
      fml = fml_2sls,
      cluster = ~ year + city_state,
      fsplit = ~month,
      mem.clean = TRUE,
      lean = TRUE
    )
    qsave(
      est_2sls,
      here(
        'data-clean', 'results', '20220831',
        paste0('est_2sls_',rural_file,'_', iv, '.qs')
      ),
      preset = 'fast'
    )
    rm(est_2sls); invisible(gc())
    est_2sls_water = feols(
      data = est_dt,
      fml = fml_2sls_water,
      cluster = ~ year + city_state,
      fsplit = ~month,
      mem.clean = TRUE,
      lean = TRUE
    )
    qsave(
      est_2sls_water,
      here(
        'data-clean', 'results', '20220831',
        paste0('est_2sls_water_',rural_file,'_', iv, '.qs')
      ),
      preset = 'fast'
    )
    rm(est_2sls_water); invisible(gc())
    
    # Estimating models with month-of-birth-heterogeneity
    if (month_het == TRUE) {
      # Formula: Reduced form, no water transport
      fml_rf_month = paste(
        outcome,
        '~',
        '1',
        paste0('+ i(I(paste(year,month)),', iv, ", ref = '1995 01')"),
        ' + sw0(',
        paste(dem_controls, collapse = '+'), ',',
        paste(c(dem_controls, f_controls), collapse = '+'), ',',
        paste(pest_controls, collapse = '+'), ',',
        'unemployment_rate',
        ')',
        '| year + month + fips'
      ) %>% as.formula()
      # Formula: reduced form with water transport
      fml_rf_water_month = paste(
        outcome,
        '~',
        '1',
        paste0('+ i(I(paste(year,month)),', iv, ", ref = '1995 01')"),
        ' + sw0(',
        # Upstream/Downstream lags without controls 
        str_subset(iv_vars,"_d.{1,4}$") %>% 
          paste0('i(I(paste(year,month)),', ., ", ref = '1995 01')") %>% 
          paste(collapse = '+'), ',',
        # Upstream/Downstream lags with controls 
        str_subset(iv_vars,"_d.{1,4}$") %>% 
          paste0('i(I(paste(year,month)),', ., ", ref = '1995 01')") %>% 
          paste(collapse = '+'), '+',
        paste(dem_controls, collapse = '+'),
        # paste(c(dem_controls, f_controls), collapse = '+'), ',',
        # paste(pest_controls, collapse = '+'), ',',
        # 'unemployment_rate',
        ')',
        '| year + month + fips'
      ) %>% as.formula()
      # Formula: Reduced form, simple version, no water transport
      fml_rf_simple_month = paste(
        outcome,
        '~',
        '1',
        paste0(' + I(as.numeric(year > 1995)):month:', 'I(as.numeric(', iv, '))'),
        ' + sw0(',
        paste(dem_controls, collapse = '+'), ',',
        paste(c(dem_controls, f_controls), collapse = '+'), ',',
        paste(pest_controls, collapse = '+'), ',',
        'unemployment_rate',
        ')',
        '| year + month + fips'
      ) %>% as.formula()
      # Formula: Reduced form, simple version, water transport
      fml_rf_simple_water_month = paste(
        outcome,
        '~',
        '1',
        paste0('+ I(as.numeric(year > 1995)):month:', 'I(as.numeric(', iv, '))'),
        # Upstream/Downstream lags without controls 
        str_subset(iv_vars,"_d.{1,4}$") %>% 
          paste0('+ I(as.numeric(year > 1995)):month:', 'I(as.numeric(', .,'))') %>% 
          paste(collapse = ' '),
        ' + sw0(',
        paste(dem_controls, collapse = '+'), #',',
        # paste(c(dem_controls, f_controls), collapse = '+'), ',',
        # paste(pest_controls, collapse = '+'), ',',
        # 'unemployment_rate',
        ')',
        '| year + month + fips'
      ) %>% as.formula()
      # Estimate!
      est_rf_month = feols(
        data = est_dt,
        fml = fml_rf_month,
        cluster = ~ year + month + fips,
        mem.clean = TRUE,
        lean = TRUE
      )
      qsave(
        est_rf_month,
        here(
          'data-clean', 'results', '20220831',
          paste0('est_rf_month_',rural_file,'_', iv, '.qs')
        ),
        preset = 'fast'
      )
      rm(est_rf_month); invisible(gc())
# TODO Running into memory issues. Current fix: Split on month (above with 'fsplit').
      # est_rf_water_month = feols(
      #   data = est_dt,
      #   fml = fml_rf_water_month,
      #   cluster = ~ year + month + fips,
      #   mem.clean = TRUE,
      #   lean = TRUE
      # )
      # qsave(
      #   est_rf_water_month,
      #   here(
      #     'data-clean', 'results', '20220831',
      #     paste0('est_rf_water_month_',rural_file,'_', iv, '.qs')
      #   ),
      #   preset = 'fast'
      # )
      # rm(est_rf_water_month); invisible(gc())
      est_rf_simple_month = feols(
        data = est_dt,
        fml = fml_rf_simple_month,
        cluster = ~ year + month + fips,
        mem.clean = TRUE,
        lean = TRUE
      )
      qsave(
        est_rf_simple_month,
        here(
          'data-clean', 'results', '20220831',
          paste0('est_rf_simple_month_',rural_file,'_', iv, '.qs')
        ),
        preset = 'fast'
      )
      rm(est_rf_simple_month); invisible(gc())
      est_rf_simple_water_month = feols(
        data = est_dt,
        fml = fml_rf_simple_water_month,
        cluster = ~ year + month + fips,
        mem.clean = TRUE,
        lean = TRUE
      )
      qsave(
        est_rf_simple_water_month,
        here(
          'data-clean', 'results', '20220831',
          paste0('est_rf_simple_water_month_',rural_file,'_', iv, '.qs')
        ),
        preset = 'fast'
      )
      rm(est_rf_simple_water_month); invisible(gc())
    }
    # Return something
    return('done')
  }


# Run function -----------------------------------------------------------------
  # # Run the function with desired instruments
  # # Eastern GMO
  est_twfe(iv = 'e100m_yield_diff_gmo_50_0', month_het = TRUE)
  est_twfe(iv = 'e100m_yield_diff_gmo_50_10')
  est_twfe(iv = 'e100m_yield_diff_gmo_40_0')
  est_twfe(iv = 'e100m_yield_diff_gmo_60_0')
  # # Eastern Soy
  est_twfe(iv = 'e100m_yield_diff_soy_50_0', month_het = TRUE)  
  est_twfe(iv = 'e100m_yield_diff_soy_50_10')
  est_twfe(iv = 'e100m_yield_diff_soy_40_0')
  est_twfe(iv = 'e100m_yield_diff_soy_60_0')
  # # All GMO
  est_twfe(iv = 'all_yield_diff_gmo_50_0')
  est_twfe(iv = 'all_yield_diff_gmo_50_0')
  # # Olive placebo
  est_twfe(iv = "all_yield_diff_olv_50_0")
  est_twfe(iv = "e100m_yield_diff_olv_50_0")
  # # Include urban counties: Eastern GMO
  # est_twfe(iv = 'e100m_yield_diff_gmo_50_0', rural_only = FALSE)  
  # # Include urban counties: Eastern Soy
  # est_twfe(iv = 'e100m_yield_diff_soy_50_0', rural_only = FALSE)  
  
  
  
