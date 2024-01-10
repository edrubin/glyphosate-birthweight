# Notes ------------------------------------------------------------------------
#   Goal:   Run main TWFE analysis with various specifications.
#   Time:   About 4 hours for 8 runs (varies with specification).


# Todo list --------------------------------------------------------------------
#   TODO Add a way to keep sample constant (due to missing demog controls)
#   TODO Add ability to have multiple instruments (and interaction?)
#   TODO Inegrate below code to do spline heterogeneity 
  # # Adding bsplines to data
  # p_load(splines)
  # spline_df = 5
  # spline_cols = paste0('pred_spline_',1:spline_df)
  # natality_dt = 
  #   cbind(
  #     natality_dt,
  #     bs(
  #       x = natality_dt$dbwt_pred_pctl_pre, 
  #       df = spline_df,
  #       intercept = TRUE
  #     ) |> 
  #     data.table() |>
  #     setnames(new = spline_cols)
  #   )
  # # 2SLS: Event study instruments with splines
  # tsls_spline_mod = feols(
  #   data = natality_dt, 
  #   cluster = ~year + state_fips,
  #   fml = dbwt ~ 
  #     # Controls  
  #     alachlor_km2 + atrazine_km2 + cyanazine_km2 + fluazifop_km2 + 
  #     metolachlor_km2 + metribuzin_km2 + nicosulfuron_km2 + 
  #     unemployment_rate +
  #     # Fixed effects have to be interacted splines as controls
  #     .[paste0('i(year_month, ',spline_cols,')')] + 
  #     .[paste0('i(fips_res, ',spline_cols,')')] + 
  #     .[paste0('i(fips_occ, ',spline_cols,')')]
  #     | # IV
  #     .[paste0('I(glyph_km2*',spline_cols,')')] ~ 
  #       .[paste0(
  #         'i(year, I(all_yield_diff_percentile_gmo*',spline_cols,'), ref = 1995)'
  #       )]
  # )

# Setup ------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    fastverse, qs, patchwork,
    fixest, splines, parallel, magrittr, here
  )
  fastverse_extend(topics = c('ST', 'DT', 'VI'))


# Load data --------------------------------------------------------------------
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data-clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Pre-period county-level crop yield percentiles
  pctl_dt = here(
    'data', 'crops', 'crop-acre-percentile-90-95.fst'
  ) |> read_fst(as.data.table = TRUE)
  # Shift-share data
  share_dt = here(
    'data', 'glyph-nat-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data: Raw
  natality_dt = here(
    'data-clean', 'natality-micro.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data: Predictions
  prediction_dt = here(
    'data-clean', 'natality-micro-rf-noind.fst'
  ) %>% read_fst(
    as.data.table = TRUE,
    columns = c('row', 'dbwt', 'dbwt_pred')
  )
  # Add predictions to the full natality dataset (using row ID)
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
  # Drop years prior (if desired)
# ADJUST years if desired
  # natality_dt %<>% .[year >= 1992]
  # Drop prediction dataset
  rm(prediction_dt)
  invisible(gc())
  # Convert ages to numeric (harmonizes; can still use as factors later)
  natality_dt[, `:=`(
    mage = mage %>% as.integer(),
    fage = fage %>% as.integer()
  )]


# Collapse birth anomalies -----------------------------------------------------
  # Create an indicator for any birth anomalies (without and with na.rm = TRUE)
  natality_dt[, `:=`(
    any_anomaly = 1 * (do.call(pmin, .SD) == 1),
    any_anomaly_nona = 1 * (do.call(function(...) pmin(..., na.rm = TRUE), .SD) == 1)
  ), .SDcols = febrile:baby_other_cong]
  # Drop individual birth anomalies 
  anomalies = natality_dt |> fselect(febrile:baby_other_cong) |> names()
  natality_dt[, (anomalies) := NULL]


# Merge crop datasets ----------------------------------------------------------
  # Add 90-95 percentiles to the county crop panel; then shift-share variables
  comb_cnty_dt %<>% merge(
    y = pctl_dt,
    by = 'GEOID',
    all = FALSE
  ) %>% merge(
    share_dt,
    by = c('GEOID', 'year'),
    all = TRUE
  )


# Define 'rural' counties ------------------------------------------------------
# NOTE: Within county, rural is constant across years (defined in a Census year)
# NOTE: Because births have two counties (occurence and residence), we define
#       two measures of 'rural births'
  # Change 'GEOID' to 'fips'
  setnames(comb_cnty_dt, old = 'GEOID', new = 'fips')
  # Key the county dataset
  setkey(comb_cnty_dt, fips, year)
  # Define 'rural' using residence
  setkey(natality_dt, fips_res, year, month)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips, rural_res = rural)],
    by.x = 'fips_res',
    by.y = 'fips',
    all.x = TRUE,
    all.y = FALSE
  )
  # Define 'rural' using birth occurence
  setkey(natality_dt, fips_occ, year, month)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips, rural_occ = rural)],
    by.x = 'fips_occ',
    by.y = 'fips',
    all.x = TRUE,
    all.y = FALSE
  )
  # Define 'groups' based upon the two rural statuses
  lvls = c('uu', 'rr', 'ru', 'ur')
  lbls = c(
    'urban res; urban occ', 'rural res; rural occ',
    'rural res; urban occ', 'urban res; rural occ'
  )
  natality_dt[, `:=`(
    rural_grp = fcase(
      rural_res == FALSE & rural_occ == FALSE, factor('uu', levels = lvls, labels = lbls),
      rural_res == TRUE & rural_occ == TRUE, factor('rr', levels = lvls, labels = lbls),
      rural_res == TRUE & rural_occ == FALSE, factor('ru', levels = lvls, labels = lbls),
      rural_res == FALSE & rural_occ == TRUE, factor('ur', levels = lvls, labels = lbls),
      default = NA
    )
  )]


# Add birthweight percentiles --------------------------------------------------
  # Add percentiles for birthweight (based upon pre-1996 births)
  ecdf_pre = natality_dt[year < 1996, dbwt %>% ecdf()]
  natality_dt[, dbwt_pctl_pre := ecdf_pre(dbwt)]
  # Repeat using predicted birthweights
  ecdf_pred_pre = natality_dt[year < 1996, dbwt_pred %>% ecdf()]
  natality_dt[, dbwt_pred_pctl_pre := ecdf_pred_pre(dbwt_pred)]
  # Add quintiles, quartiles, terciles, deciles
  natality_dt[, `:=`(
    pred_q5 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 5,
      labels = 1:5, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q4 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 4,
      labels = 1:4, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q3 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 3,
      labels = 1:3, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q10 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 10,
      labels = 1:10, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
    pred_q14 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = c(0,0.01,0.05,seq(0.1,0.9,0.1),0.95, 0.99,1),
      labels = 1:14, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  )]


# Add additional variables -----------------------------------------------------
  # Add indicators for low birthweight (<2500) and very low birthweight (<1500)
  natality_dt[, `:=`(
    i_lbw = 1 * (dbwt < 2500),
    i_vlbw = 1 * (dbwt < 1500)
  )]
  # Add month of sample
  natality_dt[, year_month := paste0(year, '-', month)]
  # Add whether glyph_km2 is above median 
  natality_dt[, above_median_glyph_km2 := glyph_km2 > median(glyph_km2)]
  # Same for main instrument 
  # TODO: do this for all of treatment variables (?)
  natality_dt[,':='( 
    above_median_all_yield_diff_percentile_gmo = 
      all_yield_diff_percentile_gmo > median(all_yield_diff_percentile_gmo),
    all_yield_diff_percentile_gmo_sq = all_yield_diff_percentile_gmo_sq^2,
    above_median_glyphosate_nat_100km = 
      glyphosate_nat_100km > median(glyphosate_nat_100km),
    glyphosate_nat_100km_sq = glyphosate_nat_100km^2
  )]


# Function: Run TFWE analysis --------------------------------------------------
  est_twfe = function(
    outcomes = c(
      'dbwt', 'gestation',
      'dbwt_pctl_pre', 
      'i_lbw', 'i_vlbw',
      'c_section', 'any_anomaly'
    ),
    iv = 'all_yield_diff_percentile_gmo',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips'),
    gly_nonlinear = NULL,
    iv_nonlinear = FALSE
    ...
  ) {

    # Define outcome variables
    outcome_vars = outcomes
    # Child and mother demographic controls (fixed effects)
    dem_fes = c(
      'sex', 'mage', 'mrace', 'mhisp', 'meduc', 'mar',
      'birth_facility', 'restatus', 'total_birth_order'
    )
    # Father controls (fixed effects)
    dad_fes = c('fage', 'fhisp', 'frace')
    # Define pesticide controls
    pest_controls = c(
      'alachlor_km2', 'atrazine_km2', 'cyanazine_km2', 'fluazifop_km2',
      'metolachlor_km2','metribuzin_km2','nicosulfuron_km2'
    )
    # Economic controls (currently just unemployment rate)
    econ_controls = c(
      'unemployment_rate'
    )
    # Collecting glyphosate variables
    glyph_vars = c(
      'glyph_km2',
      names(comb_cnty_dt) |>
        str_subset('high_kls_ppt_growing_glyph') |>
        str_subset('local', negate = TRUE),
      fifelse(gly_nonlinear == 'median', 'above_median_glyph_km2', NA)
    ) |> 
    na.omit() |> 
    as.vector()
    # iv_vars = comb_cnty_dt %>% names() %>% str_subset(iv)
    iv_vars = 
      c(
        iv, 
        iv_shift,
        fifelse(
          iv_nonlinear == TRUE & gly_nonlinear == 'median', 
          paste0('above_median_',iv), 
          NA_character_
        ),
        fifelse(
          iv_nonlinear == TRUE & gly_nonlinear == 'quadratic', 
          paste0(iv,'_sq'), 
          NA_character_
        ),
        fifelse(
          iv_nonlinear == TRUE & gly_nonlinear == 'median' & !is.null(iv_shift), 
          paste0('above_median_',iv_shift), 
          NA_character_
        ),
        fifelse(
          iv_nonlinear == TRUE & gly_nonlinear == 'quadratic' & !is.null(iv_shift), 
          paste0(iv_shift,'_sq'), 
          NA_character_
        )
      ) |>
      na.omit() |>
      as.vector()
    # Heterogeneity variables
    het_vars = c(
      'dbwt',
      'dbwt_pred',
      'dbwt_pctl_pre',
      'dbwt_pred_pctl_pre',
      het_split
    )
    # Enforce spatial subsets (essentially rural, urban, or all)
    if (is.null(spatial_subset)) {
      est_dt = natality_dt
    } else {
      est_dt = natality_dt[str_detect(rural_grp, spatial_subset)]
    }
    
    # Merge the datasets with the requested variables
    est_dt = merge(
      x = est_dt[, unique(c(
        'fips_occ', 'fips_res', 'year', 'month',
        'rural_grp', 'rural_occ', 'rural_res',
        outcome_vars, het_vars, base_fe, dem_fes, dad_fes
      )), with = FALSE],
      y = comb_cnty_dt[, unique(c(
        'fips', 'year',
        glyph_vars,
        iv_vars,
        pest_controls,
        econ_controls,
        clustering
      )), with = FALSE],
      by.x = c('fips_res', 'year'),
      by.y = c('fips', 'year'),
      all = FALSE
    )

    # Build the requested pieces of the formula...
    # Glyph variable 
    fml_gly = fcase(
      is.null(gly_nonlinear), 'glyph_km2',
      gly_nonlinear == 'quadratic', 'glyph_km2 + I(glyph_km2^2)',
      gly_nonlinear == 'median', 'glyph_km2 + glyph_km2:above_median_glyph_km2', 
      default = 'glyph_km2'
    )
    
    # Outcome(s)
    fml_y = paste0(
      'c(',
      paste(outcome_vars, collapse = ', '),
      ')'
    )
    # Instruments: Without shift-share (event study)
    fml_iv = fcase(
      iv_nonlinear == FALSE | is.null(gly_nonlinear), 
        paste0('1 + i(year, ', iv, ', ref = 1995)'),
      iv_nonlinear == TRUE & gly_nonlinear == 'quadratic',  
        paste0(
          '1 + i(year, ', iv,', ref = 1995)',
          '+ i(year, ', iv,'_sq, ref = 1995)'
        ),
      iv_nonlinear == TRUE & gly_nonlinear == 'median', 
        paste0(
          '1 + i(year, ', iv,', ref = 1995)',
          '+ i(year, ', iv,':above_median_', iv,', ref = 1995)'
        )
    )
    # Instruments: Shift-share approach (if iv_shift is defined)
    if (!is.null(iv_shift)) {
      fml_iv_ss = paste0(iv_shift, ' + ', iv_shift, ':', iv)
        fcase(
          iv_nonlinear == FALSE | is.null(gly_nonlinear), 
            paste0(iv_shift, ' + ', iv_shift, ':', iv),
          iv_nonlinear == TRUE & gly_nonlinear == 'quadratic',  
            paste0(
              iv_shift, ' + ', iv_shift, ':', iv,
              iv_shift, '_sq + ', iv_shift, '_sq:', iv
            ),
          iv_nonlinear == TRUE & gly_nonlinear == 'median', 
            paste0(
              iv_shift, ' + ', iv_shift, ':', iv, ' + ',
              iv_shift,':above_median_', iv_shift, ' + ',
              iv_shift,':above_median_', iv_shift, ':', iv
            )
        )
    }
    # Controls
    fml_controls = paste0(
      ifelse(length(controls) > 1, 'sw(', ''),
      paste(c(
        ('1')[0 %in% controls],
        paste(pest_controls, collapse = '+')[1 %in% controls],
        paste(econ_controls, collapse = '+')[2 %in% controls],
        paste(c(pest_controls, econ_controls), collapse = '+')[3 %in% controls]
      ), collapse = ', '),
      ifelse(length(controls) > 1, ')', '')
    )
    # Fixed effects
    fml_fes = paste0(
      ifelse(length(fes) > 1, 'sw(', ''),
      paste(c(
        paste(base_fe, collapse = '+')[0 %in% fes],
        paste(c(base_fe, dem_fes), collapse = '+')[1 %in% fes],
        paste(c(base_fe, dad_fes), collapse = '+')[2 %in% fes],
        paste(c(base_fe, dem_fes, dad_fes), collapse = '+')[3 %in% fes]
      ), collapse = ', '),
      ifelse(length(fes) > 1, ')', '')
    )
    # Clusters
    fml_inf = ifelse(
      length(clustering) == 1,
      clustering %>% paste0('~ ', .),
      clustering %>% paste(collapse = ' + ') %>% paste0('~ ', .)
    ) %>% as.formula()

    # Formula: Reduced form
    fml_rf = paste(
      fml_y,
      '~',
      fml_iv,
      ifelse(
        !is.null(fml_controls),
        paste0(' + ', fml_controls),
        ''
      ),
      ' | ',
      fml_fes
    ) %>% as.formula()
    
    # Formula: 2SLS via event study
    fml_2sls = paste(
      fml_y,
      '~ 1',
      ifelse(
        !is.null(fml_controls),
        paste0(' + ', fml_controls),
        ''
      ),
      ' | ',
      fml_fes,
      ' | ',
      'glyph_km2 ~ ',
      fml_iv
    ) %>% as.formula()

    # Formula: 2SLS via shift-share (if iv_shift is defind)
    if (!is.null(iv_shift)) {
      fml_2sls_ss = paste(
        fml_y,
        '~ 1',
        ifelse(
          !is.null(fml_controls),
          paste0(' + ', fml_controls),
          ''
        ),
        ' | ',
        fml_fes,
        ' | ',
        'glyph_km2 ~ ',
        fml_iv_ss
      ) %>% as.formula()
    }

    # Make folder for the results
    dir_today = here('data-clean', 'results', today() %>% str_remove_all('-'))
    dir_today %>% dir.create()
    # Base filename with all options 
    base_name = paste0(
      '_outcome-', paste(str_remove_all(outcomes, '[^a-z]'), collapse = '-'),
      '_fe-', paste0(fes, collapse = ''),
      '_controls-', paste0(controls, collapse = ''),
      '_spatial-', spatial_subset,
      '_het-', het_split %>% str_remove_all('[^0-9a-z]'),
      '_iv-', iv %>% str_remove_all('[^0-9a-z]'),
      '_cl-', clustering %>% str_remove_all('[^a-z]') %>% paste0(collapse = ''),
      '_glynl-', gly_nonlinear,
      '_ivnl-', iv_nonlinear,
      '.qs'
    )
    # Estimate with or without heterogeneity splits
    if (!is.null(het_split)) {
      est_rf = feols(
        fml = fml_rf,
        cluster = fml_inf,
        data = est_dt,
        fsplit = het_split,
        lean = TRUE
      )
    } else {
      est_rf = feols(
        fml = fml_rf,
        cluster = fml_inf,
        data = est_dt,
        lean = TRUE
      )
    }
    # Save
    qsave(
      est_rf,
      file.path(dir_today, paste0('est_rf', base_name)),
      preset = 'fast'
    )
    rm(est_rf); invisible(gc())
   
    # Estimate: 2SLS with event study
    if (!is.null(het_split)) {
      est_2sls = feols(
        fml = fml_2sls,
        cluster = fml_inf,
        data = est_dt,
        fsplit = het_split,
        lean = TRUE
      )
    } else {
      est_2sls = feols(
        fml = fml_2sls,
        cluster = fml_inf,
        data = est_dt,
        lean = TRUE
      )
    }

    # Save
    qsave(
      est_2sls,
      file.path(dir_today, paste0('est_2sls', base_name)),
      preset = 'fast'
    )
    rm(est_2sls); invisible(gc())

    # Estimate: 2SLS with shift share (if iv_shift is defined)
    if (!is.null(iv_shift)) {
      if (!is.null(het_split)) {
        est_2sls_ss = feols(
          fml = fml_2sls_ss,
          cluster = fml_inf,
          data = est_dt,
          fsplit = het_split,
          lean = TRUE
        )
      } else {
        est_2sls_ss = feols(
          fml = fml_2sls_ss,
          cluster = fml_inf,
          data = est_dt,
          lean = TRUE
        )
      }
      # Save
      qsave(
        est_2sls_ss,
        file.path(dir_today, paste0('est_2sls_ss', base_name)),
        preset = 'fast'
      )
      rm(est_2sls); invisible(gc())
    }

    # Return something
    return('done')
  }


# Estimates: Main, pooled results ----------------------------------------------
  # Instrument: Yield diff percentile GMO
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    het_split = NULL,
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  # Instrument: Yield diff percentile GMO, east of 100th meridian
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'e100m_yield_diff_percentile_gmo',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    het_split = NULL,
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  # Instrument: Yield diff percentile GMO max
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo_max',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    het_split = NULL,
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  # Instrument: Yield diff GMO, 50-0
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_gmo_50_0',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    het_split = NULL,
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  # Instrument: 1990-1995 acreage percentiles
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'percentile_gm_acres',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    het_split = NULL,
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )


# Estimates: Heterogeneity by predicted quintile and month --------------------
  # Yield diff percentile GMO
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'e100m_yield_diff_percentile_gmo',
    spatial_subset = 'rural',
    het_split = 'pred_q5',
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo',
    spatial_subset = 'rural',
    het_split = 'pred_q5',
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  # Yield diff percentile GMO max
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo_max',
    spatial_subset = 'rural',
    het_split = 'pred_q5',
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo_max',
    spatial_subset = 'rural',
    het_split = 'pred_q5',
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  # More refined heterogeneity splits: Deciles 
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo',
    spatial_subset = 'rural',
    het_split = 'pred_q10',
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  # More refined heterogeneity splits: Deciles with top/bottom 1%, 5% separate)
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo',
    spatial_subset = 'rural',
    het_split = 'pred_q14',
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )
  # Heterogeneity by month of birth
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo',
    spatial_subset = 'rural',
    het_split = 'month',
    base_fe = c('year', 'fips'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips')
  )

# Non-linearities in glyph effect ---------------------------------------------
  # NOTE: These only work for iv = 'all_yield_diff_percentile_gmo' for now
  # Quadratic in glyphosate with IV nonlinear as well
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    het_split = NULL,
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips'),
    gly_nonlinear = 'quadratic',
    iv_nonlinear = TRUE
  )
  # Split glyphosate at median
  est_twfe(
    outcomes = c('dbwt', 'dbwt_pctl_pre', 'gestation'),
    iv = 'all_yield_diff_percentile_gmo',
    iv_shift = 'glyphosate_nat_100km',
    spatial_subset = 'rural',
    het_split = NULL,
    base_fe = c('year', 'fips', 'month'),
    fes = c(0, 3),
    controls = c(0, 3),
    clustering = c('year', 'state_fips'),
    gly_nonlinear = 'median',
    iv_nonlinear = TRUE
  )