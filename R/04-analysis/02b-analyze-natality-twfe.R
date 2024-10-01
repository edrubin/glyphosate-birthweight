# Notes ----------------------------------------------------------------------------------
#   Goal:   Run main TWFE analysis with various specifications.
#   Time:   ~100 hours for 11 runs (varies a lot with specification).


# Todo list ------------------------------------------------------------------------------
#   TODO Add a way to keep sample constant (due to missing demog controls)


# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    readr, readxl, stringr, fastverse, qs, patchwork,
    rlang, fixest, splines, parallel, magrittr, here,
    fst
  )
  fastverse_extend(topics = c('ST', 'DT', 'VI'))


# Load data ------------------------------------------------------------------------------
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data', 'clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Pre-period county-level crop yield percentiles
  pctl_dt = here(
    'data', 'clean', 'crop-acre-percentile-90-95.fst'
  ) |> read_fst(as.data.table = TRUE)
  # Shift-share data
  share_dt = here(
    'data', 'clean', 'glyph-nat-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data: Raw
  natality_dt = here(
    'data', 'clean', 'natality-micro.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data: Predicted birthweight and gestation
  pred_dbwt_dt = here(
    'data', 'clean', 'prediction', 'output',
    'dbwt-natality-micro-rf-train80-noindicators-2-full-cvpred.fst'
  ) %>% read_fst(
    as.data.table = TRUE,
    columns = c('row', 'dbwt', 'dbwt_pred')
  )
  pred_gest_dt = here(
    'data', 'clean', 'prediction', 'output',
    'gestation-natality-micro-rf-train80-noindicators-2-full-cvpred.fst'
  ) %>% read_fst(
    as.data.table = TRUE,
    columns = c('row', 'gestation', 'gestation_pred')
  )
  # Change name of DBWT (for checking merge later)
  setnames(pred_dbwt_dt, old = 'dbwt', new = 'dbwt_check')
  setnames(pred_gest_dt, old = 'gestation', new = 'gestation_check')


# Merge datasets -------------------------------------------------------------------------
  # Add row to natality data for merging with predictions
  natality_dt[, row := seq_len(.N)]
  # Merge predictions onto the full natality dataset (using row ID)
# NOTE: Also adding true birth weight to check the merge
  natality_dt %<>% merge(
    y = pred_dbwt_dt,
    by = 'row',
    all = FALSE
  )
  natality_dt %<>% merge(
    y = pred_gest_dt,
    by = 'row',
    all = FALSE
  )
  # Check the merge
  if (natality_dt[, fmean(dbwt == dbwt_check)] < 1) {
    stop('Bad merge between true natality and predicted birthweight.')
  } else {
    natality_dt[, dbwt_check := NULL]
  }
  if (natality_dt[, fmean(gestation == gestation_check)] < 1) {
    stop('Bad merge between true natality and predicted gestation.')
  } else {
    natality_dt[, gestation_check := NULL]
  }
  # Drop years prior (if desired)
# ADJUST years if desired
  # natality_dt %<>% .[year >= 1992]
  # Drop prediction dataset
  rm(pred_dbwt_dt, pred_gest_dt)
  invisible(gc())
  # Convert ages to numeric (harmonizes; can still use as factors later)
  natality_dt[, `:=`(
    mage = mage %>% as.integer(),
    fage = fage %>% as.integer()
  )]


# Create birth-outcome indexes -----------------------------------------------------------
  # Create index from 5 outcomes of interest
  # Similar to Currie, Greenstone, and Meckel (doi.org/10.1126/sciadv.1603021)
  index_dt = natality_dt[, .(
    dbwt,
    gestation,
    i_lbw = 1 * (dbwt < 2500),
    i_vlbw = 1 * (dbwt < 1500),
    i_preterm = 1 * (gestation <= 37)
  )]
  # Standardize
  index_dt %<>% fscale()
  # Calculate the covariance matrix
  cov_m = index_dt |> pwcov()
  # Create vector of 1s
  v_m = rep(1, nrow(cov_m))
  # Calculate the weights
  wt_m = solve(t(v_m) %*% solve(cov_m) %*% v_m) %*% t(v_m) %*% solve(cov_m)
  # Calculate the index
  index_m = as.matrix(index_dt) %*% t(wt_m)
  # Add indexes to the natality dataset
  natality_dt[, `:=`(
    index = index_m[, 1]
  )]
  # Clean up
  rm(index_dt, cov_m, v_m, wt_m, index_m)
  invisible(gc())


# Collapse birth anomalies ---------------------------------------------------------------
  # Create an indicator for any birth anomalies (without and with na.rm = TRUE)
  natality_dt[, `:=`(
    any_anomaly = 1 * (do.call(pmin, .SD) == 1),
    any_anomaly_nona = 1 * (do.call(function(...) pmin(..., na.rm = TRUE), .SD) == 1)
  ), .SDcols = febrile:baby_other_cong]
  # Drop individual birth anomalies
  anomalies = natality_dt |> fselect(febrile:baby_other_cong) |> names()
  natality_dt[, (anomalies) := NULL]


# Merge crop datasets --------------------------------------------------------------------
  # Add 90-95 percentiles to the county crop panel; then shift-share variables
  comb_cnty_dt %<>%
    merge(
      y = pctl_dt,
      by = 'GEOID',
      all = FALSE
    ) %>%
    merge(
      share_dt,
      by = c('GEOID', 'year'),
      all = TRUE
    )


# Define 'rural' counties ----------------------------------------------------------------
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
  # Clean up
  rm(lbls, lvls)
  invisible(gc())


# If testing without real data -----------------------------------------------------------
  # natality_dt = read.fst(
  #   here('data/clean/mini-data.fst'),
  #   as.data.table = TRUE
  # )


# Add birthweight percentiles ------------------------------------------------------------
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
    ),
    pred_q14 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = c(0, 0.01, 0.05, seq(0.1, 0.9, 0.1), 0.95, 0.99, 1),
      labels = 1:14, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q20 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 20,
      labels = 1:20, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  )]
  # Add interaction between percentiles and sex
  natality_dt[, `:=`(
    pred_q5_sex = finteraction(pred_q5, sex),
    pred_q4_sex = finteraction(pred_q4, sex),
    pred_q3_sex = finteraction(pred_q3, sex),
    pred_q10_sex = finteraction(pred_q10, sex),
    pred_q14_sex = finteraction(pred_q14, sex),
    pred_q20_sex = finteraction(pred_q20, sex)
  )]


# Add gestation percentiles --------------------------------------------------------------
  # Add percentiles for birthweight (based upon pre-1996 births)
  ecdf_pre = natality_dt[year < 1996, gestation %>% ecdf()]
  natality_dt[, gestation_pctl_pre := ecdf_pre(gestation)]
  # Repeat using predicted birthweights
  ecdf_pred_pre = natality_dt[year < 1996, gestation_pred %>% ecdf()]
  natality_dt[, gestation_pred_pctl_pre := ecdf_pred_pre(gestation_pred)]
  # Add quintiles, quartiles, terciles, deciles
  natality_dt[, `:=`(
    pred_gestation_q5 = cut(
      x = gestation_pred_pctl_pre,
      breaks = 5,
      labels = 1:5, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_gestation_q4 = cut(
      x = gestation_pred_pctl_pre,
      breaks = 4,
      labels = 1:4, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_gestation_q3 = cut(
      x = gestation_pred_pctl_pre,
      breaks = 3,
      labels = 1:3, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_gestation_q10 = cut(
      x = gestation_pred_pctl_pre,
      breaks = 10,
      labels = 1:10, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_gestation_q14 = cut(
      x = gestation_pred_pctl_pre,
      breaks = c(0, 0.01, 0.05, seq(0.1, 0.9, 0.1), 0.95, 0.99, 1),
      labels = 1:14, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_gestation_q20 = cut(
      x = gestation_pred_pctl_pre,
      breaks = 20,
      labels = 1:20, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  )]
  # Add interaction between percentiles and sex
  natality_dt[, `:=`(
    pred_gestation_q5_sex = finteraction(pred_gestation_q5, sex),
    pred_gestation_q4_sex = finteraction(pred_gestation_q4, sex),
    pred_gestation_q3_sex = finteraction(pred_gestation_q3, sex),
    pred_gestation_q10_sex = finteraction(pred_gestation_q10, sex),
    pred_gestation_q14_sex = finteraction(pred_gestation_q14, sex),
    pred_gestation_q20_sex = finteraction(pred_gestation_q20, sex)
  )]


# Add end-of-sample GM/suitability percentiles -------------------------------------------
  # Find the last year of the current natality sample (will use for pctl calc)
  yr_max = natality_dt[, fmax(year)]
  # Calculate average GLY/km2 and GMO suitability by county in last 3 years
# NOTE Subsetting to only rural counties (i.e., ECDF will use rural counties)
  end_dt =
    comb_cnty_dt[between(year, yr_max - 2, yr_max) & rural == TRUE, .(
      fips,
      glyph_km2,
      all_yield_diff_percentile_gmo
    )] |>
    fgroup_by(fips) |>
    fmean()
  # Calculate ECDFs for the variables
  ecdf_gly_end = end_dt[, glyph_km2 %>% ecdf()]
  ecdf_gaez_end = end_dt[, all_yield_diff_percentile_gmo %>% ecdf()]
  # Use ECDFs to calculate percentiles
  end_dt[, `:=`(
    glyph_km2_pctl_end = ecdf_gly_end(glyph_km2),
    all_yield_diff_percentile_gmo_pctl_end = ecdf_gaez_end(all_yield_diff_percentile_gmo)
  )]
  # Add tercile
  end_dt[, `:=`(
    glyph_km_end_q3 = cut(
      x = glyph_km2_pctl_end,
      breaks = 3,
      labels = 1:3, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  )]
  # Summarize terciles
  # end_dt |> qsu(~glyph_km_end_q3)
  # ggplot(data = end_dt, aes(x = all_yield_diff_percentile_gmo)) +
  #   geom_histogram() +
  #   theme_minimal() +
  #   facet_wrap(facets = 'glyph_km_end_q3')
# NOTE Of course, GLY/km2 correlates with suitability. Within GLY tercile, there
#      is some variation in suitability. Not sure it's good variation...
  # Add new variable into county-level dataset
  comb_cnty_dt %<>% join(
    y = end_dt[, .(fips, glyph_km_end_q3)],
    on = 'fips',
    how = 'left',
    validate = 'm:1'
  )
  # Clean up
  rm(yr_max, end_dt, ecdf_gly_end, ecdf_gaez_end)
  invisible(gc())


# Add additional variables to natality ---------------------------------------------------
  # Add indicators for low birthweight (<2500) and very low birthweight (<1500)
  natality_dt[, `:=`(
    i_lbw = 1 * (dbwt < 2500),
    i_vlbw = 1 * (dbwt < 1500),
    i_preterm = 1 * (gestation <= 37)
  )]
  # Add month of sample
  natality_dt[, year_month := paste0(year, '-', month)]
  # Add indicators for child's race and mother's race, ethnicity, marital status
  natality_dt[, `:=`(
    i_female = 1 * (sex == 'F'),
    i_m_black = 1 * (mrace == 2),
    i_m_nonwhite = 1 * (mrace != 1),
    i_m_hispanic = 1 * (mhisp > 0),
    i_m_married = 1 * (mar == 1)
  )]


# Additional county-year variables -------------------------------------------------------
  # Calculate additional variables
  comb_cnty_dt[, `:=`(
    empl_rate = tot_empl / tot_pop,
    farm_empl_per_cap = farm_empl / tot_pop
  )]
  # Change name of unemployment rate
  setnames(comb_cnty_dt, old = 'unemployment_rate', new = 'unempl_rate')
  # Load ag district-code crosswalk (variable: `asd_code`)
  asd_xwalk =
    data.table(read_fwf(
      here('data/download-manual/county_list.txt'),
      col_positions = fwf_positions(
        start = c(1, 6, 11, 17),
        end = c(2, 7, 13, 100)
      ),
      show_col_type = FALSE,
      skip = 12
    ))[!is.na(X1), .(
      state_fips = X1,
      asd_code = X2,
      county_fips = X3,
      GEOID = paste0(X1, X3),
      name = str_split_i(X4, '\\\t{3,6}', i = 1),
      historical = str_split_i(X4, '\\\t{3,6}', i = 2)
    )]
  asd_xwalk[,
    GEOID := fcase(
      GEOID == '46102', '46113',
      GEOID != '46102', GEOID
    )
  ]
  # Drop historical codes
  asd_xwalk %<>% .[historical != 2]
  # Drop the full states (only want counties)
  asd_xwalk %<>% .[county_fips != '000']
  # Drop "combined" counties
  asd_xwalk %<>% .[!(county_fips %in% c(888, 999))]
  # Combine ASD code with state to make unique
  asd_xwalk[, asd_code := paste0(state_fips, '-', asd_code)]
  # Drop unwanted columns
  asd_xwalk %<>% .[, .(GEOID, asd_code)]
  # Merge onto the estimation dataset
  comb_cnty_dt %<>% merge(
    y = asd_xwalk,
    by.x = 'fips',
    by.y = 'GEOID',
    all.x = TRUE,
    all.y = FALSE
  )
  # Fix DC's missing ASD code (assign one code for DC)
  comb_cnty_dt[fips == '11001' & is.na(asd_code), asd_code := '11-01']
  # Clean up
  rm(asd_xwalk)
  # Load ARMS farm resource region cross walk (variable: `farm_region`)
  frr_xwalk =
    here('data', 'download-manual', 'farm-resource-regions.xls') |>
    read_xls(skip = 2)
  # To data table
  setDT(frr_xwalk)
  # Grab region names
  frr_names = frr_xwalk[1:9, 7]
  setnames(frr_names, 'var')
  frr_names %<>% .[, tstrsplit(var, '=')]
  setnames(frr_names, c('num', 'label'))
  # Grab desired columns
  frr_xwalk %<>% .[, 1:2]
  # Rename columns
  setnames(frr_xwalk, c('fips', 'farm_region'))
  # Pad FIPS codes
  frr_xwalk[, fips := str_pad(fips, 5, 'left', '0')]
  # Recode regions
  frr_xwalk[, `:=`(
    farm_region = factor(
      farm_region,
      levels = frr_names$num,
      labels = frr_names$label
    )
  )]
  comb_cnty_dt %<>% merge(
    y = frr_xwalk,
    by = 'fips',
    all.x = TRUE,
    all.y = FALSE
  )
  # Copy Boulder County's region to Broomfield County
  set(
    x = comb_cnty_dt,
    i = comb_cnty_dt[fips == '08014', which = TRUE],
    j = 'farm_region',
    value = comb_cnty_dt[fips == "08013", ffirst(farm_region)]
  )
  # Clean up
  rm(frr_xwalk, frr_names)
  invisible(gc())


# Function: Water analysis ---------------------------------------------------------------
  # This function is called within the analysis script to estimate water spec
  estimate_water_spec = function(
    water_type, # c('bins-simple','bins-soil','ml-pred')
    iv, est_dt,
    controls, pest_controls, econ_controls,
    fml_y, fml_iv, fml_controls, fml_fes, fml_inf,
    het_split,
    dir_today, base_name
  ) {
    if (str_detect(water_type, 'bins')) {
      # Reading raw data
      county_exposure_dt =
        read.fst(
          path = here("data/watershed/county-exposure-dt.fst"),
          as.data.table = TRUE
        )
      # soil => separate estimates for high erodibility/precipitation
      if (water_type == 'bins-soil') {
        col_regex = c(
          paste0('^', iv, '_d\\d{2,3}$'),
          '^high_kls_d\\d{2,3}$',
          '^high_ppt_growing_season_d\\d{2,3}$'
        )
        # Making all of the interactions
        water_dt =
          melt(
            county_exposure_dt,
            id.vars = c('GEOID', 'year'),
            measure = patterns(col_regex),
            variable.name = 'distance_bin',
            value.name = c('trt', 'high_kls', 'high_ppt_growing_season')
          )[, .(
            fips_res = GEOID,
            year,
            distance_bin = fcase(
              distance_bin == '1', 'd50',
              distance_bin == '2', 'd100',
              distance_bin == '3', 'd150',
              distance_bin == '4', 'd200',
              distance_bin == '5', 'd250',
              distance_bin == '6', 'd300'
            ),
            trt, high_kls, high_ppt_growing_season,
            high_kls_ppt = high_kls * high_ppt_growing_season,
            high_kls_trt = high_kls * trt,
            high_ppt_trt = high_ppt_growing_season * trt,
            high_kls_ppt_trt = high_kls * high_ppt_growing_season * trt
          )] |>
          dcast(
            formula = fips_res + year ~ distance_bin,
            value.var = c(
              'trt',
              #'high_kls','high_ppt_growing_season',
              #'high_kls_ppt','high_kls_trt','high_ppt_trt',
              'high_kls_ppt_trt'
            )
          )
        # Turning trt back into real iv variable name
        setnames(
          water_dt,
          old = colnames(water_dt),
          new = str_replace(colnames(water_dt), 'trt', iv)
        )
      } else if (water_type == 'bins-simple') {
        # Simple just has upstream trt
        col_regex = c('^all_yield_diff_percentile_gmo_d\\d{2,3}')
        water_dt =
          get_vars(
            county_exposure_dt,
            vars = c('GEOID', 'year', col_regex),
            regex = TRUE
          ) |>
          setnames('GEOID', 'fips_res')
      } else {
        stop("water_type not recognized, use 'bins-simple','bins-soil', or 'ml-pred'.")
      }
      # Make the formula
      water_fml =
        paste0(
          'i(year, ',
          colnames(water_dt)[-(1:2)],
          ', ref = 1995)'
        ) |>
        paste(collapse = ' + ')
      fml_rhs_water = paste0(water_fml, ' + ', fml_controls)
      # Merge with main table
      water_dt |> setkey('fips_res', 'year')
      est_dt |> setkey('fips_res', 'year')
      # Merge with estimating data
      est_dt = merge(
        est_dt,
        water_dt,
        by = c('fips_res', 'year'),
        all.x = TRUE
      )
      rm(county_exposure_dt, water_dt)
      gc()
    } else if (water_type == 'ml-pred') {
      # Load the water-ML predictions
      county_exposure_pred_dt =
        read.fst(
          path = here("data/watershed/county-exposure-pred-dt.fst"),
          as.data.table = TRUE
        ) |>
        setnames('GEOID', 'fips_res') |>
        setkey('fips_res', 'year', 'month')
      est_dt |> setkey('fips_res', 'year', 'month')
      # Merge with estimating data
      est_dt = merge(
        est_dt,
        county_exposure_pred_dt,
        by = c('fips_res', 'year', 'month'),
        all.x = TRUE
      )
      # Add water to formula in reduced form
      rhs_raw_fml =
        CJ(
          controls = controls,
          pred = str_subset(colnames(county_exposure_pred_dt), 'pred')
        )[, .(
          fml_string = fcase(
            controls == 0, pred,
            controls == 1, paste0(pred, ' + ', paste(pest_controls, collapse = '+')),
            controls == 2, paste0(pred, ' + ', paste(econ_controls, collapse = '+')),
            controls == 3, paste0(
              pred, ' + ',
              paste(c(pest_controls, econ_controls), collapse = '+')
            )
          )
        )]$fml_string
      fml_rhs_water = paste0(
        ifelse(length(rhs_raw_fml) > 1, 'sw(', ''),
        paste(rhs_raw_fml, collapse = ', '),
        ifelse(length(rhs_raw_fml) > 1, ')', '')
      )
    } else {
      stop("water_type not recognized, use 'bins-simple','bins-soil', or 'ml-pred'.")
    }
    # Add water to reduced form formula
    fml_rf_water =
      paste(
        fml_y,
        '~',
        fml_iv, ' + ',
        fml_rhs_water,
        ' | ',
        fml_fes
      ) %>%
      as.formula()
    # Estimating the reduced form
    if (!is.null(het_split)) {
      est_rf_water = feols(
        fml = fml_rf_water,
        cluster = fml_inf,
        data = est_dt,
        fsplit = het_split,
        lean = TRUE
      )
    } else {
      est_rf_water = feols(
        fml = fml_rf_water,
        cluster = fml_inf,
        data = est_dt,
        lean = TRUE
      )
    }
    # Save
    qsave(
      est_rf_water,
      file.path(dir_today, paste0('est_water_rf-', water_type, '_', base_name)),
      preset = 'fast'
    )
    rm(est_rf_water)
    invisible(gc())
  }

# Function: Run TFWE analysis ------------------------------------------------------------
  est_twfe = function(
    outcomes = c(
      'dbwt', 'dbwt_pred',
      'dbwt_pctl_pre',
      'i_lbw', 'i_vlbw',
      'gestation', 'i_preterm',
      'c_section', 'any_anomaly',
      'index'
    ),
    iv = 'all_yield_diff_percentile_gmo_max',
    iv_shift = NULL,
    spatial_subset = 'rural',
    county_subset = NULL,
    county_subset_name = NULL,
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    dem_fe = TRUE,
    dad_fe = TRUE,
    control_sets = list2(
      'none',
      'pest',
      'unempl_rate',
      'empl_rate',
      c('unempl_rate', 'empl_rate'),
      c('unempl_rate', 'empl_rate', 'pct_farm_empl'),
      c('unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap'),
      c('age_share'),
      c('race_share'),
      c('pest', 'unempl_rate', 'empl_rate', 'age_share', 'race_share'),
      'fert',
      NULL
    ),
    name_suffix = NULL,
    clustering = c('year', 'state_fips'),
    gly_nonlinear = NULL,
    iv_nonlinear = FALSE,
    include_ols = FALSE,
    skip_iv = FALSE,
    water_types = NULL,
    ...
  ) {

    # Define outcome variables
    outcome_vars = outcomes

    # Drop NULL elements from the control sets
    control_sets = Filter(Negate(is.null), control_sets)

    # Define sets of control variables (helps when loading data)
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
      'metolachlor_km2', 'metribuzin_km2', 'nicosulfuron_km2'
    )
    pest_fml = paste(pest_controls, collapse = ' + ')
    # Economic controls
    econ_controls = c(
      'unempl_rate',
      'pct_farm_empl',
      'farm_empl_per_cap',
      'tot_pop',
      'inc_per_cap_farm',
      'inc_per_cap_nonfarm',
      'empl_rate',
      'farm_empl_per_cap',
      NULL
    )
    # Fertilizer controls
    fert_controls = c(
      'p_commercial_km2', 'n_commercial_km2',
      'p_farm_km2', 'n_farm_km2',
      'p_nonfarm_km2', 'n_nonfarm_km2',
      'p_manure_km2', 'n_manure_km2',
      NULL
    )
    fert_fml = paste(fert_controls, collapse = ' + ')
    # Age-share controls
# NOTE Omitting the 70+ share (colinear)
    age_controls = paste0('shr_age_', seq(0, 60, 10), '_all')
    age_fml = paste(age_controls, collapse = ' + ')
    # Race-share controls
    race_controls = c(
      'shr_raceblack_all',
      'shr_racewhite_all',
      'shr_hispanic_all',
      NULL
    )
    race_fml = paste(race_controls, collapse = ' + ')

    # Collecting glyphosate variables
    glyph_vars =
      c(
        'glyph_km2',
        names(comb_cnty_dt) |>
          str_subset('high_kls_ppt_growing_glyph') |>
          str_subset('local', negate = TRUE),
        fifelse(
          gly_nonlinear == 'median',
          'above_median_glyph_km2',
          NA_character_
        )
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
          paste0('above_median_', iv),
          NA_character_
        ),
        fifelse(
          iv_nonlinear == TRUE & gly_nonlinear == 'quadratic',
          paste0(iv, '_sq'),
          NA_character_
        ),
        fifelse(
          iv_nonlinear == TRUE & gly_nonlinear == 'median' & !is.null(iv_shift),
          paste0('above_median_', iv_shift),
          NA_character_
        ),
        fifelse(
          iv_nonlinear == TRUE & gly_nonlinear == 'quadratic' & !is.null(iv_shift),
          paste0(iv_shift, '_sq'),
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
      switch(
        !is.null(het_split),
        switch(het_split %in% names(natality_dt), het_split, NULL),
        NULL
      )
    )
    # Define variables that might be used for fixed effects (in comb_cnty_dt)
    fe_vars = c(
      'state_fips', 'census_region', 'census_division',
      'asd_code', 'farm_region'
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
        'fips_occ', 'fips_res', 'year', 'month', 'year_month',
        'rural_grp', 'rural_occ', 'rural_res',
        outcome_vars, het_vars, dem_fes, dad_fes
      )), with = FALSE],
      y = comb_cnty_dt[, unique(c(
        'fips', 'year',
        switch(
          !is.null(het_split),
          switch(het_split %in% names(comb_cnty_dt), het_split, NULL),
          NULL
        ),
        glyph_vars,
        iv_vars,
        fe_vars,
        pest_controls,
        econ_controls,
        fert_controls,
        clustering
      )), with = FALSE],
      by.x = c('fips_res', 'year'),
      by.y = c('fips', 'year'),
      all = FALSE
    )

    # Load and add additional datasets (if controls require them)
    if (any(c('age_share', 'race_share', 'pop_all') %in% unlist(control_sets))) {
      # Load the age-share dataset
# NOTE The 1969-2022 version has a longer time series but lacks "origin" data
      seer_dt =
        here('data', 'seer', 'seer-shares-allpop-1990-2022.fst') |>
        read.fst(as.data.table = TRUE)
      # Merge
      est_dt %<>% merge(
        y = seer_dt,
        by.x = c('fips_res', 'year'),
        by.y = c('fips', 'yr'),
        all.x = TRUE,
        all.y = FALSE
      )
    }

    # Enforce regional subsets (e.g., Census regions)
    if (!is.null(county_subset)) {
      # Take the implied subset
      est_dt %<>% .[fips_res %in% county_subset]
    }

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
          '1 + i(year, ', iv, ', ref = 1995)',
          '+ i(year, ', iv, '_sq, ref = 1995)'
        ),
      iv_nonlinear == TRUE & gly_nonlinear == 'median',
        paste0(
          '1 + i(year, ', iv, ', ref = 1995)',
          '+ i(year, ', iv, '*above_median_', iv, ', ref = 1995)'
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
              iv_shift, ':above_median_', iv_shift, ' + ',
              iv_shift, ':above_median_', iv_shift, ':', iv
            )
        )
    }

    # Fixed effects
    fml_fes =
      c(
        if (dem_fe == TRUE) dem_fes else NULL,
        if (dad_fe == TRUE) dad_fes else NULL,
        base_fe,
        NULL
      ) |>
      paste(collapse = ' + ')
    if (fml_fes == '') fml_fes = '1'

    # Controls
    # Start by recording the length and class (for subcases)
    cs_len = control_sets |> length()
    cs_class = control_sets |> class()
    # Check cases (based on length and class)
    if (
      # Case 1: No controls (length 0 or 'none')
      (cs_len == 0) || all(control_sets == 'none')
    ) {
      # No controls: Set to NULL
      fml_controls = NULL
    } else if (
      # Case 2: One set of controls
      ((cs_len == 1) && (cs_class == 'list')) ||
      ((cs_class == 'character') && (control_sets != 'none'))
    ) {
      # One set of controls: Collapse
      fml_controls =
        control_sets |>
        unlist() |>
        paste(collapse = ' + ') |>
        str_replace(pattern = 'fert', replacement = fert_fml) |>
        str_replace(pattern = 'age_share', replacement = age_fml) |>
        str_replace(pattern = 'pest', replacement = pest_fml) |>
        str_replace(pattern = 'race_share', replacement = race_fml)
    } else if (
      # Case 3: Multiple sets of controls (iterate over list elements)
      (cs_len > 1) && (cs_class == 'list')
    ) {
      # Multiple sets of controls: Iterate over list elements
      fml_controls =
        lapply(
          X = seq_along(control_sets),
          FUN = function(i) {
            # Grab the set of controls
            cs_i = control_sets[[i]]
            # If NULL or 'none', then return '1'; else collapse
            if (is.null(cs_i) || all(cs_i == 'none')) {
              '1'
            } else {
              cs_i |>
                unlist() |>
                paste(collapse = ' + ') |>
                str_replace(pattern = 'fert', replacement = fert_fml) |>
                str_replace(pattern = 'age_share', replacement = age_fml) |>
                str_replace(pattern = 'pest', replacement = pest_fml) |>
                str_replace(pattern = 'race_share', replacement = race_fml)
            }
          }
        ) |>
        paste(collapse = ', ')
      # Wrap with fixest `sw` function
      fml_controls = paste0('sw(', fml_controls, ')')
    }

    # Clusters
    fml_inf =
      ifelse(
        length(clustering) == 1,
        clustering %>% paste0('~ ', .),
        clustering %>% paste(collapse = ' + ') %>% paste0('~ ', .)
      ) %>%
      as.formula()

    # Formula: Reduced form
    fml_rf =
      paste(
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
      ) |>
      as.formula()

    # Formula: 2SLS via event study
    fml_2sls =
      paste(
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
        fml_gly,
        ' ~ ',
        fml_iv
      ) |>
      as.formula()

    # Formula: 2SLS via shift-share (if iv_shift is defind)
    if (!is.null(iv_shift)) {
      fml_2sls_ss =
        paste(
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
        ) |>
        as.formula()
    }

    # Formula: OLS
    if (include_ols == TRUE) {
      fml_ols =
        paste(
          fml_y,
          ' ~ ',
          fml_gly,
          ifelse(
            !is.null(fml_controls),
            paste0(' + ', fml_controls),
            ''
          ),
          ' | ',
          fml_fes
        ) |>
        as.formula()
    }

    # Make folder for the results
    dir_today = here('data', 'results', 'micro-new')
    dir_today |> dir.create(showWarnings = FALSE, recursive = TRUE)
    # Calculate time suffix
    time_suffix = now() |> as.integer() |> as.character()

    # Base filename with all options
    base_name = paste0(
      # Outcomes
      '_outcome-',
      paste(str_remove_all(outcomes, '[^a-z]'), collapse = '-'),
      # Fixed effects (including demographic FEs)
      '_fe-',
      ifelse(
        is.null(base_fe) && dem_fe == FALSE && dad_fe == FALSE,
        'none',
        paste0(
          c(base_fe, ifelse(dem_fe, 'dem', ''), ifelse(dad_fe, 'dad', '')),
          collapse = '-'
        ) |>
          str_replace_all('\\^', 'X') |>
          str_remove_all('_')
      ),
      # Demographic (mother and child demographics) FEs
      ifelse(dem_fe, '_dem-fe', ''),
      # Father demographic FEs
      ifelse(dad_fe, '_dad-fe', ''),
      # Spatial subset
      ifelse(is.null(spatial_subset), '', paste0('_spatial-', spatial_subset)),
      # County subset
      ifelse(
        is.null(county_subset),
        '',
        ifelse(
          is.null(county_subset_name),
          '_county-subset-',
          paste0('_county-', county_subset_name)
        )
      ),
      # Heterogeneity split
      ifelse(
        is.null(het_split),
        '',
        paste0('_het-', het_split %>% str_remove_all('[^0-9a-z]'))
      ),
      # Instrument
      '_iv-',
      iv %>% str_remove_all('[^0-9a-z]'),
      # Clustering
      '_cl-',
      clustering %>% str_remove_all('[^a-z]') %>% paste0(collapse = '-'),
      # Nonlinearity in GLY
      ifelse(is.null(gly_nonlinear), '', paste0('_glynl-', gly_nonlinear)),
      # Nonlinearity in IV
      ifelse(iv_nonlinear, '_ivnl', ''),
      # Add name suffix (if defined)
      ifelse(
        is.null(name_suffix),
        '',
        paste0('_', name_suffix)
      ),
      # Add time suffix
      paste0('_', time_suffix),
      # File suffix
      '.qs'
    )

    # Save information about the model
    qsave(
      list2(
        outcomes = outcomes,
        iv = iv,
        iv_shift = iv_shift,
        spatial_subset = spatial_subset,
        county_subset = county_subset,
        county_subset_name = county_subset_name,
        het_split = het_split,
        base_fe = base_fe,
        dem_fe = dem_fe,
        dad_fe = dad_fe,
        control_sets = control_sets,
        name_suffix = name_suffix,
        clustering = clustering,
        gly_nonlinear = gly_nonlinear,
        iv_nonlinear = iv_nonlinear,
        include_ols = include_ols,
        skip_iv = skip_iv,
        water_types = water_types
      ),
      file.path(dir_today, paste0('info_', time_suffix, '.qs')),
      preset = 'fast'
    )

    # Estimate reduced form and 2SLS
    if (skip_iv == FALSE) {
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
      rm(est_rf)
      invisible(gc())

      # Estimate: 2SLS with event study
      if (!is.null(het_split)) {
        est_2sls = feols(
          fml = fml_2sls,
          cluster = fml_inf,
          data = est_dt[!(year %in% 1990:1991)],
          fsplit = het_split,
          lean = TRUE
        )
      } else {
        est_2sls = feols(
          fml = fml_2sls,
          cluster = fml_inf,
          data = est_dt[!(year %in% 1990:1991)],
          lean = TRUE
        )
      }
      # Save
      qsave(
        est_2sls,
        file.path(dir_today, paste0('est_2sls', base_name)),
        preset = 'fast'
      )
      rm(est_2sls)
      invisible(gc())

      # Estimate: 2SLS with shift share (if iv_shift is defined)
      if (!is.null(iv_shift)) {
        if (!is.null(het_split)) {
          est_2sls_ss = feols(
            fml = fml_2sls_ss,
            cluster = fml_inf,
            data = est_dt[!(year %in% 1990:1991)],
            fsplit = het_split,
            lean = TRUE
          )
        } else {
          est_2sls_ss = feols(
            fml = fml_2sls_ss,
            cluster = fml_inf,
            data = est_dt[!(year %in% 1990:1991)],
            lean = TRUE
          )
        }
        # Save
        qsave(
          est_2sls_ss,
          file.path(dir_today, paste0('est_2sls_ss', base_name)),
          preset = 'fast'
        )
        rm(est_2sls)
        invisible(gc())
      }
    }

    # Estimate OLS
    if (include_ols == TRUE) {
      if (!is.null(het_split)) {
        est_ols = feols(
          fml = fml_ols,
          cluster = fml_inf,
          data = est_dt[!(year %in% 1990:1991)],
          fsplit = het_split,
          lean = TRUE
        )
      } else {
        est_ols = feols(
          fml = fml_ols,
          cluster = fml_inf,
          data = est_dt[!(year %in% 1990:1991)],
          lean = TRUE
        )
      }
      # Save
      qsave(
        est_ols,
        file.path(dir_today, paste0('est_ols', base_name)),
        preset = 'fast'
      )
      rm(est_ols)
      invisible(gc())
    }

    # Estimating water results
    if (!is.null(water_types)) {
      lapply(
        water_types,
        estimate_water_spec,
        iv = iv, est_dt = est_dt, controls = controls,
        pest_controls = pest_controls, econ_controls = econ_controls,
        fml_y = fml_y, fml_iv = fml_iv,
        fml_controls = fml_controls,
        fml_fes = fml_fes, fml_inf = fml_inf,
        het_split = het_split,
        dir_today = dir_today,
        base_name = base_name
      )
    }
    # Return something
    return('done')
  }


# # Estimates: Main, pooled results --------------------------------------------------------
#   # Instrument: Yield diff percentile GMO
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = 'glyphosate_nat_100km',
#     spatial_subset = 'rural',
#     het_split = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = 0:3,
#     clustering = c('year', 'state_fips'),
#     include_ols = TRUE
#   )
#   # Instrument: Yield diff percentile GMO, east of 100th meridian
#   est_twfe(
#     iv = 'e100m_yield_diff_percentile_gmo',
#     iv_shift = NULL,
#     spatial_subset = 'rural',
#     het_split = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )
#   # Instrument: Yield diff percentile GMO max
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo_max',
#     iv_shift = NULL,
#     spatial_subset = 'rural',
#     het_split = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )
#   # Instrument: Yield diff GMO, 50-0
#   est_twfe(
#     iv = 'all_yield_diff_gmo_50_0',
#     iv_shift = NULL,
#     spatial_subset = 'rural',
#     het_split = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )


# # Estimate various FE specifications -----------------------------------------------------
#   # Define fixed effects for robustness check
#   # Standard: county, year-month
#   fe0 = c('fips_res', 'fips_occ', 'year_month')
#   # R2, Part 1: county, year, month
#   fe1 = c('fips_res', 'fips_occ', 'year', 'month')
#   # R2, Part 2: county, sample year-by-census region, calendar month-by-census region;
#   fe2a = c('fips_res', 'fips_occ', 'year^census_region', 'month^census_region')
#   # Switching to year-month-census region
#   fe2b = c('fips_res', 'fips_occ', 'year_month^census_region')
#   # Repeating with census division
#   fe2c = c('fips_res', 'fips_occ', 'year^census_division', 'month^census_division')
#   fe2d = c('fips_res', 'fips_occ', 'year_month^census_division')
#   # Adding the same but for farm resource region
#   # R2, Part 2: county, sample year-by-farm region, calendar month-by-farm region;
#   fe3a = c('fips_res', 'fips_occ', 'year^farm_region', 'month^farm_region')
#   fe3b = c('fips_res', 'fips_occ', 'year_month^farm_region')
#   # R2, Part 3: county, sample year-by-state, calendar month-by-state
#   fe4a = c('fips_res', 'fips_occ', 'year^state_fips', 'month^state_fips')
#   # Switching to year-month-state
#   fe4b = c('fips_res', 'fips_occ', 'year_month^state_fips')
#   # R2, Part 4: county, sample year-month-by-census region, calendar month-by-ag district
#   fe5a = c('fips_res', 'fips_occ', 'year_month^census_region', 'month^asd_code')
#   fe5b = c('fips_res', 'fips_occ', 'year_month^farm_region', 'month^asd_code')
#   # Add ag. district interactions
#   fe6a = c('fips_res', 'fips_occ', 'year^asd_code', 'month^asd_code')
#   fe6b = c('fips_res', 'fips_occ', 'year_month^asd_code')
#   # Create a vector of the FE specifications
#   fe_v = ls(pattern = '^fe[0-9][a-z]?')
#   # Iterate over the fixed effects
# # TODO Remove `tail` after running (error happened after 6th item)
#   blah = lapply(X = fe_v |> tail(-6), FUN = function(fe_i) {
#     # Estimate the desired set of FEs
#     est_twfe(
#       outcomes = c(
#         'dbwt',
#         'gestation',
#         'index'
#       ),
#       iv = 'all_yield_diff_percentile_gmo_max',
#       iv_shift = NULL,
#       spatial_subset = 'rural',
#       county_subset = NULL,
#       county_subset_name = NULL,
#       het_split = NULL,
#       base_fe = get(fe_i),
#       dem_fe = TRUE,
#       dad_fe = TRUE,
#       control_set = list2(
#         'none',
#         c(
#           'pest',
#           'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
#           'inc_per_cap_farm', 'inc_per_cap_nonfarm',
#           'pop_all',
#           'age_share', 'race_share',
#           'fert'
#          )
#       ),
#       clustering = c('year', 'state_fips'),
#       gly_nonlinear = NULL,
#       iv_nonlinear = FALSE,
#       include_ols = FALSE,
#       skip_iv = FALSE,
#       water_types = NULL,
#       name_suffix = fe_i
#     )
#     # Let us know we are done
#     cat(
#       'Finished with FE spec',
#       which(fe_v == fe_i), '/', length(fe_v), '\n',
#       get(fe_i) |> paste(collapse = ' + '), '\n',
#       lubridate::now() |> as.character(), '\n\n'
#     )
#     # Return something
#     return('Done!')
#   })
#   rm(blah)
#   invisible(gc())


# # Standard FEs with various controls -----------------------------------------------------
# # NOTE Always using county, year-month, and demographic FEs
# # NOTE Run time: ~12 hours
#   est_twfe(
#     outcomes = c(
#       'dbwt',
#       'gestation',
#       'index'
#     ),
#     iv = 'all_yield_diff_percentile_gmo_max',
#     iv_shift = NULL,
#     spatial_subset = 'rural',
#     county_subset = NULL,
#     county_subset_name = NULL,
#     het_split = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     dem_fe = TRUE,
#     dad_fe = TRUE,
#     control_sets = list2(
#       'none',
#       c('pest', 'unempl_rate'),
#       c('unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap'),
#       c(
#         'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
#         'inc_per_cap_farm', 'inc_per_cap_nonfarm'
#       ),
#       c(
#         'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
#         'inc_per_cap_farm', 'inc_per_cap_nonfarm',
#         'pop_all'
#       ),
#       'age_share',
#       c('age_share', 'race_share'),
#       'fert',
#       c(
#         'pest',
#         'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
#         'inc_per_cap_farm', 'inc_per_cap_nonfarm',
#         'pop_all',
#         'age_share', 'race_share',
#         'fert'
#        )
#     ),
#     name_suffix = NULL,
#     clustering = c('year', 'state_fips'),
#     gly_nonlinear = NULL,
#     iv_nonlinear = FALSE,
#     include_ols = FALSE,
#     skip_iv = FALSE,
#     water_types = NULL
#   )


# Alternative instruments using historic acreages/yields ---------------------------------
# NOTE Run time: ~11 hours
  # Instrument: 1990-1995 acreage percentiles (normalized by total cnty size)
  est_twfe(
    outcomes = c(
      'dbwt',
      'dbwt_pctl_pre',
      'i_lbw', 'i_vlbw',
      'gestation', 'i_preterm',
      'c_section',
      'index'
    ),
    iv = 'percentile_gm_acres_pct_cnty',
    iv_shift = NULL,
    spatial_subset = 'rural',
    county_subset = NULL,
    county_subset_name = NULL,
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    dem_fe = TRUE,
    dad_fe = TRUE,
    control_set = list2(
      'none',
      c(
        'pest',
        'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
        'inc_per_cap_farm', 'inc_per_cap_nonfarm',
        'pop_all',
        'age_share', 'race_share',
        'fert'
       )
    ),
    name_suffix = NULL,
    clustering = c('year', 'state_fips'),
    gly_nonlinear = NULL,
    iv_nonlinear = FALSE,
    include_ols = FALSE,
    skip_iv = FALSE,
    water_types = NULL
  )
  # Instrument: 1990-1995 max yield percentile
  est_twfe(
    outcomes = c(
      'dbwt',
      'dbwt_pctl_pre',
      'i_lbw', 'i_vlbw',
      'gestation', 'i_preterm',
      'c_section',
      'index'
    ),
    iv = 'percentile_gm_yield_max',
    iv_shift = NULL,
    spatial_subset = 'rural',
    county_subset = NULL,
    county_subset_name = NULL,
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    dem_fe = TRUE,
    dad_fe = TRUE,
    control_set = list2(
      'none',
      c(
        'pest',
        'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
        'inc_per_cap_farm', 'inc_per_cap_nonfarm',
        'pop_all',
        'age_share', 'race_share',
        'fert'
       )
    ),
    name_suffix = NULL,
    clustering = c('year', 'state_fips'),
    gly_nonlinear = NULL,
    iv_nonlinear = FALSE,
    include_ols = FALSE,
    skip_iv = FALSE,
    water_types = NULL
  )


# Alternative instruments for Eastern US -------------------------------------------------
# NOTE Run time: ?
  # Instrument: 1990-1995 acreage percentiles (normalized by total cnty size)
  est_twfe(
    outcomes = c(
      'dbwt',
      'gestation',
      'index'
    ),
    iv = 'percentile_gm_acres_pct_cnty',
    iv_shift = NULL,
    spatial_subset = 'rural',
    county_subset = NULL,
    county_subset_name = NULL,
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    dem_fe = TRUE,
    dad_fe = TRUE,
    control_set = list2(
      'none',
      c(
        'pest',
        'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
        'inc_per_cap_farm', 'inc_per_cap_nonfarm',
        'pop_all',
        'age_share', 'race_share',
        'fert'
       )
    ),
    name_suffix = NULL,
    clustering = c('year', 'state_fips'),
    gly_nonlinear = NULL,
    iv_nonlinear = FALSE,
    include_ols = FALSE,
    skip_iv = FALSE,
    water_types = NULL
  )
  # Instrument: 1990-1995 max yield percentile
  est_twfe(
    outcomes = c(
      'dbwt',
      'gestation',
      'index'
    ),
    iv = 'percentile_gm_yield_max',
    iv_shift = NULL,
    spatial_subset = 'rural',
    county_subset = NULL,
    county_subset_name = NULL,
    het_split = NULL,
    base_fe = c('year_month', 'fips_res', 'fips_occ'),
    dem_fe = TRUE,
    dad_fe = TRUE,
    control_set = list2(
      'none',
      c(
        'pest',
        'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
        'inc_per_cap_farm', 'inc_per_cap_nonfarm',
        'pop_all',
        'age_share', 'race_share',
        'fert'
       )
    ),
    name_suffix = NULL,
    clustering = c('year', 'state_fips'),
    gly_nonlinear = NULL,
    iv_nonlinear = FALSE,
    include_ols = FALSE,
    skip_iv = FALSE,
    water_types = NULL
  )


# # Estimates: Heterogeneity by predicted quintile and month ------------------------------
#   # Yield diff percentile GMO
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     spatial_subset = 'rural',
#     het_split = 'pred_q5',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )
#   est_twfe(
#     iv = 'e100m_yield_diff_percentile_gmo',
#     spatial_subset = 'rural',
#     het_split = 'pred_q5',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )
#   # More refined heterogeneity splits: Deciles
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     spatial_subset = 'rural',
#     het_split = 'pred_q10',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )
#   # More refined heterogeneity splits: Deciles with top/bottom 1%, 5% separate)
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     spatial_subset = 'rural',
#     het_split = 'pred_q14',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )
#   # More refined heterogeneity splits: vigintiles
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     spatial_subset = 'rural',
#     het_split = 'pred_q20',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )
#   # Heterogeneity by month of birth
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     spatial_subset = 'rural',
#     het_split = 'month',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )
#   # Yield diff percentile GMO; heterogeneity by quintile and sex
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     spatial_subset = 'rural',
#     het_split = 'pred_q5_sex',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips')
#   )


# Test changing demographics -------------------------------------------------------------
#   # Instrument: Yield diff percentile GMO
#   est_twfe(
#     outcomes = c(
#       'i_female', 'i_m_black', 'i_m_nonwhite', 'i_m_hispanic', 'i_m_married'
#     ),
#     iv = 'all_yield_diff_percentile_gmo',
#     spatial_subset = 'rural',
#     het_split = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 0,
#     controls = 0,
#     clustering = c('year', 'state_fips')
#   )
#   # Estimate white and nonwhite mothers separately
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = 'glyphosate_nat_100km',
#     spatial_subset = 'rural',
#     het_split = 'i_m_nonwhite',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = c(0, 3),
#     clustering = c('year', 'state_fips')
#   )


# # Separate estimates by region -----------------------------------------------------------
#   # Estimate only for midwest and northeast
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = 'glyphosate_nat_100km',
#     spatial_subset = 'rural',
#     county_subset =
#       comb_cnty_dt[census_region %in% c('Midwest', 'Northeast'), funique(fips)],
#     county_subset_name = 'mw-ne',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips')
#   )
#   # Estimate only for the south
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = 'glyphosate_nat_100km',
#     spatial_subset = 'rural',
#     county_subset =
#       comb_cnty_dt[census_region == 'South', funique(fips)],
#     county_subset_name = 'south',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips')
#   )
#   # Estimate only for the south without Florida
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = 'glyphosate_nat_100km',
#     spatial_subset = 'rural',
#     county_subset =
#       comb_cnty_dt[census_region == 'South' & state_fips != '12', funique(fips)],
#     county_subset_name = 'south-nofl',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips')
#   )


# # Just the OLS model --------------------------------------------------------------------
#   # This should only estimate OLS results (not rf or 2sls or water)
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = NULL,
#     spatial_subset = 'rural',
#     het_split = 'pred_q5',
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = c(0, 3),
#     controls = 0:3,
#     clustering = c('year', 'state_fips'),
#     include_ols = TRUE,
#     skip_iv = TRUE
#   )


# # Water results -------------------------------------------------------------------------
#   # This should only estimate water results (not rf or 2sls or ols)
# # NOTE Takes ~ 5 hours to run
#   est_twfe(
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = NULL,
#     spatial_subset = 'rural',
#     het_split = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips'),
#     include_ols = FALSE,
#     skip_iv = TRUE,
#     water_types = c('ml-pred', 'bins-soil') #'bins-simple'
#   )


# # Estimate by counties' rural status -----------------------------------------------------
# # NOTE Crashed during second-stage estimation
#   est_twfe(
#     outcomes = 'dbwt',
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = NULL,
#     spatial_subset = NULL,
#     het_split = 'rural_res',
#     county_subset = NULL,
#     county_subset_name = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips')
#   )
#   est_twfe(
#     outcomes = 'dbwt',
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = NULL,
#     spatial_subset = NULL,
#     het_split = 'rural_grp',
#     county_subset = NULL,
#     county_subset_name = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips')
#   )


# # Estimate on purely urban births --------------------------------------------------------
#   est_twfe(
#     outcomes = 'dbwt',
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = NULL,
#     spatial_subset = 'urban res; urban occ',
#     het_split = NULL,
#     county_subset = NULL,
#     county_subset_name = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips')
#   )


# # Estimate on purely rural births --------------------------------------------------------
#   est_twfe(
#     outcomes = c('dbwt', 'gestation'),
#     iv = 'all_yield_diff_percentile_gmo',
#     iv_shift = NULL,
#     spatial_subset = 'rural res; rural occ',
#     het_split = NULL,
#     county_subset = NULL,
#     county_subset_name = NULL,
#     base_fe = c('year_month', 'fips_res', 'fips_occ'),
#     fes = 3,
#     controls = 3,
#     clustering = c('year', 'state_fips')
#   )
