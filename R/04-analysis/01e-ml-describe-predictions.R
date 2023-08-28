# Notes ----------------------------------------------------------------------------------
#   Goal:   Describe predictions.
#   Time:   ~9 minutes


# Setup ------------------------------------------------------------------------
  # Load packages
  pacman::p_load(fastverse, qs, patchwork, fixest, parallel, magrittr, here)
  fastverse_extend(topics = c('ST', 'DT', 'VI', 'SP'))


# Load data --------------------------------------------------------------------
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data-clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data: Raw
  natality_dt = here(
    'data-clean', 'natality-micro.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data: Predictions
  prediction_dt = here(
    'data-clean', 'natality-micro-rf-train80-noindicators-0.fst'
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
  # Drop years prior to 1992
  natality_dt %<>% .[year >= 1992]
  # Drop prediction dataset
  rm(prediction_dt)
  invisible(gc())
  # Convert ages to numeric (harmonizes; can still use as factors later)
  natality_dt[, `:=`(
    mage = mage %>% as.integer(),
    fage = fage %>% as.integer()
  )]


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
  # Create a rounded version of the prediction (two decimals)
  natality_dt[, `:=`(
    dbwt_rnd_pctl_rural_pre = dbwt_pctl_rural_pre %>% round(2),
    dbwt_pred_rnd_pctl_rural_pre = dbwt_pred_pctl_rural_pre %>% round(2)
  )]


# Summarize predictions by predicted percentile ------------------------------------------
# TODO Add summaries of coded variables like education, age, birth facility
  # Summarize by predicted percentile
  pred_pctl = natality_dt[, .(
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M '),
    m_age_mean = fmean(mage),
    m_pct_white = fmean(mrace == 1),
    m_pct_black = fmean(mrace == 2),
    m_pct_amind = fmean(mrace == 3),
    m_pct_asian = fmean(mrace == 4),
    m_pct_hispanic = fmean(mhisp > 0),
    m_pct_nonhispanic = fmean(mhisp == 0),
    f_age_mean = fmean(5 * fage + 7.5),
    f_pct_white = fmean(frace == 1),
    f_pct_black = fmean(frace == 2),
    f_pct_amind = fmean(frace == 3),
    f_pct_asian = fmean(frace == 4),
    f_pct_hispanic = fmean(fhisp > 0),
    f_pct_nonhispanic = fmean(fhisp == 0),
    married_pct = fmean(mar == 1),
    order_mean = fmean(total_birth_order),
    rural_pct = fmean(rural),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), keyby = dbwt_pred_rnd_pctl_rural_pre]
  # Save 
  write_fst(
    x = pred_pctl,
    path = here(
      'data-clean', 'prediction', 'summaries',
      'pctl-all.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(pred_pctl); invisible(gc())
  # Repeat FOR RURAL births
  pred_pctl_rural = natality_dt[rural == TRUE, .(
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M '),
    m_age_mean = fmean(mage),
    m_pct_white = fmean(mrace == 1),
    m_pct_black = fmean(mrace == 2),
    m_pct_amind = fmean(mrace == 3),
    m_pct_asian = fmean(mrace == 4),
    m_pct_hispanic = fmean(mhisp > 0),
    m_pct_nonhispanic = fmean(mhisp == 0),
    f_age_mean = fmean(5 * fage + 7.5),
    f_pct_white = fmean(frace == 1),
    f_pct_black = fmean(frace == 2),
    f_pct_amind = fmean(frace == 3),
    f_pct_asian = fmean(frace == 4),
    f_pct_hispanic = fmean(fhisp > 0),
    f_pct_nonhispanic = fmean(fhisp == 0),
    married_pct = fmean(mar == 1),
    order_mean = fmean(total_birth_order),
    rural_pct = fmean(rural),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), keyby = dbwt_pred_rnd_pctl_rural_pre]
  # Save 
  write_fst(
    x = pred_pctl_rural,
    path = here(
      'data-clean', 'prediction', 'summaries',
      'pctl-rural.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(pred_pctl_rural); invisible(gc())


# Summarize predictions by predicted percentile split by pre/post 1996 -------------------
# TODO Add summaries of coded variables like education, age, birth facility
  # Summarize by predicted percentile and pre/post
  pred_pctl = natality_dt[, .(
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M '),
    m_age_mean = fmean(mage),
    m_pct_white = fmean(mrace == 1),
    m_pct_black = fmean(mrace == 2),
    m_pct_amind = fmean(mrace == 3),
    m_pct_asian = fmean(mrace == 4),
    m_pct_hispanic = fmean(mhisp > 0),
    m_pct_nonhispanic = fmean(mhisp == 0),
    f_age_mean = fmean(5 * fage + 7.5),
    f_pct_white = fmean(frace == 1),
    f_pct_black = fmean(frace == 2),
    f_pct_amind = fmean(frace == 3),
    f_pct_asian = fmean(frace == 4),
    f_pct_hispanic = fmean(fhisp > 0),
    f_pct_nonhispanic = fmean(fhisp == 0),
    married_pct = fmean(mar == 1),
    order_mean = fmean(total_birth_order),
    rural_pct = fmean(rural),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), keyby = .(
    dbwt_pred_rnd_pctl_rural_pre,
    period = fcase(year < 1996, 'pre', year >= 1996, 'post')
  )]
  # Save 
  write_fst(
    x = pred_pctl,
    path = here(
      'data-clean', 'prediction', 'summaries',
      'pctl-period-all.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(pred_pctl); invisible(gc())
  # Repeat FOR RURAL births
  pred_pctl_rural = natality_dt[rural == TRUE, .(
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M '),
    m_age_mean = fmean(mage),
    m_pct_white = fmean(mrace == 1),
    m_pct_black = fmean(mrace == 2),
    m_pct_amind = fmean(mrace == 3),
    m_pct_asian = fmean(mrace == 4),
    m_pct_hispanic = fmean(mhisp > 0),
    m_pct_nonhispanic = fmean(mhisp == 0),
    f_age_mean = fmean(5 * fage + 7.5),
    f_pct_white = fmean(frace == 1),
    f_pct_black = fmean(frace == 2),
    f_pct_amind = fmean(frace == 3),
    f_pct_asian = fmean(frace == 4),
    f_pct_hispanic = fmean(fhisp > 0),
    f_pct_nonhispanic = fmean(fhisp == 0),
    married_pct = fmean(mar == 1),
    order_mean = fmean(total_birth_order),
    rural_pct = fmean(rural),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), keyby = .(
    dbwt_pred_rnd_pctl_rural_pre,
    period = fcase(year < 1996, 'pre', year >= 1996, 'post')
  )]
  # Save 
  write_fst(
    x = pred_pctl_rural,
    path = here(
      'data-clean', 'prediction', 'summaries',
      'pctl-period-rural.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(pred_pctl_rural); invisible(gc())


# Summarize predictions by county ---------------------------------------------------------------
  # Summaries by county
  county_dt = natality_dt[, .(
    n = .N,
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M '),
    m_age_mean = fmean(mage),
    m_pct_white = fmean(mrace == 1),
    m_pct_black = fmean(mrace == 2),
    m_pct_amind = fmean(mrace == 3),
    m_pct_asian = fmean(mrace == 4),
    m_pct_hispanic = fmean(mhisp > 0),
    m_pct_nonhispanic = fmean(mhisp == 0),
    f_age_mean = fmean(5 * fage + 7.5),
    f_pct_white = fmean(frace == 1),
    f_pct_black = fmean(frace == 2),
    f_pct_amind = fmean(frace == 3),
    f_pct_asian = fmean(frace == 4),
    f_pct_hispanic = fmean(fhisp > 0),
    f_pct_nonhispanic = fmean(fhisp == 0),
    married_pct = fmean(mar == 1),
    order_mean = fmean(total_birth_order),
    rural_pct = fmean(rural),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), by = fips]
  # Save 
  write_fst(
    x = county_dt,
    path = here(
      'data-clean', 'prediction', 'summaries',
      'counties-all.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(county_dt); invisible(gc())


# Summarize by state ---------------------------------------------------------------------
  # Summaries by state
  natality_dt[, state := str_sub(fips, 1, 2)]
  state_dt = natality_dt[, .(
    n = .N,
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M '),
    m_age_mean = fmean(mage),
    m_pct_white = fmean(mrace == 1),
    m_pct_black = fmean(mrace == 2),
    m_pct_amind = fmean(mrace == 3),
    m_pct_asian = fmean(mrace == 4),
    m_pct_hispanic = fmean(mhisp > 0),
    m_pct_nonhispanic = fmean(mhisp == 0),
    f_age_mean = fmean(5 * fage + 7.5),
    f_pct_white = fmean(frace == 1),
    f_pct_black = fmean(frace == 2),
    f_pct_amind = fmean(frace == 3),
    f_pct_asian = fmean(frace == 4),
    f_pct_hispanic = fmean(fhisp > 0),
    f_pct_nonhispanic = fmean(fhisp == 0),
    married_pct = fmean(mar == 1),
    order_mean = fmean(total_birth_order),
    rural_pct = fmean(rural),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), by = state]
  # Save 
  write_fst(
    x = state_dt,
    path = here(
      'data-clean', 'prediction', 'summaries',
      'state-all.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(state_dt); invisible(gc())
  # Repeat for RURAL counties
  state_rural = natality_dt[rural == TRUE, .(
    n = .N,
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M '),
    m_age_mean = fmean(mage),
    m_pct_white = fmean(mrace == 1),
    m_pct_black = fmean(mrace == 2),
    m_pct_amind = fmean(mrace == 3),
    m_pct_asian = fmean(mrace == 4),
    m_pct_hispanic = fmean(mhisp > 0),
    m_pct_nonhispanic = fmean(mhisp == 0),
    f_age_mean = fmean(5 * fage + 7.5),
    f_pct_white = fmean(frace == 1),
    f_pct_black = fmean(frace == 2),
    f_pct_amind = fmean(frace == 3),
    f_pct_asian = fmean(frace == 4),
    f_pct_hispanic = fmean(fhisp > 0),
    f_pct_nonhispanic = fmean(fhisp == 0),
    married_pct = fmean(mar == 1),
    order_mean = fmean(total_birth_order),
    rural_pct = fmean(rural),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), by = state]
  # Save 
  write_fst(
    x = state_rural,
    path = here(
      'data-clean', 'prediction', 'summaries',
      'state-all.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(state_rural); invisible(gc())
