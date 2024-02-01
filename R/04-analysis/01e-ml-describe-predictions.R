# Notes ----------------------------------------------------------------------------------
#   Goal:   Describe predictions.
#   Time:   ~9 minutes


# Setup ------------------------------------------------------------------------
  # Load packages
  pacman::p_load(fastverse, fst, patchwork, parallel, magrittr, here)
  fastverse_extend(topics = c('ST', 'DT', 'VI', 'SP'))


# Load data --------------------------------------------------------------------
  # Natality data: Raw (only need a few variables)
  natality_dt = here(
    'data-clean', 'natality-micro.fst'
  ) %>% read_fst(
    as.data.table = TRUE,
    columns = c('dbwt', 'apgar5', 'gestation')
  )
  # Natality data: Predictions
  prediction_dt = here(
    'data-clean', 'natality-micro-rf-train80-noindicators-2-full-cvpred.fst'
  ) %>% read_fst(
    as.data.table = TRUE
  )


# Mere datases ---------------------------------------------------------------------------
  # Create row ID in natality for joins (matches approach in '01a*')
  natality_dt[, row := 1:.N]
  # Add predictions to the full natality dataset (using row ID)
# NOTE: Also adding true birth weight to check the merge
  setnames(natality_dt, old = 'dbwt', new = 'dbwt_check')
  setkey(prediction_dt, row)
  prediction_dt %<>% join(
    x = .,
    y = natality_dt,
    on = 'row',
    how = 'left',
    validate = '1:1'
  )
  # Check the merge
  if (prediction_dt[, fmean(dbwt == dbwt_check)] < 1) {
    stop('Bad merge between true natality and predicted birthweight.')
  } else {
    natality_dt[, dbwt_check := NULL]
  }
  # Drop natality dataset and clean up
  rm(natality_dt)
  invisible(gc())


# Add percentiles --------------------------------------------------------------
  # Add percentiles for birthweight (pre-1996, rural residence births)
  ecdf_pre_ruralres = prediction_dt[(
    rural_res == TRUE & year < 1996
  ), dbwt %>% ecdf()]
  prediction_dt[, dbwt_pctl_ruralres_pre := ecdf_pre_ruralres(dbwt)]
  # Add percentiles for birthweight (pre-1996, rural occurrence births)
  ecdf_pre_ruralocc = prediction_dt[(
    rural_occ == TRUE & year < 1996
  ), dbwt %>% ecdf()]
  prediction_dt[, dbwt_pctl_ruralocc_pre := ecdf_pre_ruralocc(dbwt)]
  # Repeat using predicted birthweights (rural residence)
  ecdf_pred_pre_ruralres = prediction_dt[(
    rural_res == TRUE & year < 1996
  ), dbwt_pred %>% ecdf()]
  prediction_dt[, dbwt_pred_pctl_ruralres_pre := ecdf_pred_pre_ruralres(dbwt_pred)]
  # Repeat using predicted birthweights (rural occurrence)
  ecdf_pred_pre_ruralocc = prediction_dt[(
    rural_occ == TRUE & year < 1996
  ), dbwt_pred %>% ecdf()]
  prediction_dt[, dbwt_pred_pctl_ruralocc_pre := ecdf_pred_pre_ruralocc(dbwt_pred)]
  # Create a rounded version of the prediction (ie, percentile bins)
  prediction_dt[, `:=`(
    dbwt_rnd_pctl_ruralres_pre = dbwt_pctl_ruralres_pre %>% round(2),
    dbwt_pred_rnd_pctl_ruralres_pre = dbwt_pred_pctl_ruralres_pre %>% round(2),
    dbwt_rnd_pctl_ruralocc_pre = dbwt_pctl_ruralocc_pre %>% round(2),
    dbwt_pred_rnd_pctl_ruralocc_pre = dbwt_pred_pctl_ruralocc_pre %>% round(2)
  )]


# Summarize predictions by predicted percentile ------------------------------------------
# TODO Add summaries of coded variables like education, age, birth facility
  # Summarize by predicted percentile
  pred_pctl_res = prediction_dt[, .(
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M'),
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
    ruralres_pct = fmean(rural_res),
    ruralocc_pct = fmean(rural_occ),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), keyby = dbwt_pred_rnd_pctl_ruralres_pre]
  # Repeat for rural occurrence
  pred_pctl_occ = prediction_dt[, .(
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M'),
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
    ruralres_pct = fmean(rural_res),
    ruralocc_pct = fmean(rural_occ),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), keyby = dbwt_pred_rnd_pctl_ruralocc_pre]
  # Save 
  write_fst(
    x = pred_pctl_res,
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctl-allbirths-ecdfres.fst'
    ),
    compress = 100
  )
  write_fst(
    x = pred_pctl_occ,
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctl-allbirths-ecdfocc.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(pred_pctl_res, pred_pctl_occ); invisible(gc())
  # Repeat subset of RURAL births (first residence, then occurrence)
# NOTE Using ECDF from rural residence to match the subset.
  pred_pctl_rural_res = prediction_dt[rural_res == TRUE, .(
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M'),
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
    ruralres_pct = fmean(rural_res),
    ruralocc_pct = fmean(rural_occ),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), keyby = dbwt_pred_rnd_pctl_ruralres_pre]
  # Now rural occurrence (with ECDF from rural occurrence)
  pred_pctl_rural_occ = prediction_dt[rural_occ == TRUE, .(
    dbwt_mean = fmean(dbwt),
    dbwt_median = fmedian(dbwt),
    dbwt_pred_mean = fmean(dbwt_pred),
    dbwt_pred_median = fmedian(dbwt_pred),
    gestation_mean = fmean(gestation),
    gestation_median = fmedian(gestation),
    apgar_mean = fmean(apgar5),
    apgar_median = fmedian(apgar5),
    female_pct = fmean(sex == 'F'),
    male_pct = fmean(sex == 'M'),
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
    ruralres_pct = fmean(rural_res),
    ruralocc_pct = fmean(rural_occ),
    month_mean = fmean(as.numeric(month)),
    month_mode = fmode(as.numeric(month)),
    year_mean = fmean(year)
  ), keyby = dbwt_pred_rnd_pctl_ruralocc_pre]
  # Save 
  write_fst(
    x = pred_pctl_rural_res,
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctl-ruralres-ecdfres.fst'
    ),
    compress = 100
  )
  write_fst(
    x = pred_pctl_rural_occ,
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctl-ruralocc-ecdfocc.fst'
    ),
    compress = 100
  )
  # Clean up
  rm(pred_pctl_rural_res, pred_pctl_rural_occ); invisible(gc())
