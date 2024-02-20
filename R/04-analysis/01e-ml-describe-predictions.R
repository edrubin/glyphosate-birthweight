# Notes ----------------------------------------------------------------------------------
#   Goal:   Describe predictions.
#   Time:   ~9 minutes


# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(data.table, collapse, stringr, fst, magrittr, here)


# Load data ----------------------------------------------------------------------------
  # Natality data: Raw (only need a few variables)
  natality_dt = here(
    'data', 'clean', 'natality-micro.fst'
  ) %>% read_fst(
    as.data.table = TRUE,
    columns = c('dbwt', 'apgar5', 'gestation')
  )
  # Natality data: Predictions
  prediction_dt = here(
    'data', 'clean', 'prediction', 'output',
    'natality-micro-rf-train80-noindicators-2-full-cvpred.fst'
  ) %>% read_fst(
    as.data.table = TRUE
  )


# More datases ---------------------------------------------------------------------------
  # Create row ID in natality for joins (matches approach in '01a*')
  natality_dt[, row := seq_len(.N)]
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


# Add percentiles ------------------------------------------------------------------------
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


# Decode variables -----------------------------------------------------------------------
  # Recode NVSS coded variables
  prediction_dt[, `:=`(
    i_female = 1 * (sex == 'F'),
    i_male = 1 * (sex == 'M'),
    i_m_white = 1 * (mrace == 1),
    i_m_black = 1 * (mrace == 2),
    i_m_amind = 1 * (mrace == 3),
    i_m_asian = 1 * (mrace == 4),
    i_m_hispanic = 1 * (mhisp > 0),
    i_m_nonhispanic = 1 * (mhisp == 0),
    i_f_white = 1 * (frace == 1),
    i_f_black = 1 * (frace == 2),
    i_f_amind = 1 * (frace == 3),
    i_f_asian = 1 * (frace == 4),
    i_f_hispanic = 1 * (fhisp > 0),
    i_f_nonhispanic = 1 * (fhisp == 0),
    i_married = 1 * (mar == 1),
    i_birth_hospital = 1 * (birth_facility == 1),
    i_m_educ_nohs = 1 * (meduc < 3),
    i_m_educ_hs = 1 * (meduc >= 3),
    i_m_educ_bs = 1 * (meduc >= 6),
    month = month |> as.numeric()
  )]
  # Recode age
  prediction_dt[, `:=`(
    m_age = fcase(
      mage == '01', 15,
      mage %in% str_pad(2:41, 2, 'left', '0'), as.numeric(mage) + 15,
      default = NA
    ),
    f_age = fcase(
      fage == 1, 15,
      (fage %in% 2:10), (fage + 1) * 5 + 2,
      default = NA
    )
  )]
  # Drop original variables
  prediction_dt[, c(
    'sex', 'mrace', 'frace', 'mhisp', 'fhisp', 'mar', 'birth_facility',
    'meduc', 'mage', 'fage'
  ) := NULL]


# Function: Summarize variables by percentile --------------------------------------------
  # Function returns data.table with variable means by percentile and counts
  pctl_means = function(data, var) {
    require(data.table)
    require(collapse)
    # Take means of each variable by predicted percentile
    pctl_dt =  data |>
    num_vars() |>
    collapv(by = var, FUN = fmean)
    # Count of observations in each percentile
    n_dt = data[, .N, keyby = var]
    setnames(n_dt, old = 'N', new = 'n')
    # Join the two datasets
    pctl_dt |> join(
      y = n_dt,
      on = var,
      how = 'left',
      verbose = 0
    )
  }


# Summarize demographics by predicted percentile -----------------------------------------
  # Summarize by percentile of predicted birthweight, rural residence
  write_fst(
    x = pctl_means(
      data = prediction_dt,
      var = 'dbwt_pred_rnd_pctl_ruralres_pre'
    ),
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctlpred-allbirths-ecdfres.fst'
    )
  )
  # Repeat for rural occurrence
  write_fst(
    x = pctl_means(
      data = prediction_dt,
      var = 'dbwt_pred_rnd_pctl_ruralocc_pre'
    ),
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctlpred-allbirths-ecdfocc.fst'
    )
  )
  # Repeat subset of RURAL births (rural residence with rural resident ECDF)
  write_fst(
    x = pctl_means(
      data = prediction_dt[rural_res == TRUE],
      var = 'dbwt_pred_rnd_pctl_ruralres_pre'
    ),
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctlpred-ruralres-ecdfres.fst'
    )
  )
  # Now rural occurrence subset (with ECDF from rural occurrence)
  write_fst(
    x = pctl_means(
      data = prediction_dt[rural_occ == TRUE],
      var = 'dbwt_pred_rnd_pctl_ruralocc_pre'
    ),
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctlpred-ruralocc-ecdfocc.fst'
    )
  )


# Summarize demographics by actual percentile --------------------------------------------
  # Summarize by percentile of actual birthweight, rural residence
  write_fst(
    x = pctl_means(
      data = prediction_dt,
      var = 'dbwt_rnd_pctl_ruralres_pre'
    ),
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctlactual-allbirths-ecdfres.fst'
    )
  )
  # Repeat for rural occurrence
  write_fst(
    x = pctl_means(
      data = prediction_dt,
      var = 'dbwt_rnd_pctl_ruralocc_pre'
    ),
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctlactual-allbirths-ecdfocc.fst'
    )
  )
  # Repeat subset of RURAL births (rural residence with rural resident ECDF)
  write_fst(
    x = pctl_means(
      data = prediction_dt[rural_res == TRUE],
      var = 'dbwt_rnd_pctl_ruralres_pre'
    ),
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctlactual-ruralres-ecdfres.fst'
    )
  )
  # Now rural occurrence subset (with ECDF from rural occurrence)
  write_fst(
    x = pctl_means(
      data = prediction_dt[rural_occ == TRUE],
      var = 'dbwt_rnd_pctl_ruralocc_pre'
    ),
    path = here(
      'data', 'clean', 'prediction', 'summaries',
      'pctlactual-ruralocc-ecdfocc.fst'
    )
  )
