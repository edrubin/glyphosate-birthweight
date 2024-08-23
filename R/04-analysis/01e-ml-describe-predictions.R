# Notes ----------------------------------------------------------------------------------
#   Goal:   Describe predictions.
#   Time:   ~6 minutes


# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(data.table, collapse, stringr, fst, magrittr, here)
  # Source settings
  source(here('R', '04-analysis', '01-settings.R'))


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
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data', 'clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Subset to 2012 (for comparability)
  comb_cnty_dt %<>% .[year == 2012]
  # Grab desired variables
  comb_cnty_dt %<>% .[, .(
    fips_res = GEOID,
    glyph_km2,
    all_yield_diff_percentile_gmo,
    all_yield_diff_percentile_mze,
    all_yield_diff_percentile_soy,
    all_yield_diff_percentile_cot
  )]


# Join datasets --------------------------------------------------------------------------
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
  # Add yield potential data
  prediction_dt %<>% join(
    x = .,
    y = comb_cnty_dt,
    on = 'fips_res',
    how = 'left',
    validate = 'm:1',
    sort = FALSE
  )


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
  # Clean up
  rm(ecdf_pre_ruralres, ecdf_pre_ruralocc, ecdf_pred_pre_ruralres, ecdf_pred_pre_ruralocc)
  invisible(gc())


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
  prediction_dt[mage == 1, m_age := 15]
  prediction_dt[fage == 1, f_age := 15]
  prediction_dt[mage > 1, m_age := mage + 13]
  prediction_dt[fage > 1, f_age := (fage + 1) * 5 + 3]
  # Drop original variables
  prediction_dt[, c(
    'sex', 'mrace', 'frace', 'mhisp', 'fhisp', 'mar', 'birth_facility',
    'meduc', 'mage', 'fage'
  ) := NULL]
  # Drop unwanted variables
  prediction_dt[, c('.row', '.row_check', 'dbwt_check') := NULL]


# Functions: Summarize variables by percentile -------------------------------------------
  # Function that splits the data and saves files
  master_means = function(data, var, sub_space = NA, sub_time = NA, save_dir) {
    # Helpfer function: returns data.table with variable means by percentile and counts
    pctl_means = function(data, var, sub_space = NA, sub_time = NA) {
      require(data.table)
      require(collapse)
      require(magrittr)
      # Take spatial subset (if non-na)
      if (!is.na(sub_space)) data = data[get(sub_space)]
      # Take temporal subset (if non-na)
      if (!is.na(sub_time)) {
        if (sub_time == 'pre') data = data[year < 1996]
        if (sub_time == 'post') data = data[year >= 1996]
      }
      # Take means of each variable by predicted percentile
      pctl_dt =
        data |>
        num_vars() |>
        collapv(by = var, FUN = fmean)
      # Count of observations in each percentile
      n_dt = data[, .N, keyby = var]
      setnames(n_dt, old = 'N', new = 'n')
      # Join the two datasets
      pctl_dt %<>% join(
        y = n_dt,
        on = var,
        how = 'left',
        verbose = 0
      )
      # Change name of percentile variable
      setnames(pctl_dt, old = var, new = 'pctl')
      # Return the data.table
      return(pctl_dt)
    }
    require(data.table)
    require(stringr)
    require(fst)
    require(here)
    # Split the data
    pctl_dt = pctl_means(data, var, sub_space, sub_time)
    # Grab name components based upon 'var'
    name_parts = var |> str_split('_') |> unlist()
    bwt_type = ifelse(str_detect(var, 'pred'), 'pctlpred', 'pctlactual')
    pop_sum = ifelse(is.na(sub_space), 'allbirths', sub_space) |> str_remove('_')
    pop_ecdf = name_parts |> tail(2) |> head(1) |> str_replace('rural', 'ecdf')
    name_time = ifelse(is.na(sub_time), 'alltime', sub_time)
    # Build filename
    filename = paste(
      bwt_type, pop_sum, pop_ecdf, name_time, sep = '-'
    ) |> paste0('.fst')
    # Save the data
    write_fst(
      x = pctl_dt,
      path = here(save_dir, filename),
      compress = 100
    )
  }


# Summarize demographics by predicted percentile -----------------------------------------
  # All time-space combinations
  time_space = CJ(
    sub_time = c(NA, 'pre', 'post'),
    sub_space = c(NA, 'rural_res')
  )
  # Run function based upon rural-residence ECDF
  for (i in seq_len(nrow(time_space))) {
    master_means(
      data = prediction_dt,
      var = 'dbwt_pred_rnd_pctl_ruralres_pre',
      sub_space = time_space$sub_space[i],
      sub_time = time_space$sub_time[i],
      save_dir = here('data', 'clean', 'prediction', 'summaries')
    )
  }
  # Repeat for actual birthweight (still rural-residence ECDF)
  for (i in seq_len(nrow(time_space))) {
    master_means(
      data = prediction_dt,
      var = 'dbwt_rnd_pctl_ruralres_pre',
      sub_space = time_space$sub_space[i],
      sub_time = time_space$sub_time[i],
      save_dir = here('data', 'clean', 'prediction', 'summaries')
    )
  }
  # Repeat only for rural occurrence ECDF and subset
  master_means(
    data = prediction_dt,
    var = 'dbwt_pred_rnd_pctl_ruralocc_pre',
    sub_space = 'rural_occ',
    sub_time = NA,
    save_dir = here('data', 'clean', 'prediction', 'summaries')
  )
