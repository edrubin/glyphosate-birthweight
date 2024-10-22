# Notes ----------------------------------------------------------------------------------
#   Goal:   Summarize prediction performance.
#   Time:   
#   Note:   Currently prints a data frame to screen. (Table in paper made by hand)


# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    readr, readxl, stringr, fastverse, qs, patchwork,
    rlang, fixest, splines, parallel, magrittr, here,
    fst
  )
  fastverse_extend(topics = c('ST', 'DT', 'VI'))


# Load data ------------------------------------------------------------------------------
  # Natality data: Predicted birthweight
  pred_dt = here(
    'data', 'clean', 'prediction', 'output',
    'dbwt-natality-micro-rf-train80-noindicators-2-full-cvpred.fst'
  ) %>% read_fst(
    as.data.table = TRUE,
    columns = c('dbwt', 'dbwt_pred', 'year', 'fips_res', 'rural_res')
  )


# Add birthweight percentiles ------------------------------------------------------------
  # Add percentiles for birthweight (based upon pre-1996 births)
  ecdf_pre = pred_dt[year < 1996, dbwt %>% ecdf()]
  pred_dt[, dbwt_pctl_pre := 100 * ecdf_pre(dbwt)]
  # Repeat using predicted birthweights
  ecdf_pred_pre = pred_dt[year < 1996, dbwt_pred %>% ecdf()]
  pred_dt[, dbwt_pred_pctl_pre := 100 * ecdf_pred_pre(dbwt_pred)]
  # Add quintiles and deciles (of prediction)
  pred_dt[, `:=`(
    pred_q5 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 5,
      labels = 1:5, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    pred_q10 = cut(
      x = dbwt_pred_pctl_pre,
      breaks = 10,
      labels = 1:10, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  )]
  pred_dt[, `:=`(
    true_q5 = cut(
      x = dbwt_pctl_pre,
      breaks = 5,
      labels = 1:5, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    ),
    true_q10 = cut(
      x = dbwt_pctl_pre,
      breaks = 10,
      labels = 1:10, right = FALSE, include.lowest = TRUE, ordered_result = TRUE
    )
  )]
  # Factors to integer
  pred_dt[, `:=`(
    true_q10 = true_q10 |> as.integer(),
    true_q5 = true_q5 |> as.integer(),
    pred_q10 = pred_q10 |> as.integer(),
    pred_q5 = pred_q5 |> as.integer()
  )]


# Summarize predictions ------------------------------------------------------------------
  # Full sample (rural residents only)
  perf_full = pred_dt[rural_res == TRUE, .(
    dbwt_rmse = sqrt(fmean((dbwt - dbwt_pred)^2)),
    dbwt_mae = fmean(abs(dbwt - dbwt_pred)),
    dbwt_mape = fmean(abs(dbwt - dbwt_pred) / dbwt) * 100,
    pctl_rmse = sqrt(fmean((dbwt_pctl_pre - dbwt_pred_pctl_pre)^2)),
    pctl_mae = fmean(abs(dbwt_pctl_pre - dbwt_pred_pctl_pre)),
    pctl_mape = fmean(abs(dbwt_pctl_pre - dbwt_pred_pctl_pre) / dbwt_pctl_pre) * 100,
    decl_rmse = sqrt(fmean((true_q10 - pred_q10)^2)),
    decl_mae = fmean(abs(true_q10 - pred_q10)),
    decl_mape = fmean(abs(true_q10 - pred_q10) / true_q10) * 100,
    decl_acc = fmean(true_q10 == pred_q10) * 100,
    quin_rmse = sqrt(fmean((true_q5 - pred_q5)^2)),
    quin_mae = fmean(abs(true_q5 - pred_q5)),
    quin_mape = fmean(abs(true_q5 - pred_q5) / true_q5) * 100,
    quin_acc = fmean(true_q5 == pred_q5) * 100,
    sample = 'Full'
  )]
  # Restricted to 01-99 pctl of birthweight
  perf_01_99 = pred_dt[rural_res == TRUE & between(dbwt_pctl_pre, 1, 99), .(
    dbwt_rmse = sqrt(fmean((dbwt - dbwt_pred)^2)),
    dbwt_mae = fmean(abs(dbwt - dbwt_pred)),
    dbwt_mape = fmean(abs(dbwt - dbwt_pred) / dbwt) * 100,
    pctl_rmse = sqrt(fmean((dbwt_pctl_pre - dbwt_pred_pctl_pre)^2)),
    pctl_mae = fmean(abs(dbwt_pctl_pre - dbwt_pred_pctl_pre)),
    pctl_mape = fmean(abs(dbwt_pctl_pre - dbwt_pred_pctl_pre) / dbwt_pctl_pre) * 100,
    decl_rmse = sqrt(fmean((true_q10 - pred_q10)^2)),
    decl_mae = fmean(abs(true_q10 - pred_q10)),
    decl_mape = fmean(abs(true_q10 - pred_q10) / true_q10) * 100,
    decl_acc = fmean(true_q10 == pred_q10) * 100,
    quin_rmse = sqrt(fmean((true_q5 - pred_q5)^2)),
    quin_mae = fmean(abs(true_q5 - pred_q5)),
    quin_mape = fmean(abs(true_q5 - pred_q5) / true_q5) * 100,
    quin_acc = fmean(true_q5 == pred_q5) * 100,
    sample = '01--99'
  )]
  # Restricted to 05-95 pctl of birthweight
  perf_05_95 = pred_dt[rural_res == TRUE & between(dbwt_pctl_pre, 5, 95), .(
    dbwt_rmse = sqrt(fmean((dbwt - dbwt_pred)^2)),
    dbwt_mae = fmean(abs(dbwt - dbwt_pred)),
    dbwt_mape = fmean(abs(dbwt - dbwt_pred) / dbwt) * 100,
    pctl_rmse = sqrt(fmean((dbwt_pctl_pre - dbwt_pred_pctl_pre)^2)),
    pctl_mae = fmean(abs(dbwt_pctl_pre - dbwt_pred_pctl_pre)),
    pctl_mape = fmean(abs(dbwt_pctl_pre - dbwt_pred_pctl_pre) / dbwt_pctl_pre) * 100,
    decl_rmse = sqrt(fmean((true_q10 - pred_q10)^2)),
    decl_mae = fmean(abs(true_q10 - pred_q10)),
    decl_mape = fmean(abs(true_q10 - pred_q10) / true_q10) * 100,
    decl_acc = fmean(true_q10 == pred_q10) * 100,
    quin_rmse = sqrt(fmean((true_q5 - pred_q5)^2)),
    quin_mae = fmean(abs(true_q5 - pred_q5)),
    quin_mape = fmean(abs(true_q5 - pred_q5) / true_q5) * 100,
    quin_acc = fmean(true_q5 == pred_q5) * 100,
    sample = '05--95'
  )]
  # Null model guessing means
  perf_null = pred_dt[rural_res == TRUE, .(
    dbwt_rmse = sqrt(fmean((dbwt - fmean(dbwt))^2)),
    dbwt_mae = fmean(abs(dbwt - fmean(dbwt))),
    dbwt_mape = fmean(abs(dbwt - fmean(dbwt)) / dbwt) * 100,
    pctl_rmse = sqrt(fmean((dbwt_pctl_pre - fmean(dbwt_pctl_pre))^2)),
    pctl_mae = fmean(abs(dbwt_pctl_pre - fmean(dbwt_pctl_pre))),
    pctl_mape = fmean(abs(dbwt_pctl_pre - fmean(dbwt_pctl_pre)) / dbwt_pctl_pre) * 100,
    decl_rmse = sqrt(fmean((true_q10 - 5.5)^2)),
    decl_mae = fmean(abs(true_q10 - 5.5)),
    decl_mape = fmean(abs(true_q10 - 5.5) / true_q10) * 100,
    decl_acc = fmean(true_q10 == 5) * 100,
    quin_rmse = sqrt(fmean((true_q5 - 3)^2)),
    quin_mae = fmean(abs(true_q5 - 3)),
    quin_mape = fmean(abs(true_q5 - 3) / true_q5) * 100,
    quin_acc = fmean(true_q5 == 3) * 100,
    sample = 'Null model'
  )]
  # Random guessing
  set.seed(12345)
  perf_rand = cbind(
    pred_dt[rural_res == TRUE][, .(dbwt, dbwt_pred = dbwt[sample(.N)])][, .(
      dbwt_rmse = sqrt(fmean((dbwt - dbwt_pred)^2)),
      dbwt_mae = fmean(abs(dbwt - dbwt_pred)),
      dbwt_mape = fmean(abs(dbwt - dbwt_pred) / dbwt) * 100
    )],
    CJ(x = seq(.1, 99.9, .1), y = seq(.1, 99.9, .1)) %>%
      .[, .(
        pctl_rmse = sqrt(fmean((x - y)^2)),
        pctl_mae = fmean(abs(x - y)),
        pctl_mape = fmean(abs(x - y) / x) * 100
      )],
    CJ(x = 1:10, y = 1:10) %>%
      .[, .(
        decl_rmse = sqrt(fmean((x - y)^2)),
        decl_mae = fmean(abs(x - y)),
        decl_mape = fmean(abs(x - y) / x) * 100,
        decl_acc = fmean(x == y) * 100
      )],
    CJ(x = 1:5, y = 1:5) %>%
      .[, .(
        quin_rmse = sqrt(fmean((x - y)^2)),
        quin_mae = fmean(abs(x - y)),
        quin_mape = fmean(abs(x - y) / x) * 100,
        quin_acc = fmean(x == y) * 100,
        sample = 'Random'
      )]
  )
  # Combine
  list(perf_full, perf_01_99, perf_05_95, perf_null, perf_rand) %>%
    rbindlist(fill = TRUE, use.names = TRUE) %>%
    .[, .(sample, dbwt_mae, dbwt_mape, pctl_mae, pctl_mape, decl_mae, decl_mape, quin_mae, quin_mape)] %>%
    janitor::adorn_rounding(digits = 3)

