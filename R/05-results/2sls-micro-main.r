# Making 2sls results 
library(pacman)
p_load(
  here, data.table, fst, fixest, ggplot2,
  stringr, qs, collapse, janitor, knitr, kableExtra
)
# Setup -----------------------------------------------------------------------
  # Creating directory to put tables in 
  dir.create(here('tables/2sls/robust-cntrl'), recursive = T, showWarnings = F)
  dir.create(here('tables/ols/robust-cntrl'), recursive = T, showWarnings = F)
  dir.create(here('tables/water'), recursive = T, showWarnings = F)
  # Making a statistic of observations scaled to millions 
  fitstat_register(
    'n_millions',
    fun = \(mod){round(mod$nobs/1e6, digits = 2)},
    'N (millions)'
  )
  # Making the dictionary
  setFixest_dict(c(
    glyph_km2 = 'GLY/$km^2$',
    dbwt = 'BW',
    dbwt_pred = 'Pred BW',
    dbwt_pctl_pre = 'BW Pctl',
    i_lbw = 'LBW',
    i_vlbw = 'VLBW',
    gestation = 'Gestation',
    i_preterm = 'Preterm',
    c_section = 'C-section',
    any_anomaly = 'Anomaly',
    year = 'Year',
    state_fips = 'State',
    alachlor_km2 = 'Alachlor/$km^2$',
    atrazine_km2 = 'Atrazine/$km^2$',
    cyanazine_km2 = 'Cyanazine/$km^2$',
    fluazifop_km2 = 'Fluazifop/$km^2$',
    metolachlor_km2 = 'Metolachlor/$km^2$',
    metribuzin_km2 = 'Metribuzin/$km^2$',
    nicosulfuron_km2 = 'Nicosulfuron/$km^2$',
    unemployment_rate  = 'Unempl Rate',
    pred_ampa_in_water_lasso = 'Predicted AMPA (LASSO)',
    pred_ampa_in_water_rf = 'Predicted AMPA (RF)',
    pred_glyph_in_water_lasso = 'Predicted GLY (LASSO)',
    pred_glyph_in_water_rf = 'Predicted GLY (RF)'
  ))
  # Calculating 2012 mean for outcomes and GLY
  # TODO: Use the main est table for this rather than county-level table
  comb_cnty_health_dt  = 
    read.fst(
      'data/clean/comb-cnty-health-dt.fst',
      as.data.table = TRUE
    )
  mean_dt = 
    comb_cnty_health_dt[
      !is.na(tot_inf_births) & rural == TRUE,
      .(glyph_km2 = fmean(glyph_km2, w = tot_inf_births),
        dbwt = fmean(mean_birth_wt, w = tot_inf_births), 
        i_lbw = fmean(low_birth_wt/tot_inf_births, w = tot_inf_births),
        i_vlbw = fmean(v_low_birth_wt/tot_inf_births, w = tot_inf_births),
        gestation = fmean(mean_gest,  w = tot_inf_births),
        i_preterm = fmean(preterm/tot_inf_births, w = tot_inf_births),
        c_section = fmean(tot_c_section/tot_inf_births, w = tot_inf_births),
        any_anomaly = fmean(any_anomaly/tot_inf_births, w = tot_inf_births),
        tot_births = fsum(tot_inf_births)
      ),
      keyby = year
    ] |>
    melt(id.var = 'year')

# New Table 1: Policy and GLY effect (at mean) w/CI's -------------------------
  main_spec = 
    qread(
      list.files(here('data/results/micro'), full.names = TRUE) |>
      str_subset('est_2sls_outcome') |>
      str_subset('controls-0123_spatial-rural_het-_iv-allyielddiffpercentilegmo')
    )
  gly_mean_2012 = mean_dt[year == 2012 & variable == 'glyph_km2']$value
  # Calculating effect at GLY mean
  main_effect_at_mean_dt = 
    data.table(coeftable(
      main_spec[
        fixef = 'mage', 
        lhs = '^dbwt$|dbwt_pctl|i_lbw|i_vlbw|preterm|section|gest'
      ], 
      keep = 'glyph'
    ))[# Filtering to all controls or no controls 
      rhs == '1' | str_detect(rhs, 'nicosulfuron_km2 \\+ unemployment')
      ,.(
      i = .I,
      type = fcase(
        rhs == '1', 'policy', 
        str_detect(rhs, 'nicosulfuron_km2 \\+ unemployment'), 'gly'
      ) |> factor(levels = c('policy','gly')),
      lhs, 
      effect = fcase(
        lhs == 'dbwt', round(Estimate*gly_mean_2012,1),
        lhs == 'gestation', round(Estimate*gly_mean_2012*7,2),
        !(lhs %in% c('dbwt','gestation')), round(Estimate*gly_mean_2012*100,2)
      ),
      ci = paste0('[', 
        fcase(
          lhs == 'dbwt', round((Estimate + qnorm(0.025)*`Std. Error`)*gly_mean_2012, 1), 
          lhs == 'gestation', round((Estimate + qnorm(0.025)*`Std. Error`)*gly_mean_2012*7, 2), 
          !(lhs %in% c('dbwt','gestation')), round((Estimate + qnorm(0.025)*`Std. Error`)*gly_mean_2012*100, 2)
        ), ', ',
        fcase(
          lhs == 'dbwt', round((Estimate + qnorm(0.975)*`Std. Error`)*gly_mean_2012, 1), 
          lhs == 'gestation', round((Estimate + qnorm(0.975)*`Std. Error`)*gly_mean_2012*7, 2), 
          !(lhs %in% c('dbwt','gestation')), round((Estimate + qnorm(0.975)*`Std. Error`)*gly_mean_2012*100, 2)
        ), ']'
      )
    )] |>
    dcast(
      formula = lhs ~ type,
      value.var = c('effect','ci')
    ) |>
    merge(
      mean_dt[year == 2012,-'year'], 
      by.x = 'lhs', 
      by.y = 'variable', 
      all.x = TRUE
    )
    # Making the nice looking table 
    main_effect_at_mean_dt[,.(
      Outcome = 
        fcase(
          lhs == 'dbwt', 'Birthweight (g)', 
          lhs == 'dbwt_pctl_pre', 'Birthweight Percentile (%pt)',
          lhs == 'i_lbw', 'Low Birthweight (%pt)',
          lhs == 'i_vlbw', 'Very Low Birthweight (%pt)',
          lhs == 'gestation', 'Gestation Length (days)',
          lhs == 'i_preterm', 'Preterm (%pt)',
          lhs == 'c_section', 'C-section (%pt)'
        ) |> 
        factor(levels = c(
          'Birthweight (g)', 
          'Birthweight Percentile (%pt)',
          'Gestation Length (days)',
          'Low Birthweight (%pt)',
          'Very Low Birthweight (%pt)',
          'Preterm (%pt)',
          'C-section (%pt)'
        )),
      `Estimate` = as.character(effect_policy), 
      `Conf. Interval` = ci_policy, 
      `Estimate` = as.character(effect_gly), 
      `Conf. Interval` = ci_gly, 
      `2012 Mean` = fcase(
        lhs == 'dbwt', round(value,1),
        lhs == 'gestation', round(value*7,1),
        !(lhs %in% c('dbwt','gestation')), round(value*100,1)
      )
    )][order(Outcome)]  |>
    kbl(
      format = 'latex',
      align = 'lrrrrr'
    ) |>
    kable_classic() |>
    column_spec(1, bold = T) |>
    add_header_above(c(" " = 1, '\\textit{Policy Effect}' = 2, '\\textit{GLY Effect}' = 2, ' ' = 1)) |>
    footnote(general = 
      "All reported estimates are the effect at the weighted mean of GLY in 2012, where we weight by total births. \textit{Policy Effect} is from 2SLS regression of perinatal health on GLY, controlling for family demographics. \textit{GLY Effect} is from 2SLS regression of perinatal health on GLY, controlling for other pesticides and unemployment. See text for details. 95\\% confidence intervals calculated using standard errors clustered by year and state. Sample restricted to births occurring in rural counties or to mothers residing in rural counties. "
    )
# First the main table: Main spec with different outcomes ---------------------
  # Filtering down to all controls and fixed effects
  main_mod = main_spec[
    fixef = 'mage', 
    rhs = 'nicosulfuron_km2 \\+ unemployment|1',
    lhs = '^dbwt$|dbwt_pctl|i_lbw|i_vlbw|preterm|section|gest'
  ]
  # Calculating effect at GLY mean
  effect_at_mean_dt = 
    data.table(coeftable(main_mod, keep = 'glyph'))[,.(
      i = .I,
      lhs, 
      rhs, 
      effect_at_mean = Estimate*mean_dt[
          year == 2012 & variable == 'glyph_km2'
        ]$value
    )]
  # Merging with mean of each outcome 
  effect_at_mean_dt = 
    merge(
      effect_at_mean_dt[,-'mean'], 
      mean_dt[
        (year == 2012 & variable != 'any_anomaly') | 
        (year == 2003 & variable == 'any_anomaly'),
        .(variable, mean = value)
      ],
      by.x = 'lhs', 
      by.y = 'variable',
      all.x = TRUE
    )[order(i)]
  # Making Two tables: first is policy effect, second is gly effect
  etable(
    main_mod[rhs = '1'],
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    keep = 'GLY',
    digits = 3,
    signif.code = NA,
    group = list(
      '^_Pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE,
    notes = "Sample restricted to births occuring in rural countries or from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with year. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    extralines = list(
      '__2012 mean' = effect_at_mean_dt[rhs == '1']$mean,
      '__Effect at mean' = effect_at_mean_dt[rhs == '1']$effect_at_mean
    ),
    label = 'tab:main-outcomes-policy',
    title = '\\textbf{Policy Effect of GLY on perinatal health estimated with 2SLS.}'
  ) |> write(here('tables/2sls/main-outcomes-policy.tex'))
  # Making the table 
  etable(
    main_mod[rhs = 'nicosulfuron'],
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    keep = 'GLY',
    digits = 3,
    signif.code = NA,
    group = list(
      '^_Pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE,
    notes = "Sample restricted to births occuring in rural countries or from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with year. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    extralines = list(
      '__2012 mean' = effect_at_mean_dt[str_detect(rhs, 'nicosulfuron')]$mean,
      '__Effect at mean' = effect_at_mean_dt[str_detect(rhs, 'nicosulfuron')]$effect_at_mean
    ),
    label = 'tab:main-outcomes-gly',
    title = '\\textbf{Effect of GLY on perinatal health estimated with 2SLS.}'
  ) |> write(here('tables/2sls/main-outcomes-gly.tex'))
  # OLS version of table
  ols_spec = 
    qread(
      list.files(here('data/results/micro'), full.names = TRUE) |>
      str_subset('est_ols_outcome') |>
      str_subset('controls-0123_spatial-rural_het-predq5')
    )
  ols_mod = ols_spec[
    fixef = 'mage', 
    rhs = 'nicosulfuron_km2 \\+ unemployment|1',
    lhs = '^dbwt$|dbwt_pctl|i_lbw|i_vlbw|preterm|section|gest',
    sample = 'Full'
  ]
  etable(
    ols_mod[rhs = 'nicosulfuron'],
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    keep = 'GLY',
    digits = 3,
    signif.code = NA,
    group = list(
      '^_Pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE,
    notes = "Sample restricted to births occuring in rural countries or from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with year. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    label = 'tab:main-outcomes-ols-gly',
    title = '\\textbf{Effect of GLY on perinatal health estimated with OLS.}'
  ) |> write(here('tables/ols/main-outcomes-gly.tex'))
  etable(
    ols_mod[rhs = '1'],
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    keep = 'GLY',
    digits = 3,
    signif.code = NA,
    group = list(
      '^_Pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE,
    notes = "Sample restricted to births occuring in rural countries or from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with year. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    label = 'tab:main-outcomes-ols-policy',
    title = '\\textbf{Effect of GLY on perinatal health estimated with OLS.}'
  ) |> write(here('tables/ols/main-outcomes-policy.tex'))
  rm(ols_mod, ols_spec)


# Appendix tables -------------------------------------------------------------
  # Main table but with estimates of control variables included 
  etable(
    main_mod,
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    digits = 3,
    signif.code = NA,
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    notes = "Sample restricted to births occuring in rural countries or from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with year. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    extralines = list(
      '__2012 mean' = effect_at_mean_dt$mean,
      '__GLY/$km^2$ effect at mean' = effect_at_mean_dt$effect_at_mean
    ),
    label = 'tab:main-outcomes-controls',
    title = '\\textbf{Effect of GLY on perinatal health estimated with 2SLS.}'
  ) |> write(here('tables/2sls/main-outcomes-cntrl-coefs.tex'))
  # Main table with OLS estimates 
  

# Function to make robustness tables for each outcome -------------------------
# Make control table for one outcome 
make_outcome_control_table = function(outcome_in, mod, trt, spatial){
  # Cleaning some names 
  trt_name = fcase(
    trt == "allyielddiffpercentilegmo",
      'attainable yield percentile for GM crops',
    trt == "allyielddiffgmo500",
      'attainable yield percentile for GM crops, split at the median',
    trt == "allyielddiffpercentilegmomax",
      'maximum attainable yield percentile for GM crops',
    trt ==  "e100myielddiffpercentilegmo" ,
      'attainable yield percentile for GM crops in the eastern US',
    trt == "percentilegmacres",
      "1990-1995 GM acreage percentile"
  )
  spatial_name = fcase(
    spatial == 'rural', 
      'residing in a rural county'
  )
  outcome_name = fcase(
    outcome_in == 'dbwt', 'birthweight',
    outcome_in == 'any_anomaly', 'probability of any anomaly',
    outcome_in == 'c_section', 'probability of C-section',
    outcome_in == 'dbwt_pctl_pre', 'birthweight percentile',
    outcome_in == 'dbwt_pred', 'predicted birthweight',
    outcome_in == 'gestation', 'gestation length',
    outcome_in == 'i_lbw', 'probability of low birthweight',
    outcome_in == 'i_vlbw', 'probability of very low birthweight',
    outcome_in == 'i_preterm', 'probability of preterm birth'
  )
  # Making the table 
  etable(
    mod[lhs = paste0('^',outcome_in,'$')],
    tex = TRUE,
    depvar = FALSE, 
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    digits = 3,
    signif.code = NA,
    keep = 'GLY',
    group = list(
      '^_Pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE, 
    notes = paste0(
      "Sample restricted to births from mothers ",
      spatial_name,
      ". Instruments are the ", 
      trt_name,
      " in each county interacted with year. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$."
    ),
    label = paste0('tab:robust-cntrl-',outcome_in,'-',trt),
    title = paste0(
      '\\textbf{The effect of GLY on ',
      outcome_name,
      ', \\\\ Robustness to alternative controls and fixed effects}' 
    ),
    fontsize = 'small'
  ) |> write(here(paste0(
    'tables/2sls/robust-cntrl/',outcome_in,'-',trt,'.tex'
  )))
}

# Control tables for all outcomes of a particular model 
make_control_robust_tables = function(mod_path){
  # Load the models 
  mods = qread(mod_path)
  # Get metadata from filepath 
  trt = str_extract(mod_path,'(?<=_iv-).*(?=_cl-)')
  spatial = str_extract(mod_path,'(?<=_spatial-).*(?=_het-)')
  # Run estimation
  lapply(
    unique(coeftable(mods)$lhs),
    make_outcome_control_table,
    mod = mod,
    trt = trt,
    spatial = spatial
  )
}


# Running control tables for all estimations
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_2sls_outcome'
  ) 
lapply(
  mod_paths,  
  make_control_robust_tables
)

# Table for shift-share estimation --------------------------------------------
  main_spec_ss = qread(str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_2sls_ss'
  )[1])
  main_mod_ss = main_spec_ss[
      fixef = 'mage', 
      rhs = 'nicosulfuron_km2 \\+ unemployment|1',
      lhs = '^dbwt$|dbwt_pctl|i_lbw|i_vlbw|preterm|section|gest'
    ]
  # Calculating effect at GLY mean
  effect_at_mean_dt_ss = 
    data.table(coeftable(main_mod_ss, keep = 'glyph'))[,.(
      i = .I,
      rhs,
      lhs, 
      effect_at_mean = Estimate*mean_dt[
          year == 2012 & variable == 'glyph_km2'
        ]$value
    )]
  # Merging with mean of each outcome 
  effect_at_mean_dt_ss = 
    merge(
      effect_at_mean_dt_ss[,-'mean'], 
      mean_dt[
        (year == 2012 & variable != 'any_anomaly') | 
        (year == 2003 & variable == 'any_anomaly'),
        .(variable, mean = value)
      ],
      by.x = 'lhs', 
      by.y = 'variable',
      all.x = TRUE
    )[order(i)]
  etable(
    main_mod_ss[rhs = '1'],
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    keep = 'GLY',
    digits = 3,
    signif.code = NA,
    group = list(
      '^_Pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE,
    notes = "Sample restricted to births occuring in rural countries or from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with national GLY usage, excluding counties within 100km or upstream. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    extralines = list(
      '__2012 mean' = effect_at_mean_dt_ss[rhs == '1']$mean,
      '__Effect at mean' = effect_at_mean_dt_ss[rhs == '1']$effect_at_mean
    ),
    label = 'tab:main-outcomes-ss-policy',
    title = '\\textbf{Effect of GLY on perinatal health estimated with 2SLS shift-share instrument.}'
  ) |> write(here('tables/2sls/main-outcomes-ss-policy.tex'))
  etable(
    main_mod_ss[rhs = 'nicosulfuron'],
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    keep = 'GLY',
    digits = 3,
    signif.code = NA,
    group = list(
      '^_Pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE,
    notes = "Sample restricted to births occuring in rural countries or from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with national GLY usage, excluding counties within 100km or upstream. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    extralines = list(
      '__2012 mean' = effect_at_mean_dt_ss[str_detect(rhs, 'nicosulf')]$mean,
      '__Effect at mean' = effect_at_mean_dt_ss[str_detect(rhs, 'nicosulf')]$effect_at_mean
    ),
    label = 'tab:main-outcomes-ss-gly',
    title = '\\textbf{Effect of GLY on perinatal health estimated with 2SLS shift-share instrument.}'
  ) |> write(here('tables/2sls/main-outcomes-ss-gly.tex'))


# Table for water ml model ----------------------------------------------------
  water_ml_spec = 
    qread(
      list.files(here('data/results/micro'), full.names = TRUE) |>
      str_subset('est_water_rf-ml-pred') |>
      str_subset('controls-3_spatial-rural_het-_iv-allyielddiffpercentilegmo')
    )
# Function to make table for each outcome 
make_water_ml_table = function(outcome_in, mod){
  etable(
    mod[lhs = paste0('^',outcome_in,'$')], 
    tex = TRUE,
    keep = 'Pred',
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    digits = 3,
    signif.code = NA,
    group = list(
      '^_Local attainable yield' = 'all_yield',
      '^_Pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo' = 'year_month',
      '-^County' = 'fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE,
    label = paste0('tab:water-ml-',outcome_in)
  ) |> write(here(paste0('tables/water/water-ml-',outcome_in,'.tex')))
}

lapply(
  unique(coeftable(water_ml_spec)$lhs),  
  make_water_ml_table,
  mod = water_ml_spec
)
