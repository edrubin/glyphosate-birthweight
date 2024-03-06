# Making 2sls results 
library(pacman)
p_load(
  here, data.table, fst, fixest, ggplot2,
  stringr, qs, collapse, janitor
)
# Setup -----------------------------------------------------------------------
  # Creating directory to put tables in 
  dir.create(here('tables/2sls/robust-cntrl'), recursive = T, showWarnings = F)
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
    dbwt_pred = 'Predicted BW',
    dbwt_pctl_pre = 'BW Pctl',
    i_lbw = 'Low BW',
    i_vlbw = 'Very Low BW',
    gestation = 'Gestation',
    i_preterm = 'Preterm',
    c_section = 'C-section',
    any_anomaly = 'Any Anomaly',
    year = 'Year',
    state_fips = 'State',
    alachlor_km2 = 'Alachlor/$km^2$',
    atrazine_km2 = 'Atrazine/$km^2$',
    cyanazine_km2 = 'Cyanazine/$km^2$',
    fluazifop_km2 = 'Fluazifop/$km^2$',
    metolachlor_km2 = 'Metolachlor/$km^2$',
    metribuzin_km2 = 'Metribuzin/$km^2$',
    nicosulfuron_km2 = 'Nicosulfuron/$km^2$',
    unemployment_rate  = 'Unempl Rate'
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

# First the main table: Main spec with different outcomes ---------------------
  main_spec = 
    qread(str_subset(
      list.files(here('data/results/micro'), full.names = TRUE), 
      'controls-0123_spatial-rural_het-_iv-allyielddiffpercentilegmo'
    ))
  main_spec_ols = 
    qread(str_subset(
      mod_paths, 
      'controls-0123_spatial-rural_het-_iv-allyielddiffpercentilegmo'
    ))
  # Filtering down to all controls and fixed effects
  main_mod = main_spec[
    fixef = 'mage', 
    rhs = 'nicosulfuron_km2 \\+ unemployment',
    lhs = '^dbwt$|dbwt_pctl|i_lbw|i_vlbw|preterm|section|gest'
  ]
  # Calculating effect at GLY mean
  effect_at_mean_dt = 
    data.table(coeftable(main_mod, keep = 'glyph'))[,.(
      i = .I,
      lhs, 
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
  
  # Making the table 
  etable(
    main_mod,
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var',
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
      '^_Local pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo + Cnty' = 'year_month|fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    tpt = TRUE,
    notes = "Sample restricted to births from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with year. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    extralines = list(
      '__2012 mean' = effect_at_mean_dt$mean,
      '__Effect at mean' = effect_at_mean_dt$effect_at_mean
    )
  ) |> write(here('tables/2sls/main-outcomes.tex'))

# Appendix tables -------------------------------------------------------------
  # Main table but with estimates of control variables included 
  etable(
    main_mod,
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    se.below = TRUE,
    digits = 3,
    signif.code = NA,
    fixef.group = list(
      '-^Yr x Mo + Cnty' = 'year_month|fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    notes = "Sample restricted to births from mothers residing in rural counties. Instruments are the attainable yield percentile for GM crops in each county interacted with year. Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    extralines = list(
      '__2012 mean' = effect_at_mean_dt$mean,
      '__GLY/$km^2$ effect at mean' = effect_at_mean_dt$effect_at_mean
    )
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
      depvar.title = 'Dep Var',
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
      '^_Local pesticides' = 'Nicosulfuron',
      '^_Unemployment' = 'Unemp'
    ),
    fixef.group = list(
      '-^Yr x Mo + Cnty' = 'year_month|fips',
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
      '\\textbf{The effect of glyphosate on ',
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