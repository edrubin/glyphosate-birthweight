# Making 2sls results 
library(pacman)
p_load(
  here, data.table, fst, fixest, ggplot2,
  stringr, qs, collapse
)

# Function to load the models -------------------------------------------------
mod_paths = 
  str_subset(
    list.files(here('data/results/micro'), full.names = TRUE),
    'est_2sls_outcome'
  ) 
mod_2sls = qread(mod_paths[1])

# Calculating 2012 mean for outcomes and GLY ----------------------------------
  # TODO: Use the main estimating table for this rather than county-level table
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
      effect_at_mean = Estimate*mean_dt[year == 2012 & variable == 'glyph_km2']$value
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
  # Getting N into millions 
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
    state_fips = 'State'
  ))
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
      '^_Local pesticides' = 'nicosulfuron_km2',
      '^_Unemployment' = 'unemployment'
    ),
    fixef.group = list(
      '-^Yr x Mo + Cnty' = 'year_month|fips',
      '-^Family Demog' = 'age|sex|race|hisp|birth|mar|educ|restatus'
    ),
    se.row = FALSE,
    fitstat = ~n_millions,
    digits.stats = 4,
    notes = "Family demographic controls include mother's age, mother's race, mother's origin, mother's education, sex of child, total birth order, mother's residence status, and birth facility. Pesticide controls include alachlor, atrazine, cyanizine, fluazifop, metolachlor, metribuzin, and nicosulfuron. GLY/$km^2$ is $kg/km^2$.",
    extralines = list(
      '__2012 mean' = effect_at_mean_dt$mean,
      '__Effect at mean' = effect_at_mean_dt$effect_at_mean
    )
  ) |> write(here('figures/micro/2sls/main-outcomes.tex'))


# Appendix tables -------------------------------------------------------------
  # Main table but with estimates of control variables included 
  # Robustness to alternative treatment defn 

  # Robustness to controls 

  # Robustness to fixed effects 

# Heterogeneity plots ---------------------------------------------------------
  # By predicted bw percentiles 

  # By month of birth 
