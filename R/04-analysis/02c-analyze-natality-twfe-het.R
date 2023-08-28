# Notes ------------------------------------------------------------------------
#   Goal:   Heterogeneity and stuff
#   Time:   


# Setup ------------------------------------------------------------------------
  # Load packages
  pacman::p_load(fastverse, extrafont, fixest, parallel, here)
  fastverse_extend(topics = c('ST', 'VI'))


# Load data --------------------------------------------------------------------
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data-clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Natality data
  natality_dt = here(
    'data-clean', 'natality-micro.fst'
  ) %>% read_fst(as.data.table = TRUE)


# Key datasets -----------------------------------------------------------------
  # Change names for merge
  setnames(comb_cnty_dt, old = 'GEOID', new = 'fips')
  setnames(natality_dt, old = 'fips_occ', new = 'fips')
  natality_dt[, fips_res := NULL]
  # Key datasets
  setkey(natality_dt, fips, year, month)
  setkey(comb_cnty_dt, fips, year)


# Build analysis dataset -----------------------------------------------------------------
  # Define demographic and mother-related controls
  dem_controls = c(
    'sex', 'mage', 'mrace', 'mhisp', 'meduc', 'mar',
    'birth_facility', 'restatus', 'total_birth_order'
  )
  # Father-related controls
  f_controls = c('fage', 'fhisp', 'frace')
  # Define pesticide controls
  pest_controls = c(
    'alachlor_km2', 'atrazine_km2', 'cyanazine_km2', 'fluazifop_km2',
    'metolachlor_km2','metribuzin_km2','nicosulfuron_km2'
  )
  # Collect glyphosate variables 
  glyph_vars = c(
    'glyph_km2',
    names(comb_cnty_dt) |> 
      str_subset('high_kls_ppt_growing_glyph') |> 
      str_subset('local', negate = TRUE)
  )
  # Collect IV vars
  iv_vars =  c(
    'e100m_yield_diff_gmo_50_0',
    'e100m_yield_diff_gmo_50_10',
    'e100m_yield_diff_gmo_40_0',
    'e100m_yield_diff_gmo_40_10',
    'e100m_yield_diff_gmo_60_0',
    'e100m_yield_diff_gmo_60_10'
  )
  # Merge the datasets with the requested variables
  est_dt = merge(
    x = natality_dt,
    y = comb_cnty_dt[, c(
      'fips', 'year', 'rural',
      glyph_vars,
      iv_vars,
      pest_controls,
      'unemployment_rate'
    ), with = FALSE],
    by = c('fips', 'year'),
    all = FALSE
  )

  est_controls = feols(
    gestation ~
    1 + i(year, e100m_yield_diff_gmo_50_0, ref = 1995) |
    csw0(
      month,
      sex + mage + mrace + mhisp + meduc + mar,
      birth_facility + restatus,
      total_birth_order
    ) +
    year + fips,
    cluster = ~year + fips,
    data = est_dt[rural == TRUE],
    lean = TRUE
  )
    
  est_list = feols(
    c(glyph_km2, dbwt, dbwt <= 2500, dbwt <= 1500, gestation, c_section) ~
    1 + i(year, e100m_yield_diff_gmo_50_0, ref = 1995) |
    sw0(month + sex + mage + mrace + mhisp + meduc + mar) +
    year + fips,
    cluster = ~year + fips,
    data = est_dt[rural == TRUE],
    lean = TRUE
  )
  
  pctl_dbwt = lapply(
    X = seq(0.05, 0.95, 0.05),
    FUN = function(p) est_dt[rural == TRUE & year < 1995, fnth(dbwt, p)]
  ) %>% unlist()

  pctl_formula = paste0(
    'c(',
    paste0('dbwt <= ', pctl_dbwt, collapse = ', '),
    ')',
    ' ~ ',
    '1 + i(year, e100m_yield_diff_gmo_50_0, ref = 1995) |',
    'sex + mage + mrace + mhisp + meduc + mar +',
    'month + year + fips'
  ) %>% as.formula()

  est_pctl = feols(
    fml = pctl_formula,
    cluster = ~year + fips,
    data = est_dt[rural == TRUE],
    lean = TRUE
  )

  for (i in seq_along(pctl_dbwt)) est_pctl[i] |> iplot()
  data.table(p = seq(0.05, 0.95, 0.05), dbwt = pctl_dbwt)


  # Repeat with mutually exclusive decile bins
  deciles_dbwt = lapply(
    X = seq(0.1, 0.9, 0.1),
    FUN = function(p) est_dt[rural == TRUE & year < 1995, fnth(dbwt, p)]
  ) %>% unlist()
  deciles_dbwt = c(0, deciles_dbwt, Inf)

  decile_formula = paste0(
    'c(',
    lapply(
      X = 1:(length(deciles_dbwt)-1),
      FUN = function(i) {
        paste0('1 * between(dbwt, ', deciles_dbwt[i], ', ', deciles_dbwt[i+1]-1, ')')
      }
    ) %>% unlist() %>% paste0(collapse = ', '),
    ')',
    ' ~ ',
    '1 + i(year, e100m_yield_diff_gmo_50_0, ref = 1995) |',
    'sex + mage + mrace + mhisp + meduc + mar +',
    'month + year + fips'
  ) %>% as.formula()

  est_decile = feols(
    fml = decile_formula,
    cluster = ~year + fips,
    data = est_dt[rural == TRUE],
    lean = TRUE
  )

  for (i in 1:10) est_decile[i] |> iplot()
  data.table(p = seq(0, 1, 0.1), dbwt = deciles_dbwt)



  # Repeat with mutually exclusive quartile bins
  quartiles_dbwt = lapply(
    X = seq(0.25, 0.75, 0.25),
    FUN = function(p) est_dt[rural == TRUE & year < 1995, fnth(dbwt, p)]
  ) %>% unlist()
  quartiles_dbwt = c(0, quartiles_dbwt, Inf)

  quartile_formula = paste0(
    'c(',
    lapply(
      X = 1:(length(quartiles_dbwt)-1),
      FUN = function(i) {
        paste0('1 * between(dbwt, ', quartiles_dbwt[i], ', ', quartiles_dbwt[i+1]-1, ')')
      }
    ) %>% unlist() %>% paste0(collapse = ', '),
    ')',
    ' ~ ',
    '1 + i(year, e100m_yield_diff_gmo_50_0, ref = 1995) |',
    'sex + mage + mrace + mhisp + meduc + mar +',
    'month + year + fips'
  ) %>% as.formula()

  est_quartile = feols(
    fml = quartile_formula,
    cluster = ~year + fips,
    data = est_dt[rural == TRUE],
    lean = TRUE
  )

  for (i in 1:10) est_quartile[i] |> iplot()
  data.table(p = seq(0, 1, 0.25), dbwt = quartiles_dbwt)

  # Group education levels
  est_dt[, `:=`(
    meduc_grp = fcase(
      meduc %in% 1:2, 'a: DNF HS',
      meduc == 3, 'b: HS',
      meduc == 4, 'c: DNF Coll.',
      meduc >= 5, 'd: Coll. Deg.'
    )
  )]
  est_meduc = feols(
    dbwt ~
    1 + i(year, e100m_yield_diff_gmo_50_0, ref = 1995) |
    sex + mage + mrace + mhisp + mar +
    month + year + fips,
    cluster = ~year + fips,
    data = est_dt[rural == TRUE],
    fsplit = ~ meduc_grp,
    lean = TRUE
  )
  for (i in 1:5) est_meduc[i] |> iplot()

  # Effect on percentile
  # Calculate birthweight percentile in distribution of rural, pre-1996 births
  ecdf_pre_rural = est_dt[rural == TRUE & year < 1996, dbwt %>% ecdf()]
  est_dt[, dbwt_pctl_rural_pre := ecdf_pre_rural(dbwt)]
  # Estimate with various controls
  est_dbwt_pctl = feols(
    100 * dbwt_pctl_rural_pre ~
    1 + i(year, e100m_yield_diff_gmo_50_0, ref = 1995) + mage + fage |
    csw0(
      sex + mrace + mhisp + mar,
      fhisp + frace
    ) +
    month + year + fips,
    cluster = ~year + fips,
    data = est_dt[rural == TRUE],
    lean = TRUE
  )
  est_dbwt_pctl_pesticides = feols(
    100 * dbwt_pctl_rural_pre ~
    1 + i(year, e100m_yield_diff_gmo_50_0, ref = 1995) +
      mage + fage + unemployment_rate + 
      alachlor_km2 + atrazine_km2 + cyanazine_km2 + fluazifop_km2 +
      metolachlor_km2 + metribuzin_km2 + nicosulfuron_km2 |
    sex + mrace + mhisp + mar +
      fhisp + frace +
    month + year + fips,
    cluster = ~year + fips,
    data = est_dt[rural == TRUE],
    lean = TRUE
  )

  for (i in 1:3) iplot(est_dbwt_pctl[i])
  est_dbwt_pctl_pesticides %>% iplot()
