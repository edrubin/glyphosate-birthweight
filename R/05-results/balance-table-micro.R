# Notes ----------------------------------------------------------------------------------
#   Goal:   Build a summary table for the appendix.
#   Time:   ???


# Todo list ------------------------------------------------------------------------------


# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    readr, readxl, stringr, fastverse, qs, patchwork,
    rlang, fixest, splines, parallel, magrittr, here,
    fst
  )
  fastverse_extend(topics = c('ST', 'DT', 'VI'))


# Load data ------------------------------------------------------------------------------
# NOTE Using analysis script to build dataset
  # Read the analysis script
  load_scr = here('R', '04-analysis', '02b-analyze-natality-twfe.R') |> readLines()
  # Find the lines to start and stop
  a = str_detect(load_scr, '^# Load data -') |> which() |> head(1)
  b = str_detect(load_scr, '# Function: Water analysis -') |> which() |> head(1)
  # Now load the data by running the identified lines
  source(textConnection(object = load_scr[a:(b-1)]))


# Define variables of interest -----------------------------------------------------------
  # Define desired variables
  outcome_vars = c(
    'dbwt',
    # 'dbwt_pred',
    # 'dbwt_pctl_pre',
    'i_lbw',
    'i_vlbw',
    'gestation',
    'i_preterm',
    'c_section',
    # 'any_anomaly',
    'index'
  )
  dem_fes = c(
    'sex',
    # 'mage',
    # 'mrace',
    # 'mhisp',
    # 'meduc',
    # 'mar',
    # 'birth_facility',
    # 'restatus',
    # 'total_birth_order',
    NULL
  )
  # dad_fes = c('fage', 'fhisp', 'frace')
  # pest_controls = c(
  #   'alachlor_km2', 'atrazine_km2', 'cyanazine_km2', 'fluazifop_km2',
  #   'metolachlor_km2', 'metribuzin_km2', 'nicosulfuron_km2'
  # )
  econ_controls = c(
    'unempl_rate',
    'pct_farm_empl',
    'farm_empl_per_cap',
    'tot_pop',
    'inc_per_cap_farm',
    'inc_per_cap_nonfarm',
    'empl_rate',
    'farm_empl_per_cap'
  )
  # fert_controls = c(
  #   'p_commercial_km2', 'n_commercial_km2',
  #   'p_farm_km2', 'n_farm_km2',
  #   'p_nonfarm_km2', 'n_nonfarm_km2',
  #   'p_manure_km2', 'n_manure_km2'
  # )
  acre_controls = c(
    'corn_acres_km2',
    'soy_acres_km2',
    'cotton_acres_km2',
    'other_acres_km2'
  )
  race_controls = c(
    'shr_raceblack_all',
    'shr_racewhite_all',
    'shr_hispanic_all'
  )
  glyph_vars = c(
    'glyph_km2'
  )
  iv_vars = 'all_yield_diff_percentile_gmo_max'
  # control_sets = c(
  #   'pest',
  #   'unempl_rate', 'empl_rate', 'pct_farm_empl', 'farm_empl_per_cap',
  #   'inc_per_cap_farm', 'inc_per_cap_nonfarm',
  #   'pop_all',
  #   'age_share', 'race_share',
  #   'fert'
  # )
  # het_vars = c(
  #   'dbwt',
  #   'dbwt_pred',
  #   'dbwt_pctl_pre',
  #   'dbwt_pred_pctl_pre'
  # )
  mom_vars = c(
    'i_female',
    'i_m_black',
    'i_m_nonwhite',
    'i_m_hispanic',
    'i_m_married',
    'i_m_hs',
    'i_m_college'
  )


# Build dataset --------------------------------------------------------------------------
  # Create summary dataset from loaded datasets
  est_dt = merge(
    x = natality_dt[, unique(c(
      'year',
      'fips_res',
      # 'fips_occ', 'fips_res', 'month', 'year_month',
      # 'rural_grp',
      # 'rural_occ',
      'rural_res',
      mom_vars,
      outcome_vars,
      # het_vars,
      dem_fes
      # dad_fes
    )), with = FALSE],
    y = comb_cnty_dt[, unique(c(
      'fips', 'year',
      glyph_vars,
      iv_vars,
      # fe_vars,
      # pest_controls,
      econ_controls,
      # fert_controls,
      acre_controls
    )), with = FALSE],
    by.x = c('fips_res', 'year'),
    by.y = c('fips', 'year'),
    all = FALSE
  )
  est_dt %<>% .[year < 1996]
  # Create our groups: rural above/below median suitability and urban
  est_dt[, grp := fcase(
    rural_res == TRUE & all_yield_diff_percentile_gmo_max > .5, 1,
    rural_res == TRUE & all_yield_diff_percentile_gmo_max <= .5, 2,
    default = 3
  )]
  # C-section to numeric
  est_dt[, c_section := 1 * c_section]


# Create summary table -------------------------------------------------------------------
  # Summarize
  sum_l =
    est_dt |>
    fselect(-year, -fips_res, -rural_res, -sex)|>
    qsu(by = ~grp)
  sum_dt =
    sum_l |>
    as.data.frame() |>
    fselect(-Min, -Max, -N) |>
    as.data.table() |>
    dcast(Variable ~ Group, value.var = c('Mean', 'SD'))
  # Rearrange columns
  setcolorder(sum_dt, c('Variable', 'Mean_1', 'SD_1', 'Mean_2', 'SD_2', 'Mean_3'))
  # Enforce some rounding
  cbind(sum_dt[,1], sum_dt[,-1] |> round(3))
  # Additional summary for totals
  est_dt[, .(.N, n_cnty = fndistinct(fips_res)), by = grp]
