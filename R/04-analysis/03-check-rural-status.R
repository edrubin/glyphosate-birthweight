# Notes ----------------------------------------------------------------------------------
#   Goal:   Figure out FIPS occurence vs. residential splits


# Setup ------------------------------------------------------------------------
  # Load packages
  pacman::p_load(fastverse, qs, patchwork, fixest, parallel, magrittr, here)
  fastverse_extend(topics = c('ST', 'DT', 'VI'))


# Load data --------------------------------------------------------------------
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data', 'clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE, columns = c('GEOID', 'year', 'rural'))
  # Natality data: Raw
  natality_dt = here(
    'data', 'clean', 'natality-micro.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Drop years prior to 1992
  natality_dt %<>% .[year >= 1992]



# Merge rural status to natality data ----------------------------------------------------
# NOTE: Setting 'rural' to a constant (1995 value)
  # First define based upon residence
  setkey(natality_dt, fips_res)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips_res = GEOID, rural_res = rural)],
    by = 'fips_res',
    all.x = TRUE,
    all.y = FALSE
  )
  # Now define based upon occurrence
  setkey(natality_dt, fips_occ)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips_occ = GEOID, rural_occ = rural)],
    by = 'fips_occ',
    all.x = TRUE,
    all.y = FALSE
  )
  # Add states
  natality_dt[, `:=`(
    state_res = fips_res |> str_sub(1, 2),
    state_occ = fips_occ |> str_sub(1, 2)
  )]
  # Add county-only fips (ignoring state prefix)
  natality_dt[, `:=`(
    co_res = fips_res |> str_sub(3, 5),
    co_occ = fips_occ |> str_sub(3, 5)
  )]
  # Flag states inside of CONUS
  states_conus = maps::state.fips$fips |> str_pad(2, 'left', 0)
  natality_dt[, `:=`(
    in_conus_res = state_res %in% states_conus,
    in_conus_occ = state_occ %in% states_conus
  )]
  natality_dt[, in_conus_both := in_conus_res & in_conus_occ]


# Compare --------------------------------------------------------------------------------
  # First: Are they all within CONUS?
  natality_dt[,.(in_conus_res, in_conus_occ)] |>
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
  # How often do states match (and how after are they missing?)
  natality_dt[, .(state_res == state_occ)] |> 
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
  natality_dt[in_conus_both == TRUE, .(state_res == state_occ)] |>
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
  # How often do counties match (and how often are they missing)
  natality_dt[, .(fips_res == fips_occ)] |> 
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
  natality_dt[in_conus_both == TRUE, .(fips_res == fips_occ)] |>
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
  # Is there some sort of censoring going on?
  natality_dt[co_res == '000']
  natality_dt[co_res == '000', state_res] |> qtab()
  # How consistent is the rural designation?
  natality_dt[, .(rural_res, rural_occ)] |> 
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
  natality_dt[in_conus_both == TRUE, .(rural_res, rural_occ)] |> 
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
  natality_dt[in_conus_both == TRUE & year == 1995, .(rural_res, rural_occ)] |> 
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
  natality_dt[in_conus_both == TRUE & year == 2013, .(rural_res, rural_occ)] |> 
    qtab(na.exclude = FALSE) |> prop.table() |> round(5) |> multiply_by(100)
