# Notes ----------------------------------------------------------------------------------
#   Goal:   Clean data for birhweight prediction
#   Time:   ~ 15 minutes


# Data notes -----------------------------------------------------------------------------
#   - Restricting to 1992 and later (matches analysis years)


# To-do list -----------------------------------------------------------------------------
#   - Add option for removing imputation flags


# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    tidymodels, readxl, fastverse, fst, stringr,
    fixest, parallel, here
  )
  # Source settings
  source(here::here('R', '04-analysis', '01-settings.R'))


# Load data: Natality --------------------------------------------------------------------
  # Natality data
  natality_dt = here(
    'data', 'clean', 'natality-micro.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Create row ID for later joins
  natality_dt[, row := seq_len(.N)]
  # Drop variables: 'place_fips', 'city_res', 'no_city'
  natality_dt[, c('place_fips', 'city_res', 'no_city') := NULL]
# ADJUST years if desired
  # natality_dt %<>% .[year >= 1992]
  # Drop individual birth anomalies (do not want to use for prediction)
  anomalies = natality_dt |> fselect(febrile:baby_other_cong) |> names()
  natality_dt[, (anomalies) := NULL]


# Define 'rural' counties ------------------------------------------------------
# NOTE: Within county, rural is constant across years (defined in a Census year)
# NOTE: Because births have two counties (occurence and residence), we define
#       two measures of 'rural births'
  # Yield-potential treatment definitions
  comb_cnty_dt = here(
    'data', 'clean', 'comb-cnty-dt.fst'
  ) %>% read_fst(as.data.table = TRUE)
  # Change 'GEOID' to 'fips'
  setnames(comb_cnty_dt, old = 'GEOID', new = 'fips')
  # Key the county dataset
  setkey(comb_cnty_dt, fips, year)
  # Define 'rural' using residence
  setkey(natality_dt, fips_res, year, month)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips, rural_res = rural)],
    by.x = 'fips_res',
    by.y = 'fips',
    all.x = TRUE,
    all.y = FALSE
  )
  # Define 'rural' using birth occurence
  setkey(natality_dt, fips_occ, year, month)
  natality_dt %<>% merge(
    y = comb_cnty_dt[year == 1995, .(fips, rural_occ = rural)],
    by.x = 'fips_occ',
    by.y = 'fips',
    all.x = TRUE,
    all.y = FALSE
  )
  # Clean up
  rm(comb_cnty_dt)
  invisible(gc())


# Clean data -----------------------------------------------------------------------------
  # Drop health-realted variables that could be affected by GLY (bad controls)
# NOTE Dropping alcohol variable as it drops out of data starting in 2003
  to_drop = c('dbwt', 'apgar5', 'gestation', 'c_section', 'alcohol')
  # Keep the outcome variable
  to_drop = setdiff(to_drop, outcome_var)
  natality_dt[, (to_drop) := NULL]
  # Force ages to numeric
  natality_dt[, `:=`(
    mage = as.numeric(mage),
    fage = as.numeric(fage)
  )]
  # Add states
  natality_dt[, `:=`(
    state_res = str_sub(fips_res, 1, 2),
    state_occ = str_sub(fips_occ, 1, 2)
  )]
  # Drop observations missing the state of residence
  natality_dt %<>% .[state_res != 'NA']
  # Drop states outside of CONUS
  natality_dt %<>% .[!(state_res %in% c('00', '02', '15', '66', '72', '78'))]
  natality_dt %<>% .[!(state_occ %in% c('00', '02', '15', '66', '72', '78'))]
  # Now deal with missingness in the dataset...
  # If birthweight is outcome variable, drop instances with missing birthweight
  if (outcome_var == 'dbwt') {
    natality_dt %<>% .[!is.na(dbwt)]
  }
  # If gestation is outcome variable, drop instances with missing gestation
  if (outcome_var == 'gestation') {
    natality_dt %<>% .[!is.na(gestation)]
  }
  # Impute missing demographics with county-year modal value (and create flags)
  natality_dt[, `:=`(
    meduc_mode = fmode(meduc),
    mage_mode = fmode(mage),
    mhisp_mode = fmode(mhisp),
    fage_mode = fmode(fage),
    fhisp_mode = fmode(fhisp),
    frace_mode = fmode(frace),
    birth_facility_mode = fmode(birth_facility)
  ), by = .(fips_occ, year)]
  natality_dt[, `:=`(
    meduc_na = is.na(meduc),
    mage_na = is.na(mage),
    mhisp_na = is.na(mhisp),
    fage_na = is.na(fage),
    fhisp_na = is.na(fhisp),
    frace_na = is.na(frace),
    birth_facility_na = is.na(birth_facility)
  )]
  natality_dt[meduc_na == TRUE, meduc := meduc_mode]
  natality_dt[mage_na == TRUE, mage := mage_mode]
  natality_dt[mhisp_na == TRUE, mhisp := mhisp_mode]
  natality_dt[fage_na == TRUE, fage := fage_mode]
  natality_dt[fhisp_na == TRUE, fhisp := fhisp_mode]
  natality_dt[frace_na == TRUE, frace := frace_mode]
  natality_dt[birth_facility_na == TRUE, birth_facility := birth_facility_mode]
  # Repeat using all years in a county if county-year imputation is missing
  natality_dt[, `:=`(
    meduc_mode = fmode(meduc),
    mage_mode = fmode(mage),
    mhisp_mode = fmode(mhisp),
    fage_mode = fmode(fage),
    fhisp_mode = fmode(fhisp),
    frace_mode = fmode(frace),
    birth_facility_mode = fmode(birth_facility)
  ), by = .(fips_occ)]
  natality_dt[, `:=`(
    meduc_na2 = is.na(meduc),
    mage_na2 = is.na(mage),
    mhisp_na2 = is.na(mhisp),
    fage_na2 = is.na(fage),
    fhisp_na2 = is.na(fhisp),
    frace_na2 = is.na(frace),
    birth_facility_na2 = is.na(birth_facility)
  )]
  natality_dt[meduc_na2 == TRUE, meduc := meduc_mode]
  natality_dt[mage_na2 == TRUE, mage := mage_mode]
  natality_dt[mhisp_na2 == TRUE, mhisp := mhisp_mode]
  natality_dt[fage_na2 == TRUE, fage := fage_mode]
  natality_dt[fhisp_na2 == TRUE, fhisp := fhisp_mode]
  natality_dt[frace_na2 == TRUE, frace := frace_mode]
  natality_dt[birth_facility_na2 == TRUE, birth_facility := birth_facility_mode]
  # Repeat using state-year modes if county imputation is missing
  natality_dt[, `:=`(
    meduc_mode = fmode(meduc),
    mage_mode = fmode(mage),
    mhisp_mode = fmode(mhisp),
    fage_mode = fmode(fage),
    fhisp_mode = fmode(fhisp),
    frace_mode = fmode(frace),
    birth_facility_mode = fmode(birth_facility)
  ), by = .(state_occ, year)]
  natality_dt[, `:=`(
    meduc_na3 = is.na(meduc),
    mage_na3 = is.na(mage),
    mhisp_na3 = is.na(mhisp),
    fage_na3 = is.na(fage),
    fhisp_na3 = is.na(fhisp),
    frace_na3 = is.na(frace),
    birth_facility_na3 = is.na(birth_facility)
  )]
  natality_dt[meduc_na3 == TRUE, meduc := meduc_mode]
  natality_dt[mage_na3 == TRUE, mage := mage_mode]
  natality_dt[mhisp_na3 == TRUE, mhisp := mhisp_mode]
  natality_dt[fage_na3 == TRUE, fage := fage_mode]
  natality_dt[fhisp_na3 == TRUE, fhisp := fhisp_mode]
  natality_dt[frace_na3 == TRUE, frace := frace_mode]
  natality_dt[birth_facility_na3 == TRUE, birth_facility := birth_facility_mode]
  # Clean up
  natality_dt[, `:=`(
    meduc_mode = NULL,
    mage_mode = NULL,
    mhisp_mode = NULL,
    fage_mode = NULL,
    fhisp_mode = NULL,
    frace_mode = NULL,
    birth_facility_mode = NULL
  )]


# Merge: Natality and rural codes --------------------------------------------------------
# NOTE: Dropping observations whose counties don't match a rural code (<0.5%)
  # Load the rural population file
  rural_dt = here(
    'data', 'pop-area-empl', 'ruralurbancodes2003.xls'
  ) %>% read_xls()
  setDT(rural_dt)
  # Grab desired variables
  rural_dt %<>% .[, c(1, 4)]
  # Fix names
  setnames(rural_dt, c('fips', 'rural_code'))
  # First: Merge on occurence county
  natality_dt %<>% merge(
    y = rural_dt[, .(fips_occ = fips, rural_code_occ = rural_code)],
    by = 'fips_occ',
    all = FALSE,
    sort = FALSE
  )
  # Second: Merge on residence county
  natality_dt %<>% merge(
    y = rural_dt[, .(fips_res = fips, rural_code_res = rural_code)],
    by = 'fips_res',
    all = FALSE,
    sort = FALSE
  )


# Define splits for training, testing, and validation ------------------------------------
  # Remove post-1995 years (for now)
  natality_post = natality_dt[year > 1995]
  natality_pre = natality_dt[year <= 1995]
  # For now: Randomly chosen validation set
  set.seed(12345)
  natality_split = natality_pre %>% initial_split(prop = 0.8)
  # Grab the training and testing subsamples
  natality_train = natality_split %>% training()
  natality_test = natality_split %>% testing()
# NOTE Could save datasets that are not immediately necessary to free up memory


# Cleaning -------------------------------------------------------------------------------
  # Garbage control
  invisible(gc())