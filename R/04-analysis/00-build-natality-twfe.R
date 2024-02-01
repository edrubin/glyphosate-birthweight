
# Notes ------------------------------------------------------------------------
#   Goal:   Build cleaned natality dataset for DID/TWFE analysis.
#   Time:   10-15 minutes


# Setup ------------------------------------------------------------------------
  # Load packages
  pacman::p_load(fastverse, patchwork, parallel, magrittr, here)
  fastverse_extend(topics = c('ST'))


# Load data --------------------------------------------------------------------
  # Define desired years
# ADJUST Choose desired years
  yrs = 1990:2013
  # Natality data
  threads_fst(1)
  natality_dt = 
    mclapply(
      X = yrs,
      FUN = function(yr) {
        # Load the dataset
        read_fst(
          here(paste0("data/health-restricted/period-clean/natality-",yr,".fst")),
          as.data.table = TRUE
        )
      },
      mc.cores = length(yrs)
    ) %>% rbindlist(use.names = TRUE, fill = TRUE)
  threads_fst(parallel::detectCores())


# Clean natality data ----------------------------------------------------------
  # # Load natality data dictionary
  # load(here('data', 'health-restricted', 'natality-dct.RData'))
  # # Define a variable to check out
  # var = 'total_birth_order'
  # # Find all codes
  # natality_dct[var_name == var, var_codes] %>% unique()
  # # Determine if there are any missing codes
  # natality_dt[, fmean(live_birth_order == 9), year] %>% plot()
  # Recode mother's age for consistency
  natality_dt[mage %in% as.character(37:41), mage := '36']
  # Re-code missing values
  natality_dt[gestation == 99, gestation := NA]
  natality_dt[mhisp == 9, mhisp := NA]
  natality_dt[meduc == '9', meduc := NA]
  natality_dt[fage == '11', fage := NA]
  natality_dt[frace == '9', frace := NA]
  natality_dt[fhisp == 9, fhisp := NA]
  natality_dt[birth_facility == 9, birth_facility := NA]
  natality_dt[live_birth_order == 9, birth_facility := NA]
  natality_dt[total_birth_order == 9, birth_facility := NA]
  # Change names of county fips
  setnames(
    natality_dt,
    old = c('GEOID', 'GEOID_occ'),
    new = c('fips_res', 'fips_occ')
  )


# Save data --------------------------------------------------------------------
  # Save the natality dataset
  write_fst(
    x = natality_dt,
    path = here(
      'data', 'clean',
      'natality-micro.fst'
    ),
    compress = 100
  )

