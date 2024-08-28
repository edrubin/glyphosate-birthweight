# Notes ----------------------------------------------------------------------------------
#   Goal:   Process raw SEER data for population-share controls.
#   Time:   ~ 2 minutes


# Setup ----------------------------------------------------------------------------------
  # Load packages
  library(pacman)
  p_load(data.table, collapse, iotools, fst, stringr, magrittr, here)


# Data notes -----------------------------------------------------------------------------
#  - Explanation of "continuity" of FIPS codes in SEER data:
#    https://seer.cancer.gov/seerstat/variables/countyattribs/ruralurban.html


# Loop over start years for dataset
#   - 1990: includes ethnicity (Hispanic origin) and 2 more race categories (not used);
#   - 1969: longer time series.
for (data_year in c(1990, 1969)) {


# Load data ------------------------------------------------------------------------------
  # Abbreviations for column types
  chr = 'character'
  num = 'numeric'
  int = 'integer'
  # Read the raw (fixed-width) SEER data
  seer_dt =
    here(
      'data', 'download-manual',
      paste0('us.', data_year, '_2022.singleages.adjusted.txt')
    ) |>
    input.file(
      formatter = dstrfw,
      col_types = c(num, rep(chr, 3), rep(num, 6)),
      widths = c(
        4, 2, 2, 3, 2, 1, 1, 1, 2, 8
      )
    )
  # To data table
  setDT(seer_dt)
  # Add names
  setnames(
    seer_dt,
    c('yr', 'st', 'fips_st', 'fips_co', 'registry', 'race', 'origin', 'sex', 'age', 'pop')
  )


# Clean data -----------------------------------------------------------------------------
  # Drop unwanted columns
  seer_dt[, c('registry') := NULL]
  # Drop Alaska and Hawaii
  seer_dt %<>% .[!(st %in% c('AK', 'HI'))]
  # Drop Katrina-affected population
  seer_dt %<>% .[st != 'KR']
  # Create FIPS
  seer_dt[, fips := paste0(fips_st, fips_co)]
  # Drop more unwanted columns
  seer_dt[, c('st', 'fips_st', 'fips_co') := NULL]


# Aggregate population by county-year ----------------------------------------------------
  # 1. Total population in a county-year
  pop_all = collap(
    X = seer_dt,
    by = ~ yr + fips,
    custom = list(fsum = c(pop_all = 'pop'))
  )
  # 2. Female population in a county-year
  pop_female = collap(
    X = seer_dt[sex == 2],
    by = ~ yr + fips,
    custom = list(fsum = c(pop_female = 'pop'))
  )
  # 3. Male population in a county-year
  pop_male = collap(
    X = seer_dt[sex == 1],
    by = ~ yr + fips,
    custom = list(fsum = c(pop_male = 'pop'))
  )


# Aggregate by age groups ----------------------------------------------------------------
  # Define decadal age groups
  seer_dt[, age_group := age %/% 10 * 10]
  # Combine the 80+ group with the 70-79 group
  seer_dt[age_group == 80, age_group := 70]
  # 1: Total population by year, county, and age group
  age_all = collap(
    X = seer_dt,
    by = ~ yr + fips + age_group,
    custom = list(fsum = c(age_all = 'pop'))
  )
  # 2: Female population by year, county, and age group
  age_female = collap(
    X = seer_dt[sex == 2],
    by = ~ yr + fips + age_group,
    custom = list(fsum = c(age_female = 'pop'))
  )
  # 3: Male population by year, county, and age group
  age_male = collap(
    X = seer_dt[sex == 1],
    by = ~ yr + fips + age_group,
    custom = list(fsum = c(age_male = 'pop'))
  )


# Calculate county-year age shares -------------------------------------------------------
  #   a. merge age-group datasets with relevant population datasets;
  #   b. calculate the shares
  #   c. drop unwanted columns and pivot to wide
  #   d. rename columns
  # Full population (across sexes)
  shr_age_all =
    age_all |>
    merge(y = pop_all, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_age_all = age_all / pop_all) |>
    fselect(-pop_all, -age_all) |>
    pivot(how = 'wider', ids = c('yr', 'fips'), names = 'age_group', drop = FALSE)
  setnames(
    shr_age_all,
    c('yr', 'fips',
      paste0('shr_age_', shr_age_all %>% names() %>% .[3:10], '_all')
    )
  )
  # Female population
  shr_age_female =
    age_female |>
    merge(y = pop_female, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_age_female = age_female / pop_female) |>
    fselect(-pop_female, -age_female) |>
    pivot(how = 'wider', ids = c('yr', 'fips'), names = 'age_group', drop = FALSE)
  setnames(
    shr_age_female,
    c('yr', 'fips',
      paste0('shr_age_', shr_age_female %>% names() %>% .[3:10], '_female')
    )
  )
  # Male population
  shr_age_male =
    age_male |>
    merge(y = pop_male, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_age_male = age_male / pop_male) |>
    fselect(-pop_male, -age_male) |>
    pivot(how = 'wider', ids = c('yr', 'fips'), names = 'age_group', drop = FALSE)
  setnames(
    shr_age_male,
    c('yr', 'fips',
      paste0('shr_age_', shr_age_male %>% names() %>% .[3:10], '_male')
    )
  )


# Aggregate by race ----------------------------------------------------------------------
  # 1: Total black population by year and county
  race_all = collap(
    X = seer_dt[race %in% 1:2],
    by = ~ yr + fips + race,
    custom = list(fsum = c(race_all = 'pop'))
  )
  # 2: Female black population by year and county
  race_female = collap(
    X = seer_dt[race %in% 1:2 & sex == 2],
    by = ~ yr + fips + race,
    custom = list(fsum = c(race_female = 'pop'))
  )
  # 3: Male black population by year and county
  race_male = collap(
    X = seer_dt[race %in% 1:2 & sex == 1],
    by = ~ yr + fips + race,
    custom = list(fsum = c(race_male = 'pop'))
  )


# Calculate county-year race shares ------------------------------------------------------
  #   a. merge age-group datasets with relevant population datasets;
  #   b. calculate the shares
  #   c. drop unwanted columns and pivot to wide
  #   d. rename columns
  # Full population (across sexes)
  shr_race_all =
    race_all |>
    merge(y = pop_all, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_race_all = race_all / pop_all) |>
    fselect(-pop_all, -race_all) |>
    pivot(how = 'wider', ids = c('yr', 'fips'), names = 'race', drop = FALSE)
  setnames(
    shr_race_all,
    c('yr', 'fips', 'shr_racewhite_all', 'shr_raceblack_all')
  )
  # Female population
  shr_race_female =
    race_female |>
    merge(y = pop_female, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_race_female = race_female / pop_female) |>
    fselect(-pop_female, -race_female) |>
    pivot(how = 'wider', ids = c('yr', 'fips'), names = 'race', drop = FALSE)
  setnames(
    shr_race_female,
    c('yr', 'fips', 'shr_racewhite_female', 'shr_raceblack_female')
  )
  # Male population
  shr_race_male =
    race_male |>
    merge(y = pop_male, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_race_male = race_male / pop_male) |>
    fselect(-pop_male, -race_male) |>
    pivot(how = 'wider', ids = c('yr', 'fips'), names = 'race', drop = FALSE)
  setnames(
    shr_race_male,
    c('yr', 'fips', 'shr_racewhite_male', 'shr_raceblack_male')
  )


# Aggregate by origin --------------------------------------------------------------------
# NOTE Origin is only available for the dataset that begins in 1990
if (data_year == 1990) {
  # 1: Total hispanic population by year and county
  hispanic_all = collap(
    X = seer_dt[origin == 1],
    by = ~ yr + fips,
    custom = list(fsum = c(hispanic_all = 'pop'))
  )
  # 2: Female hispanic population by year and county
  hispanic_female = collap(
    X = seer_dt[origin == 1 & sex == 2],
    by = ~ yr + fips,
    custom = list(fsum = c(hispanic_female = 'pop'))
  )
  # 3: Male hispanic population by year and county
  hispanic_male = collap(
    X = seer_dt[origin == 1 & sex == 1],
    by = ~ yr + fips,
    custom = list(fsum = c(hispanic_male = 'pop'))
  )
}


# Calculate county-year origin share -----------------------------------------------------
if (data_year == 1990) {
  #   a. merge age-group datasets with relevant population datasets;
  #   b. calculate the shares
  #   c. drop unwanted columns and pivot to wide
  #   d. rename columns
  # Full population (across sexes)
  shr_hispanic_all =
    hispanic_all |>
    merge(y = pop_all, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_hispanic_all = hispanic_all / pop_all) |>
    fselect(-pop_all, -hispanic_all)
  setnames(
    shr_hispanic_all,
    c('yr', 'fips', 'shr_hispanic_all')
  )
  # Female population
  shr_hispanic_female =
    hispanic_female |>
    merge(y = pop_female, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_hispanic_female = hispanic_female / pop_female) |>
    fselect(-pop_female, -hispanic_female)
  setnames(
    shr_hispanic_female,
    c('yr', 'fips', 'shr_hispanic_female')
  )
  # Male population
  shr_hispanic_male =
    hispanic_male |>
    merge(y = pop_male, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE) |>
    fmutate(shr_hispanic_male = hispanic_male / pop_male) |>
    fselect(-pop_male, -hispanic_male)
  setnames(
    shr_hispanic_male,
    c('yr', 'fips', 'shr_hispanic_male')
  )
}


# Join datasets --------------------------------------------------------------------------
  # Create master dataset with all fips-year combinations; merge relevant population data
  all_dt =
    merge(
      x = CJ(
        yr = pop_all[, funique(yr)],
        fips = pop_all[, funique(fips)]
      ),
      y = pop_all,
      by = c('yr', 'fips'),
      all.x = TRUE,
      all.y = FALSE
    )
  female_dt =
    merge(
      x = CJ(
        yr = pop_all[, funique(yr)],
        fips = pop_all[, funique(fips)]
      ),
      y = pop_female,
      by = c('yr', 'fips'),
      all.x = TRUE,
      all.y = FALSE
    )
  male_dt =
    merge(
      x = CJ(
        yr = pop_all[, funique(yr)],
        fips = pop_all[, funique(fips)]
      ),
      y = pop_male,
      by = c('yr', 'fips'),
      all.x = TRUE,
      all.y = FALSE
    )
  # Merge relevant age data
  all_dt %<>%
    merge(y = shr_age_all, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
  female_dt %<>%
    merge(y = shr_age_female, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
  male_dt %<>%
    merge(y = shr_age_male, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
  # Merge relevant race data
  all_dt %<>%
    merge(y = shr_race_all, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
  female_dt %<>%
    merge(y = shr_race_female, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
  male_dt %<>%
    merge(y = shr_race_male, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
  # Merge relevant origin data
if (data_year == 1990) {
  all_dt %<>%
    merge(y = shr_hispanic_all, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
  female_dt %<>%
    merge(y = shr_hispanic_female, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
  male_dt %<>%
    merge(y = shr_hispanic_male, by = c('yr', 'fips'), all.x = TRUE, all.y = FALSE)
}


# Fill in missing zeroes -----------------------------------------------------------------
  # Set aside observations missing population data
  napop_all = all_dt[is.na(pop_all)]
  napop_female = female_dt[is.na(pop_female)]
  napop_male = male_dt[is.na(pop_male)]
  # And grab observations with population (zeroes are relevant for them)
  nonapop_all = all_dt[!is.na(pop_all)]
  nonapop_female = female_dt[!is.na(pop_female)]
  nonapop_male = male_dt[!is.na(pop_male)]
  # Fill in missing values (in the no-NA-population datasets) with zeroes
  # If population is not missing, then missing share is zero (missing due to SEER)
  nonapop_all[is.na(nonapop_all)] = 0
  nonapop_female[is.na(nonapop_female)] = 0
  nonapop_male[is.na(nonapop_male)] = 0
  # Combine the datasets
  all_dt = rbindlist(list(napop_all, nonapop_all))
  female_dt = rbindlist(list(napop_female, nonapop_female))
  male_dt = rbindlist(list(napop_male, nonapop_male))
  # Order by FIPS and year
  setorder(all_dt, fips, yr)
  setorder(female_dt, fips, yr)
  setorder(male_dt, fips, yr)


# Save -----------------------------------------------------------------------------------
  # Ensure the target directory exits
  dir_save = here('data', 'seer')
  dir_save |> dir.create(recursive = TRUE, showWarnings = FALSE)
  # Save each dataset (noting the data year)
  write_fst(
    x = all_dt,
    path = here(dir_save, paste0('seer-shares-allpop-', data_year, '-2022.fst')),
    compress = 100
  )
  write_fst(
    x = female_dt,
    path = here(dir_save, paste0('seer-shares-femalepop-', data_year, '-2022.fst')),
    compress = 100
  )
  write_fst(
    x = male_dt,
    path = here(dir_save, paste0('seer-shares-malepop-', data_year, '-2022.fst')),
    compress = 100
  )


}
