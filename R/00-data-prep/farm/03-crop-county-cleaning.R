
# Takes raw crop acreage/yield/irrigation data and turns it into county-year panel
library(pacman)
p_load(
  here, fst, data.table, magrittr, readr, janitor, purrr
)


# ALL crop acreage
path_survey = "data/download-script/nass-survey/acres-planted"
all_crop_acre = 
  map_dfr(
    here(paste0(path_survey,"/",list.files(path = here(path_survey)))),
    read.fst,
    as.data.table = TRUE
  ) |> 
  clean_names()

# Aggregating crops and casting to wide format
all_crop_acre_dt = 
  all_crop_acre[
    #year %in% yr_start:yr_end &
      county_ansi != "" # Dropping the "rest of state" data that doesnt have a county
    , .(
      year, 
      state_fips = state_fips_code, 
      county_fips = county_ansi,
      GEOID = paste0(state_fips_code,county_ansi),
      crop = fcase( 
        commodity_desc == "CORN", "corn_acres",
        commodity_desc == "SOYBEANS", "soy_acres", 
        commodity_desc == "COTTON", "cotton_acres", 
        default =  "other_acres"
      ),
      value_int = parse_number(value)
    )] %>%
  .[,.(
    tot_acres = sum(value_int)),
    by = .(year,state_fips,county_fips,GEOID,crop)
  ] %>%
  dcast(
    formula = year + state_fips + county_fips + GEOID ~ crop, 
    value.var = "tot_acres",
    fill = 0
  ) %>%
  setkey(GEOID, year)

# Saving the results
write.fst(
  all_crop_acre_dt,
  here('data/raw/all-crop-acre-dt.fst')
)

# Now running yield data
path_survey_yield = "data/download-script/nass-survey/yield"
all_crop_yield = map_dfr(
  here(paste0(path_survey_yield,"/",list.files(path = here(path_survey_yield)))),
  read.fst,
  as.data.table = TRUE
)

# Aggregating crops and casting to wide format
all_crop_yield_dt = 
  all_crop_yield[
    #year %in% yr_start:yr_end &
      county_ansi != "" # Dropping the "rest of state" data that doesnt have a county
    , .(
      year, 
      state_fips = state_fips_code, 
      county_fips = county_ansi,
      GEOID = paste0(state_fips_code,county_ansi),
      crop = fcase( 
        commodity_desc == "CORN", "corn_yield",
        commodity_desc == "SOYBEANS", "soy_yield", 
        commodity_desc == "COTTON", "cotton_yield", 
        default =  "other_yield"
      ),
      value_int = parse_number(Value)
    )] %>%
  .[,.(
    tot_yield = sum(value_int)),
    by = .(year,state_fips,county_fips,GEOID,crop)
  ] %>%
  dcast(
    formula = year + state_fips + county_fips + GEOID ~ crop, 
    value.var = "tot_yield",
    fill = 0
  ) %>%
  setkey(GEOID, year)

write.fst(
  all_crop_yield_dt,
  here('data/raw/all-crop-yield-dt.fst')
)


# ALL crop acreage
path_survey_irrigated = "data/download-script/nass-survey/irrigated"
all_crop_irrigated = map_dfr(
  here(paste0(path_survey_irrigated,"/",list.files(path = here(path_survey_irrigated)))),
  read.fst,
  as.data.table = TRUE
)

# Aggregating crops and casting to wide format
all_crop_irrigated_dt = 
  all_crop_irrigated[
    county_ansi != "" # Dropping the "rest of state" data that doesnt have a county
    , .(
      year = as.numeric(year), 
      state_fips = state_fips_code, 
      county_fips = county_ansi,
      GEOID = paste0(state_fips_code,county_ansi),
      crop = fcase( 
        commodity_desc == "CORN", "corn",
        commodity_desc == "SOYBEANS", "soy", 
        commodity_desc == "COTTON", "cotton", 
        default =  "other"
      ),
      irrigated = prodn_practice_desc == 'IRRIGATED',
      value_int = Value
    )] %>%
  .[,.(
    tot_acres = sum(value_int)),
    by = .(year,state_fips,county_fips,GEOID,crop,irrigated)
  ] %>%
  dcast(
    formula = year + state_fips + county_fips + GEOID + crop ~ irrigated, 
    value.var = "tot_acres",
    fill = 0
  ) %>% 
  .[,.(
    year, state_fips, county_fips, GEOID, crop, 
    pct_irrigated = `TRUE`/(`TRUE`+`FALSE`),
    tot_acres = `TRUE`+`FALSE`
  )] %>% 
  dcast(
    formula = year + state_fips + county_fips + GEOID ~ crop, 
    value.var = c("pct_irrigated",'tot_acres'),
    fill = 0
  ) %>% 
  setkey(GEOID, year)

# Taking weighted mean for corn/soy/cotton
all_crop_irrigated_dt[,':='(
  pct_irrigated_gm = 
    (pct_irrigated_corn*tot_acres_corn + 
       pct_irrigated_soy*tot_acres_soy + 
       pct_irrigated_cotton*tot_acres_cotton
    )/(tot_acres_corn + tot_acres_soy + tot_acres_cotton)
)]

write.fst(
  all_crop_irrigated_dt,
  here('data/raw/all-crop-irrigated-dt.fst')
)

path_census = "data/download-script/nass-census"
# Reading all of the results in together
census_crop_acre = map_dfr(
  list.files(path = here(path_census), full.names = TRUE),
  read.fst,
  as.data.table = TRUE
) |> clean_names()

# Saving the results
write.fst(
  census_crop_acre, 
  path = here("data/raw/census_crop_acre_county.fst")
)