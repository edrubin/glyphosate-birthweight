#-------------------------------------------------------
# Getting data from USDA NASS database
#-------------------------------------------------------
# Loading required packages
library(pacman)
p_load(
  tidyverse, usdarnass, rnassqs, data.table, 
  here, purrr, magrittr, janitor, fst
)

# NASS API key
nassqs_auth(Sys.getenv("NASS_KEY"))

#-------------------------------------------------------
# Survey data (annual)
#-------------------------------------------------------

# Looking for the parameters that we want to get
short_desc_acres_survey_raw = 
  nass_param(
    "short_desc",
    sector_desc = "CROPS",
    agg_level_desc = "COUNTY",
    source_desc = "SURVEY"
  ) 

# Filtering to acres planted, excluding rows that would 
# double count (irrigated + nonirrigated and total)
short_desc_acres_survey = 
  short_desc_acres_survey_raw[
    short_desc_acres_survey_raw |> str_detect("ACRES PLANTED$") &
    !(short_desc_acres_survey_raw |> str_detect("IRRIGATED")) &
    !(short_desc_acres_survey_raw |> str_detect("OIL")) &
    !(short_desc_acres_survey_raw |> str_detect("WINTER")) &
    !(short_desc_acres_survey_raw |> str_detect("SPRING")) &
      !(short_desc_acres_survey_raw |> str_detect("FOLLOWING ANOTHER CROP"))&
      !(short_desc_acres_survey_raw |> str_detect("CL CHICKPEAS"))
  ]
short_desc_yield_survey = 
  short_desc_acres_survey_raw[
    short_desc_acres_survey_raw |> str_detect("YIELD.*/ ACRE$") &
      !(short_desc_acres_survey_raw |> str_detect("IRRIGATED")) &
      !(short_desc_acres_survey_raw |> str_detect("OIL")) &
      !(short_desc_acres_survey_raw |> str_detect("WINTER")) &
      !(short_desc_acres_survey_raw |> str_detect("SPRING")) &
      !(short_desc_acres_survey_raw |> str_detect("CL CHICKPEAS")) &
      !(short_desc_acres_survey_raw |> str_detect("ALFALFA"))&
      !(short_desc_acres_survey_raw |> str_detect("FOLLOWING ANOTHER CROP"))
  ]
short_desc_acres_irrigated_survey = 
  short_desc_acres_survey_raw[
    short_desc_acres_survey_raw |> str_detect("ACRES PLANTED$") &
    short_desc_acres_survey_raw |> str_detect("IRRIGATED") &
    short_desc_acres_survey_raw |> str_detect("CORN|SOYBEANS|COTTON")
  ]


# Getting years available for each crop
crop_years =
  map_dfr(
    short_desc_acres_survey,
    \(sda){
        nass_param(
          "year",
          short_desc = sda,
          agg_level_desc = "COUNTY",
          sector_desc = "CROPS"
        ) |> tibble() |>
        mutate(
          short_desc = sda,
          year = `nass_param(...)`
        ) |>
        select(short_desc,year)
    }
  )

crop_years_yield =
  map_dfr(
    short_desc_yield_survey,
    \(sda){
      nass_param(
        "year",
        short_desc = sda,
        agg_level_desc = "COUNTY",
        sector_desc = "CROPS"
      ) |> tibble() |>
        mutate(
          short_desc = sda,
          year = `nass_param(...)`
        ) |>
        select(short_desc,year)
    }
  )

crop_years_irrigated =
  map_dfr(
    short_desc_acres_irrigated_survey,
    \(sda){
      nass_param(
        "year",
        short_desc = sda,
        agg_level_desc = "COUNTY",
        sector_desc = "CROPS"
      ) |> tibble() |>
        mutate(
          short_desc = sda,
          year = `nass_param(...)`
        ) |> 
        select(short_desc,year)
    }
  )

# Filtering to same time as health data and crops not already downloaded
path_survey = "data/crops/crop_yr/survey/acres-planted"
path_survey_yield = "data/crops/crop_yr/survey/yield"
path_survey_irrigated = "data/crops/crop_yr/survey/irrigated"

crop_years_survey = 
  crop_years |> 
  filter(
    year %in% 1985:2017 & 
    !(paste0(short_desc,"-",year,".fst") %in% list.files(path = here(path_survey)))
  ) |>
  as.data.table()
crop_years_survey_yield = 
  crop_years_yield |> 
  filter(
    year %in% 1985:2017 & 
      !(paste0(str_replace_all(short_desc,"/","per"),"-",year,".fst") %in% list.files(path = here(path_survey_yield)))
  ) |>
  as.data.table()
crop_years_survey_irrigated = 
  crop_years_irrigated |> 
  filter(
    year %in% 1985:2017 & 
      !(paste0(str_replace_all(short_desc,"/","per"),"-",year,".fst") %in% list.files(path = here(path_survey_irrigated)))
  ) |>
  as.data.table()


# Getting data for all crop/year combinations
all_crop_acre = 
  map2_dfr(
    .x = crop_years_survey$short_desc,
    .y = crop_years_survey$year,
    .f = \(sda, yr){
      yr_crop =   
        nassqs(
          list(
            short_desc = sda,
            year = yr,
            agg_level_desc = "COUNTY",
            sector_desc = "CROPS"
            ),
          as="data.frame"
        )
      write.fst(
        yr_crop, 
        path = here(paste0(path_survey,"/",sda,"-",yr,".fst"))
      )
    }
  )
all_crop_acre_yield = 
  map2_dfr(
    .x = crop_years_survey_yield$short_desc,
    .y = crop_years_survey_yield$year,
    .f = \(sda, yr){
      yr_crop =   
        nassqs(
          list(
            short_desc = sda,
            year = yr,
            agg_level_desc = "COUNTY",
            sector_desc = "CROPS"
          ),
          as="data.frame"
        )
      write.fst(
        yr_crop, 
        path = here(paste0(path_survey_yield,"/",str_replace_all(sda,"/","per"),"-",yr,".fst"))
      )
    }
  )
all_crop_acre_irrigated = 
  map2_dfr(
    .x = crop_years_survey_irrigated$short_desc,
    .y = crop_years_survey_irrigated$year,
    .f = \(sda, yr){
      yr_crop =   
        nassqs(
          list(
            short_desc = sda,
            year = yr,
            agg_level_desc = "COUNTY",
            sector_desc = "CROPS"
          ),
          as="data.frame"
        )
      write.fst(
        yr_crop, 
        path = here(paste0(path_survey_irrigated,"/",sda,"-",yr,".fst"))
      )
    }
  )
# Reading all of the results in together
all_crop_acre = map_dfr(
  here(paste0(path_survey,"/",list.files(path = here(path_survey)))),
  read.fst,
  as.data.table = T
)

all_crop_acre %<>% clean_names()

all_crop_yield = map_dfr(
  here(paste0(path_survey_yield,"/",list.files(path = here(path_survey_yield)))),
  read.fst,
  as.data.table = T
)

all_crop_irrigated = map_dfr(
  here(paste0(path_survey_irrigated,"/",list.files(path = here(path_survey_irrigated)))),
  read.fst,
  as.data.table = T
)


# Saving the results
write.fst(
  all_crop_acre, 
  path = here("data/crops/all_crop_acre_county.fst")
)
write.fst(
  all_crop_yield, 
  path = here("data/crops/all_crop_yield_county.fst")
)
write.fst(
  all_crop_irrigated, 
  path = here("data/crops/all_crop_irrigated_county.fst")
)

#-------------------------------------------------------
# Census data (5yrs)
#-------------------------------------------------------
# Looking for the parameters that we want to get
short_desc_acres_census_raw = 
  nass_param(
    "short_desc",
    sector_desc = "CROPS",
    agg_level_desc = "COUNTY",
    source_desc = "CENSUS"
  ) 

# Filtering to acres planted, excluding rows that would 
# double count - aggregations, irrigated/non-irrigated, etc.
short_desc_acres_census = 
  short_desc_acres_census_raw[
    ( # First group is fruit and tree nuts 
      short_desc_acres_census_raw |> str_detect("ACRES BEARING & NON-BEARING") &
      !(short_desc_acres_census_raw |> str_detect("TOTALS")) &
      !(short_desc_acres_census_raw |> str_detect("OTHER")) &
      !(short_desc_acres_census_raw |> str_detect(",")) &
      !(short_desc_acres_census_raw |> str_detect("ORCHARDS"))
    )|( # Cherries and Walnuts, excluded because of comma above
      short_desc_acres_census_raw |> str_detect("ACRES BEARING & NON-BEARING") & 
      ((short_desc_acres_census_raw |> str_detect("CHERRIES")
        )|(short_desc_acres_census_raw |> str_detect("WALNUTS")))
    )|( # Now for the field crops and vegetables
      short_desc_acres_census_raw |> str_detect("ACRES HARVESTED") &
      !(short_desc_acres_census_raw |> str_detect("FRESH MARKET")) &
      !(short_desc_acres_census_raw |> str_detect("PROCESSING")) &
      !(short_desc_acres_census_raw |> str_detect("IRRIGATED")) &
      !(short_desc_acres_census_raw |> str_detect("OTHER")) &
      !(short_desc_acres_census_raw |> str_detect("TOTAL")) &
      !(short_desc_acres_census_raw |> str_detect("PIMA")) &
      !(short_desc_acres_census_raw |> str_detect("UPLAND")) &
      !(short_desc_acres_census_raw |> str_detect("ALFALFA")) &
      !(short_desc_acres_census_raw |> str_detect("SALT HAY")) &
      !(short_desc_acres_census_raw |> str_detect("HAYLAGE")) &
      !(short_desc_acres_census_raw |> str_detect("SMALL GRAIN")) &
      !(short_desc_acres_census_raw |> str_detect("HAY, WILD")) &
      !(short_desc_acres_census_raw |> str_detect("LETTUCE, HEAD")) &
      !(short_desc_acres_census_raw |> str_detect("LETTUCE, LEAF")) &
      !(short_desc_acres_census_raw |> str_detect("LETTUCE, ROMAINE")) &
      !(short_desc_acres_census_raw |> str_detect("RASPBERRIES, BLACK")) &
      !(short_desc_acres_census_raw |> str_detect("RASPBERRIES, RED")) &
      !(short_desc_acres_census_raw |> str_detect("SQUASH, SUMMER")) &
      !(short_desc_acres_census_raw |> str_detect("SQUASH, WINTER")) &
      !(short_desc_acres_census_raw |> str_detect("SUNFLOWER, OIL")) &
      !(short_desc_acres_census_raw |> str_detect("SUNFLOWER, NON-OIL")) &
      !(short_desc_acres_census_raw |> str_detect("VEGETABLES")) &
      !(short_desc_acres_census_raw |> str_detect("WHEAT, SPRING")) &
      !(short_desc_acres_census_raw |> str_detect("WHEAT, WINTER"))
    )|( # Alfalfa seed excluded from above
      short_desc_acres_census_raw |> str_detect("LEGUMES, ALFALFA, SEED - ACRES HARVESTED") 
    )
  ]
  
# Getting years available for each crop
crop_years_census_all =   
  map_dfr(
    short_desc_acres_census,
    \(sda){
      nass_param(
        "year",
        short_desc = sda,
        agg_level_desc = "COUNTY",
        sector_desc = "CROPS",
        source_desc = "CENSUS"
      ) |> tibble() |>
        mutate(
          short_desc = sda,
          year = `nass_param(...)`
        ) |> 
        select(short_desc,year)
    }
  )

# Filtering to same time as health data
# Not actually doing anything because 
# these are only years loaded in API
path_census = "data/crops/crop_yr/census"

crop_years_census = 
  crop_years_census_all |> 
  filter(
    year %in% seq(1997,2017,by=5)& 
    !(paste0(short_desc,"-",year,".fst") %in% list.files(path = here(path_census)))
  ) |>
  as.data.table()

# Getting data for census
census_crop_acre = 
  map2_dfr(
    .x = crop_years_census$short_desc,
    .y = crop_years_census$year,
    .f = \(sda, yr){
      yr_crop = 
        nassqs(
          list(
            short_desc = sda,
            year = yr,
            agg_level_desc = "COUNTY",
            sector_desc = "CROPS",
            source_desc = "CENSUS"
          ),
          as="data.frame"
        )
      write.fst(
        yr_crop, 
        path = here(paste0(path_census,"/",sda,"-",yr,".fst"))
      )
    }
  )


# Reading all of the results in together
census_crop_acre = map_dfr(
  here(paste0(path_census,"/",list.files(path = here(path_census)))),
  read.fst,
  as.data.table = T
)

census_crop_acre %<>% clean_names()

# Saving the results
write.fst(
  census_crop_acre, 
  path = here("data/crops/census_crop_acre_county.fst")
)