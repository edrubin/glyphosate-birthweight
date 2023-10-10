#-------------------------------------------------------
# Getting farm employment data from the BEA
#-------------------------------------------------------

# Setup
pacman::p_load(
  bea.R, data.table, stringr, magrittr, here, fst
)

# Getting API key 
bea_key = Sys.getenv("BEA_KEY")

#-------------------------------------------------------
# Downloading data from API
#-------------------------------------------------------

# Getting farm employment 2001-2019
farm_01 = beaGet(
  list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'Regional',
    'LineCode' = '70',
    'GeoFips' = 'COUNTY',
    'TableName'= 'CAEMP25N',
    'Year' = 'ALL',
    'ResultFormat' = 'json'
  )
)

# Getting total employment 2001-2019
tot_01 = beaGet(
  list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'Regional',
    'LineCode' = '10',
    'GeoFips' = 'COUNTY',
    'TableName'= 'CAEMP25N',
    'Year' = 'ALL',
    'ResultFormat' = 'json'
  )
)

# Getting farm employment pre 2001
farm_00 = beaGet(
  list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'Regional',
    'LineCode' = '70',
    'GeoFips' = 'COUNTY',
    'TableName'= 'CAEMP25S',
    'Year' = 'ALL',
    'ResultFormat' = 'json'
  )
)

# Getting total employment pre 2001
tot_00 = beaGet(
  list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'Regional',
    'LineCode' = '10',
    'GeoFips' = 'COUNTY',
    'TableName'= 'CAEMP25S',
    'Year' = 'ALL',
    'ResultFormat' = 'json'
  )
)

# Non-farm personal income
nf_income = beaGet(
  list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'Regional',
    'LineCode' = '11',
    'GeoFips' = 'COUNTY',
    'TableName'= 'CAINC4',
    'Year' = 'ALL',
    'ResultFormat' = 'json'
  )
)
# On-farm personal income 
f_income = beaGet(
  list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'Regional',
    'LineCode' = '12',
    'GeoFips' = 'COUNTY',
    'TableName'= 'CAINC4',
    'Year' = 'ALL',
    'ResultFormat' = 'json'
  )
)

# Population
tot_pop = beaGet(
  list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'Regional',
    'LineCode' = '20',
    'GeoFips' = 'COUNTY',
    'TableName'= 'CAINC4',
    'Year' = 'ALL',
    'ResultFormat' = 'json'
  )
)


#-------------------------------------------------------
# Tidying up
#-------------------------------------------------------
farm_empl_dt = 
  rbind(
    farm_01|>
      melt(
        id.vars = 1:5,
        variable.name = "year",
        value.name = "employment"
      ) %>%
      .[,.(
        GEOID = GeoFips,
        year = str_sub(year, start = -4),
        empl_type = "farm_empl",
        employment
      )],
    farm_00|>
      melt(
        id.vars = 1:5,
        variable.name = "year",
        value.name = "employment"
      ) %>%
      .[,.(
        GEOID = GeoFips,
        year = str_sub(year, start = -4),
        empl_type = "farm_empl",
        employment
      )],
    tot_01|>
      melt(
        id.vars = 1:5,
        variable.name = "year",
        value.name = "employment"
      ) %>%
      .[,.(
        GEOID = GeoFips,
        year = str_sub(year, start = -4),
        empl_type = "tot_empl",
        employment
      )],
    tot_00|>
      melt(
        id.vars = 1:5,
        variable.name = "year",
        value.name = "employment"
      ) %>%
      .[,.(
        GEOID = GeoFips,
        year = str_sub(year, start = -4),
        empl_type = "tot_empl",
        employment
      )],
    nf_income|>
      melt(
        id.vars = 1:5,
        variable.name = "year",
        value.name = "employment"
      ) %>%
      .[,.(
        GEOID = GeoFips,
        year = str_sub(year, start = -4),
        empl_type = "nonfarm_income",
        employment
      )],
    f_income|>
      melt(
        id.vars = 1:5,
        variable.name = "year",
        value.name = "employment"
      ) %>%
      .[,.(
        GEOID = GeoFips,
        year = str_sub(year, start = -4),
        empl_type = "farm_income",
        employment
      )],
    tot_pop |>
      melt(
        id.vars = 1:5,
        variable.name = "year",
        value.name = "employment"
      ) %>%
      .[,.(
        GEOID = GeoFips,
        year = str_sub(year, start = -4),
        empl_type = "tot_pop",
        employment
      )]
  ) |>
  dcast(
    GEOID + year ~ empl_type,
    value.var = "employment"
  )

# Calculating per capita income 
farm_empl_dt[,':='(
  inc_per_cap_farm = farm_income/tot_pop,
  inc_per_cap_nonfarm = nonfarm_income/tot_pop
)]
  
# Saving the results 
write.fst(
  farm_empl_dt, 
  path = here("data/download-script/farm-empl-dt.fst")
)


