#-------------------------------------------------------
# County population and area
#-------------------------------------------------------
pacman::p_load(
  tidycensus, censusapi, data.table, purrr,
  lubridate, stringr, tigris, sf, units, janitor,
  dplyr, magrittr, fst, here
)

# Using censusapi package to find API endpoints
#apis = listCensusApis()
#View(apis)

# Getting fips codes
states_sf = 
  states(year = 2010, cb = TRUE) |>
  filter( # Limiting to continental US
    !(STATE %in% c("02","15","60","66","69","72","78"))
  ) |>
  clean_names()

# Monsanto headquarters
stl_sfc = st_sfc(st_point(x = c(-90.400708,38.669813))) |>
  st_set_crs(4326)

# Confirmig it is in fact the right location
#ggplot() + 
#  geom_sf(data = states_sf |> st_transform(crs = 4326)) + 
#  geom_sf(data =stl_sfc, color = "red")


#-------------------------------------------------------
# County area 
#-------------------------------------------------------

# Function to get county shapefiles by state/year
get_cnty_area = function(state_fips, yr){
  
  # Getting the shapefiles
  cnty_area_raw = 
    counties(
      state = state_fips,
      cb = TRUE,
      year = yr
    ) |> st_transform(crs = 4326)
  
  # Calculating area
  cnty_area_raw %<>% 
    mutate(
      area_km2 = st_area(cnty_area_raw) |> set_units(km^2) |> drop_units(),
      dist_stl_km = st_distance(cnty_area_raw, stl_sfc) |> set_units(km) |> drop_units()
    )
  
  # Converting to datatable
  cnty_area_raw = cnty_area_raw |> data.table() |> select(-geometry)
  
  # Cleaning data
  if(yr == 1990 | yr == 2000){
    cnty_area =
      cnty_area_raw[,.(
        state_fips = STATEFP, 
        county_fips = COUNTYFP, 
        census_year = ifelse(yr == 1990,"199","200"),
        area_km2,
        dist_stl_km
      )]

  }else if(yr == 2010){
    # Area is in sq mi
    cnty_area =
      cnty_area_raw[,.(
        state_fips = STATE, 
        county_fips = COUNTY, 
        census_year = "201",
        area_km2,
        dist_stl_km
      )]
  } else {
    print("YEAR IS WRONG")
  }
  
  print(paste(yr, state_fips, "done"))
  
  return(cnty_area)
}

# Vectors to run the function over 
yrs = rep(c(1990,2000,2010), each = length(states_sf$state))
state_fips_yrs = rep(states_sf$state, length(unique(yrs)))

# Getting data for every county/year
cnty_area_dt = 
  map2_dfr(
    state_fips_yrs,
    yrs,
    get_cnty_area
  )

# Creating GEOID
cnty_area_dt = 
  cnty_area_dt[,.(
    GEOID = paste0(state_fips,county_fips),
    census_year,
    area_km2, dist_stl_km
  )]


# Saving the results
write.fst(
  cnty_area_dt, 
  path = here("data/pop-area-empl/cnty-area-dt.fst")
)

#-------------------------------------------------------
# 1990-2000: pep/int_charagegroups
#-------------------------------------------------------
vars90 = 
  listCensusMetadata(
    name = "pep/int_charagegroups", 
    vintage = 1990,
    type = "variables"
  )

# Function to get data for all states 1990-1999
get_cnty_pop90 = function(state_fips){
  
  # Getting data from API
  cnty_dt = getCensus(
    name = "pep/int_charagegroups",
    vintage = 1990,
    region = "county:*",
    regionin = paste0("state:",state_fips),
    vars = c("YEAR","AGEGRP","RACE_SEX","HISP","POP")
  ) |> as.data.table()
  

  
  # Returning results summarized by year/county
  return(
    # Calculating percent hispanic
    cnty_dt[,.(
      tot_pop = sum(POP)),
      by = .(
        state_fips = state,
        county_fips = county,
        year = paste0("19",YEAR),
        hispanic = HISP
      )] |> 
      dcast(
        state_fips + county_fips + year ~ hispanic, 
        value.var = "tot_pop"
      ) %>% 
      .[,tot_pop := `1` + `2`] %>% 
      .[,`1` := NULL] |>
      setnames(old = "2", new = "tot_pop_hisp") 
  )
}

# Running for all states
cnty_pop_90_dt = 
  map_dfr(
    states_sf$state,
    get_cnty_pop90
  )

cnty_pop_90_dt = cnty_pop_90_dt[year != "199"]

# Saving the results
write.fst(
  cnty_pop_90_dt, 
  path = here("data/pop-area-empl/cnty-pop-90.fst")
)

#-------------------------------------------------------
# 2000-2010: pep/int_population
#-------------------------------------------------------
vars00 = 
  listCensusMetadata(
    name = "pep/int_charagegroups", 
    vintage = 2000,
    type = "variables"
  )


# Function to get data for all states 2000-2009
get_cnty_pop00 = function(state_fips){
  
  # Getting data from API
  cnty_dt = getCensus(
    name = "pep/int_charagegroups",
    vintage = 2000,
    region = "county:*",
    regionin = paste0("state:",state_fips),
    vars = c("DATE_","DATE_DESC","POP","HISP")
  ) |> as.data.table()
  
  # Returning dt with date parsed
  return(
    cnty_dt[
      !str_detect(DATE_DESC,"base"), #excl duplicates in 2000
      .(state_fips = state,
        county_fips = county,
        year = str_sub(DATE_DESC,start = 5, end = 8),
        tot_pop = POP,
        hispanic = HISP == 2)
    ] |> 
      dcast(
        state_fips + county_fips + year ~ hispanic, 
        value.var = "tot_pop",
        fun.aggregate = sum
      ) %>% 
      .[,tot_pop := `TRUE` + `FALSE`] %>% 
      .[,`FALSE` := NULL] |>
      setnames(old = "TRUE", new = "tot_pop_hisp") 
  )
}

# Running for all states
cnty_pop_00_dt = 
  map_dfr(
    states_sf$state,
    get_cnty_pop00
  )

# Saving the results
write.fst(
  cnty_pop_00_dt, 
  path = here("data/pop-area-empl/cnty-pop-00.fst")
)

#-------------------------------------------------------
# 2010-2019: pep/population
#-------------------------------------------------------
vars10 = 
  listCensusMetadata(
    name = "pep/charagegroups", 
    vintage = 2019,
    type = "variables"
  )


# Function to get data for all states 2011-2019
get_cnty_pop10 = function(state_fips){
  
  # Getting data from API
  cnty_dt = getCensus(
    name = "pep/charagegroups",
    vintage = 2019,
    region = "county:*",
    regionin = paste0("state:",state_fips),
    vars = c("DATE_CODE","POP", "HISP")
  ) |> as.data.table()
  
  cnty_dt[,date_code := as.numeric(DATE_CODE)]
  
  # Returning dt with date parsed
  return(
    cnty_dt[
      date_code > 3, #excl 2010
      .(state_fips = state,
        county_fips = county,
        year = as.character(2007 + date_code),
        tot_pop = POP,
        hispanic = HISP == 2)
    ] |> 
      dcast(
        state_fips + county_fips + year ~ hispanic, 
        value.var = "tot_pop",
        fun.aggregate = sum
      ) %>% 
      .[,tot_pop := `TRUE` + `FALSE`] %>% 
      .[,`FALSE` := NULL] |>
      setnames(old = "TRUE", new = "tot_pop_hisp")
  )
}

# Running for all states
cnty_pop_10_dt = 
  map_dfr(
    states_sf$state,
    get_cnty_pop10
  )

# Saving the results
write.fst(
  cnty_pop_10_dt, 
  path = here("data/pop-area-empl/cnty-pop-10.fst")
)
#-------------------------------------------------------
# Combining all of the results 
#-------------------------------------------------------

cnty_pop_dt = 
  rbind(
    cnty_pop_90_dt,
    cnty_pop_00_dt,
    cnty_pop_10_dt
  )

# Creating GEOID
cnty_pop_dt = 
  cnty_pop_dt[,.(
    GEOID = paste0(state_fips,county_fips),
    year = as.numeric(year),
    tot_pop, tot_pop_hisp
  )]


# Saving the results
write.fst(
  cnty_pop_dt, 
  path = here("data/pop-area-empl/cnty-pop-dt.fst")
)







