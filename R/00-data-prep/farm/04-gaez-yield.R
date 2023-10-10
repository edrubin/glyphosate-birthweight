# Aggregating GAEZ Crop Suitability Index by county and watershed
library(pacman)
p_load(
  here, stars, data.table, sf, tigris, janitor, dplyr, purrr,
  ggplot2, fst, stringr, furrr, terra, exactextractr, magrittr
)
options(tigris_use_cache = TRUE)
# Getting one file to convert crs to
y_soy_h = read_stars(here(
  "data/download-manual/attainable-yield/ylHr_soy.tif"
))
# Going to transform everything into this CRS
y_crs = st_crs(y_soy_h)

# Loading county shapes
# Getting state shapes from tigris package
# Just need fips codes 
states_sf = 
  states(year = 2010, cb = T) |>
  filter( # Limiting to continental US
    !(STATE %in% c("02","15","60","66","69","72","78"))
  ) |>
  st_transform(crs = y_crs) |>
  clean_names()

# Continental US sf
cont_sf = st_union(states_sf) |> st_as_sf() |> mutate(in_us = TRUE)

# Getting county level data for 2010
county_sf =
  map_dfr(
    states_sf$state,
    counties,
    year = 2010,
    cb = TRUE
  ) |> 
  clean_names() |>
  mutate(geoid = paste0(statefp,countyfp))  |>
  st_transform(crs = y_crs) 

# Loading the suitability index data
aggregate_gaez_county = function(crop){
  # Reading in the raster
  yield_rast = c(
    rast(here(paste0( # low yield
      "data/download-manual/attainable-yield/ylLr_",crop,".tif"
    ))), 
    rast(here(paste0( # high yield
      "data/download-manual/attainable-yield/ylHr_",crop,".tif"
    )))
  )
  # Now we can aggregate to the county level 
  y_county_mean = exact_extract(
    x = yield_rast, 
    y = county_sf, 
    fun = 'mean'
  )
  # Cleaning up 
  y_diff_dt = 
    data.table(
      geoid = county_sf$geoid,
      y_county_mean
    ) |> 
    setnames(c('GEOID','yield_low','yield_high'))
  y_diff_dt[,':='(
      crop = crop,
      yield_diff = fifelse(yield_high - yield_low >= 0, yield_high - yield_low, 0)
  )]
  # Saving the results 
  write.fst(
    y_diff_dt, 
    path = here(paste0(
      "data/raw/attainable-yield-county/y-diff-",
      crop,"-dt.fst"
    ))
  )
  return(y_diff_dt)
}  
  
# Grabbing the crops we have to aggregate
crop_list = list.files(here("data/download-manual/attainable-yield")) |>
  str_extract("(?<=_)\\w{3}(?=\\.)") |>
  unique()
already_run = list.files(here(
  "data/raw/attainable-yield-county"
  )) |>
  str_extract("(?<=y-diff-)\\w{3}(?=-dt)") |>
  unique()

# Running!
plan(multisession(workers = 4))
y_diff_dt  =
  future_map_dfr(
    crop_list,#[!(crop_list %in% already_run)],
    aggregate_gaez_county
  )
# Reading the results 
y_diff_dt = 
  map_dfr(
    list.files(
      here("data/raw/attainable-yield-county"),
      full.names = TRUE
    ),
    \(x){read.fst(path = x, as.data.table = TRUE)}
  )

# Saving the result   
write.fst(
  y_diff_dt, 
  here("data/raw/y_diff_dt.fst")
)