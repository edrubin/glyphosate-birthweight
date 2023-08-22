# Code to weight glyphosate by the area of overlap between watersheds and counties 
pacman::p_load(
  data.table,here,fixest,ggplot2,modelsummary,purrr,haven,
  tigris, hrbrthemes, magrittr, sf, tidyr, dplyr, HydroCode,
  janitor, stars, tidyverse, tictoc, furrr, fst
)
options(
  tigris_use_cache = TRUE,
  sf_use_s2 = FALSE
)

# Loading data ----------------------------------------------------------------
  # Loading HydroBASINS data
  hydrobasin_sf = 
    read_sf(here("data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
    clean_names() |>
    filter(lake == 0) |>
    st_make_valid()
  # County shapes from tigris
  state_sf = states(year = 2010, cb = TRUE) |>
    filter( # Limiting to continental US
      !(STATE %in% c("02","15","60","66","69","72","78"))
    ) 
  county_sf =
    map_dfr(
      state_sf$STATE,
      counties,
      year = 2010,
      cb = TRUE
    ) |> 
    clean_names() |>
    mutate(
      geoid = paste0(statefp,countyfp),
      cnty_area = st_area(geometry)
    ) |>
    st_transform(crs = st_crs(hydrobasin_sf)) 

# Area weighting with the HydroSHEDS data -------------------------------------
  # Joining watersheds to counties 
  hydrobasin_county_sf = 
    st_intersection(hydrobasin_sf,county_sf) |>
    mutate(area = st_area(geometry))
  # Calculating the weights 
  hydrobasin_county_dt = 
    data.table(hydrobasin_county_sf)[,.(
      hybas_id, 
      geoid, 
      watershed_weight = units::drop_units(area/cnty_area)
    )]
  # Saving the results
  write.fst(
    hydrobasin_county_dt, 
    path = here("data-clean/watershed/weights/hydrobasin/hydrobasin-area-weights.fst")
  )



# Old code that does this with different county shapes for each year ----------

# hydrobasin_area_weight = function(load_yr){
#   # State and county shapefiles from tigris package
#   # Available 1990, 2000, 2010-2020
#   if(load_yr == 1990){
#     county_sf =
#       map_dfr(
#         states_sf$statefp00,
#         counties,
#         year = load_yr,
#         cb = TRUE
#       ) |> 
#       clean_names() |>
#       mutate(
#         geoid = paste0(statefp,countyfp),
#         cnty_area = st_area(geometry)
#       )  |>
#       st_transform(crs = st_crs(hydrobasin_sf)) 
#   } else {
#     county_sf =
#       map_dfr(
#         states_sf$statefp00,
#         counties,
#         year = load_yr
#       ) |> 
#       clean_names() |>
#       mutate(
#         geoid = paste0(statefp,countyfp),
#         cnty_area = st_area(geometry)
#       )  |>
#       st_transform(crs = st_crs(hydrobasin_sf))  
#   }
  
#   # Joining watersheds to counties 
#   hydrobasin_county_sf = 
#     st_intersection(hydrobasin_sf,county_sf) |>
#     mutate(area = st_area(geometry))
  
#   # Calculating the weights 
#   hydrobasin_county_dt = 
#     hydrobasin_county_sf |>
#     data.table() |>
#     select(hybas_id, geoid, area, cnty_area) |>
#     mutate(
#       year = load_yr,
#       watershed_weight = area/cnty_area,
#     )
  
#   return(hydrobasin_county_dt[,.(hybas_id,geoid,year,watershed_weight)])
# }

# # Running for all years
# hydrobasin_county_dt = 
#   map_dfr(
#     c(1990,2000,2010:2018),
#     hydrobasin_area_weight
#   )

# # Saving the results
# write.fst(
#   hydrobasin_county_dt, 
#   path = here("data-clean/watershed/weights/hydrobasin/hydrobasin-area-weights.fst")
# )


