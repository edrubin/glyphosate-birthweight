# Code to weight glyphosate by the area of overlap between watersheds and counties 
pacman::p_load(
  data.table,here,purrr, tigris, magrittr, sf, tidyr, dplyr,
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
    path = here("data-clean/watershed/weights/hydrobasin-area-weights.fst")
  )

