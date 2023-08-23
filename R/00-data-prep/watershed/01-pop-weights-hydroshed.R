# --------------------------------------------------------------
# Watershed weighting 
# --------------------------------------------------------------
pacman::p_load(
  data.table, here, purrr, tigris, magrittr, sf, 
  tidyr, dplyr, janitor, stars, exactextractr,
  tidyverse, tictoc, furrr, fst, terra
)
options(tigris_use_cache=TRUE)
sf_use_s2(FALSE)

# --------------------------------------------------------------
# Loading Data 
# --------------------------------------------------------------
# Getting state shapes from tigris package 
states_sf = 
  states(year = 2010) |>
  filter( # Limiting to continental US
    !(STATEFP10 %in% c("02","15","60","66","69","72","78"))
  ) |>
  clean_names()
# Continental US sf
cont_sf = st_union(states_sf) |> st_as_sf() |> mutate(in_us = TRUE)
cont_crs = st_crs(cont_sf)
# County shapes
county_sf =
  map_dfr(
    states_sf$statefp10,
    counties,
    year = 2010
  ) |> 
  clean_names() |>
  mutate(geoid = paste0(statefp,countyfp))  |>
  st_transform(crs = cont_crs)  
# Loading HydroBASINS data limiting to those in the continental US
watershed_sf = 
  read_sf(here("data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
  st_simplify(preserveTopology = FALSE, dTolerance = 0.01) |>
  st_make_valid() |>
  st_transform(crs = cont_crs) |>
  clean_names() |>
  st_join(cont_sf) |>
  filter(in_us == TRUE)

# --------------------------------------------------------------
# Calculating population weights for each watershed
# --------------------------------------------------------------
# Pop data available 1990, 2000, 2010
watershed_pop_weight = function(load_yr){
  print(paste(load_yr,"started"))
  # Loading population grid data 
  pop_grid = rast(here(paste0("data/spatial/us-pop-grid/uspop",load_yr,".tif")))
  # Taking intersection of watersheds and counties 
  county_watersheds = st_intersection(watershed_sf,county_sf)
  print(paste(load_yr,"starting pop aggregation"))
  # Aggregating population from county in each watershed
  watershed_pop = exact_extract(
    x = pop_grid, 
    y = county_watersheds, 
    fun = 'sum'
  )
  print(paste(load_yr,"weight calculations"))
  # Turning it into a data.table 
  watershed_pop_dt = data.table(
    hybas_id = county_watersheds$hybas_id,
    geoid = county_watersheds$geoid,
    pop = watershed_pop
  )
  # Creating county totals
  watershed_pop_dt[,
    ':='(pop_cnty = sum(pop, na.rm = TRUE)),
    by = .(geoid)
  ]  
  # Calculating weights 
  watershed_pop_dt[,':='(
    pop_weight = pop/pop_cnty
  )]
  # Returning just the weights
  write.fst(
    watershed_pop_dt[,.(hybas_id,geoid,pop_weight)],
    path = here(paste0(
      "data-clean/watershed/weights/hydrobasin-pop-weights",load_yr,".fst"
      ))
  )
  print(paste(load_yr,"done"))
}

# Running pop weight 
map(
  c(1990,2000,2010),
  watershed_pop_weight
)


