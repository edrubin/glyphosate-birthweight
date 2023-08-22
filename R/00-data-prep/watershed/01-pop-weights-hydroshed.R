# --------------------------------------------------------------
# Watershed weighting 
# --------------------------------------------------------------
pacman::p_load(
  data.table, here, fixest, ggplot2, modelsummary, purrr, haven,
  tigris, magrittr, sf, tidyr, dplyr, janitor, stars,
   tidyverse, tictoc, furrr, fst
)
options(tigris_use_cache=TRUE)

# --------------------------------------------------------------
# Loading Data 
# --------------------------------------------------------------
# Loading CSDL crs
csdl_crs = 
  read_stars(
    here(paste0("data/spatial/csdl/1999_CSDL_v04.tif")),
    proxy = TRUE
  ) |>
  st_crs()

# Getting state shapes from tigris package
# Just need fips codes 
states_sf = 
  states(year = 2000) |>
  filter( # Limiting to continental US
    !(STATEFP00 %in% c("02","15","60","66","69","72","78"))
  ) |>
  st_transform(crs = csdl_crs) |>
  clean_names()

# Continental US sf
cont_sf = st_union(states_sf) |> st_as_sf() |> mutate(in_us = TRUE)

# Loading HydroBASINS data limiting to those in the continental US
watershed_sf = 
  read_sf(here("data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
  st_simplify(preserveTopology = FALSE, dTolerance = 0.01) |>
  st_transform(crs = csdl_crs) |>
  clean_names() |>
  st_join(cont_sf) |>
  filter(in_us == TRUE)|>
  st_make_valid()

# --------------------------------------------------------------
# Calculating population weights for each watershed
# --------------------------------------------------------------
# Pop data available 1990, 2000, 2010
watershed_pop_weight = function(load_yr){
  
  print(paste(load_yr,"started"))
  
  # Loading population grid data 
  pop_grid = 
    read_stars(
      here(paste0("data/spatial/us-pop-grid/uspop",load_yr,".tif")),
      proxy = TRUE  
    ) 
  
  # Renaming
  pop_grid$pop = pop_grid[[1]]
  pop_grid = 
    pop_grid |>
    dplyr::select(pop) |>
    st_transform(crs = csdl_crs)

  # County shapefiles from tigris package
  # Available 1990, 2000, 2010-2020
  if(load_yr == 1990){
    # Need to specify CB=T for 1990
    county_sf =
      map_dfr(
        states_sf$statefp00,
        counties,
        year = load_yr,
        cb = TRUE
      ) |> 
      clean_names() |>
      mutate(geoid = paste0(statefp,countyfp))  |>
      st_transform(crs = csdl_crs) 
    
  } else {
    county_sf =
      map_dfr(
        states_sf$statefp00,
        counties,
        year = load_yr
      ) |> 
      clean_names() |>
      mutate(geoid = paste0(statefp,countyfp))  |>
      st_transform(crs = csdl_crs)  
  }
  
  print(paste(load_yr,"data loaded, starting intersection"))
  
  # Taking intersection of watersheds and counties 
  county_watersheds = st_intersection(watershed_sf,county_sf)
  
  print(paste(load_yr,"starting pop aggregation"))
  # Aggregating population from county in each watershed
  watershed_pop = aggregate(
    pop_grid, 
    by = county_watersheds, 
    FUN = sum, 
    na.rm = TRUE
  )
  
  print(paste(load_yr,"merging pop"))
  
  # Turning it into a data.table 
  watershed_pop_dt =   
      st_as_sf(watershed_pop) |> 
        data.table() |>
        mutate(
          hybas_id = county_watersheds$hybas_id,
          geoid = county_watersheds$geoid
        ) |> 
        select(-geometry)
  
  print(paste(load_yr,"weight calculations"))
  
  # Creating county totals
  watershed_pop_dt[,':='(
    pop_cnty = sum(pop, na.rm = T)
  ),
  by = .(geoid)
  ]
  
  # Calculating weights 
  watershed_pop_dt[,':='(
    pop_weight = pop/pop_cnty
  )]
  print(paste(load_yr,"done"))
  
  # Returning just the weights
  write.fst(
    watershed_pop_dt[,.(hybas_id,geoid,pop_weight)],
    path = here(paste0(
      "data-clean/watershed/weights/hydrobasin/hydrobasin-pop-weights",load_yr,".fst"
      ))
  )
}

# Running pop weight 
map(
  c(1990,2000,2010),
  watershed_pop_weight
)


