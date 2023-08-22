# Script calculates the distance between centroids of watersheds 
library(pacman)
p_load(
  here, fst, data.table, sf, dplyr, janitor, units
)

# Watershed shape file to calculate distance from 
hydrobasin_sf = 
  read_sf(here("data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
  st_transform(crs = 2163) |>
  clean_names()

# SF with just watersheds that have downstream
up_sf = hydrobasin_sf |> filter(next_down != 0)
up_df = up_sf |> tibble() |> select(hybas_id, next_down)

# Matching SF with downstream geometries
down_sf = 
  left_join(
    up_df |> select(hybas_id = next_down, next_up = hybas_id), 
    hydrobasin_sf,
    by = 'hybas_id'
  ) |> st_as_sf()


# Calculate distance between all of them
dist = st_distance(
  up_sf |> st_centroid(), 
  down_sf |> st_centroid(), 
  by_element = TRUE
)

# Adding distances to the data
hybas_dist_dt = 
  cbind(
    up_df,
    dist_km = dist |> set_units('km') |> drop_units()
  ) |> data.table()

# Saving results 
write.fst(hybas_dist_dt, path = here("data-clean/watershed/hybas-dist-dt.fst"))


upstream_dt = 
  read.fst(
    path = here("data-clean/watershed/upstream-dt-hydrobasin.fst"),
    as.data.table = TRUE
  )[order(hybas_id, hybas_id2)]

upstream_hybas_sf = 
  merge(
    upstream_dt, 
    hydrobasin_sf,
    by = 'hybas_id'
  )[order(hybas_id, hybas_id2)] |> 
  st_as_sf() |>
  st_centroid()
upstream_hybas_sf2 = 
  merge(
    upstream_dt, 
    hydrobasin_sf,
    by.x = 'hybas_id2',
    by.y = 'hybas_id'
  )[order(hybas_id, hybas_id2)] |> 
  st_as_sf() |>
  st_centroid()

dist = st_distance(
  upstream_hybas_sf, 
  upstream_hybas_sf2, 
  by_element = TRUE
)

upstream_dt[,dist_km:=dist |> set_units('km') |> drop_units()]
  
write.fst(upstream_dt, path = here("data-clean/watershed/upstream-dist-dt.fst"))

