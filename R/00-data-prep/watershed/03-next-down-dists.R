# Script calculates the distance between centroids of watersheds 
# from the next downstream watershed
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