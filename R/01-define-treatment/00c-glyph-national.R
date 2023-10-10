# -----------------------------------------------------------------------------
# Shift share instrument: Want to get total glyphosate sprayed nationally
# - Minus own county
# - Minus any counties within XX distance of own county
# - Minus any within the same watershed
library(pacman)
p_load(
    here, fst, data.table, sf , tigris, purrr, stringr, dplyr,
    janitor, collapse
)
options(tigris_use_cache = TRUE)
# Loading data ----------------------------------------------------------------
  # glyphosate 
  pest_dt = 
    read.fst(
      here("data/raw/est_pest_use.fst"),
      as.data.table = TRUE
    )[,census_year := str_sub(year, 1,3)]
  # County and state shapes
  states_sf = 
    states(
      year = 2010,
      cb = TRUE  
    ) |>
    filter( # Limiting to continental US
      !(STATE %in% c("02","15","60","66","69","72","78"))
    ) |>
    clean_names()
  county_sf =
    map_dfr(
      states_sf$state,
      counties,
      year = 2010,
      cb = TRUE
    ) |> 
    clean_names() |>
    mutate(
      GEOID = paste0(statefp,countyfp)
    ) |>
    st_transform(crs = 2163) 

# Calculating distance between every county -----------------------------------
if(!file.exists(here('data/raw/county-dist-dt.fst'))){
  county_dist_mat = st_distance(county_sf, county_sf)
  # Converting to long data table 
  county_dist_dt = data.table(county_dist_mat)
  setnames(
    county_dist_dt, 
    paste0('GEOID_',county_sf$GEOID)
  )
  county_dist_dt[,GEOID := county_sf$GEOID] 
  county_dist_dt = 
    melt(
      county_dist_dt, 
      id.vars = 'GEOID'
    )[,.(
      GEOID, 
      to_geoid = str_remove(variable, 'GEOID_'), 
      dist_m = units::drop_units(value)
    )] 
  # Saving 
  write.fst(
    county_dist_dt, 
    here('data/raw/county-dist-dt.fst')
  )
}else{
  # Read the already calculated distance file
  county_dist_dt = 
    read.fst(
      here('data/raw/county-dist-dt.fst'),
      as.data.table = TRUE
    )
}

# Finding all counties upstream -----------------------------------------------
  # County to hybas crosswalk
  area_weight_dt = 
    read_fst(
      here("data/watershed/weights/hydrobasin-area-weights.fst"),
      as.data.table = TRUE
    )[,.(
      hybas_id, 
      GEOID = geoid, 
      watershed_weight 
    )] |>
    setkey(hybas_id,GEOID)
  # Upstream watersheds
  upstream_dt = 
    read.fst(
      path = here("data/watershed/upstream-dt-hydrobasin.fst"), 
      as.data.table = TRUE
    )[ # Filtering to just upstream. Picking out relevant columns
      dist_km > 0 & local == FALSE,.(
      hybas_id, hybas_id2, dist_km, dist_km_bin
    )] |> 
    setkey(hybas_id2)
  # First merging to counties to get all watersheds upstream from a county
  county_upstream_dt = 
    merge(
      area_weight_dt,
      upstream_dt, 
      by = 'hybas_id',
      allow.cartesian = TRUE
    ) |> 
    # Now merge upstream watersheds back to county
    merge(
      area_weight_dt,
      by.x = 'hybas_id2',
      by.y = 'hybas_id',
      allow.cartesian = TRUE
    ) %>%
    .[,.(GEOID = GEOID.x, up_geoid = GEOID.y)] |>
    unique()

# Now we summarize non-local, non-upsteam glyph spraying ----------------------
  # Merging upstream with distances 
  counties_up_100km = 
    rbind(
      county_dist_dt[dist_m < 100000,.(GEOID, geoid_2 = to_geoid)],
      county_upstream_dt[,.(GEOID, geoid_2 = up_geoid)]
    ) |> unique()
  # Glyphosate sprayed in counties less than 100km from own county
  pest_dt_100km = 
    merge(
      counties_up_100km,
      pest_dt, 
      by.x = 'geoid_2',
      by.y = 'GEOID',
      allow.cartesian = TRUE
    )[,-c('geoid_2','census_year')] |>
    gby(GEOID, year) |>
    fsum()
  # National glyphosate
  pest_dt_nat = 
    pest_dt[,-c('GEOID','census_year')] |>
    gby(year) |>
    fsum()
  # Merging together to create instruments 
  shift_share_dt = 
    CJ(
      year = pest_dt_nat$year,
      GEOID = county_sf$GEOID
    ) |>
    merge(
      pest_dt_nat,
      by = 'year'
    ) |>
    merge(
      pest_dt_100km,
      by = c('GEOID','year'),
      all.x = TRUE
    ) |>
    merge(
      pest_dt,
      by = c('GEOID','year'),
      all.x = TRUE
    )  %>%
    .[,.(
      GEOID, 
      year,
      glyphosate_local = fifelse(is.na(glyphosate),0,glyphosate), 
      glyphosate_100km = glyphosate.y,
      glyphosate_nat = glyphosate.x - fifelse(is.na(glyphosate),0,glyphosate),
      glyphosate_nat_100km = glyphosate.x - glyphosate.y
    )]

# Saving the results 
write.fst(
  shift_share_dt, 
  here('data/clean/glyph-nat-dt.fst')
)

# Now making a watershed level version
  # First re-adjusting weights 
  area_weight_dt[,
    wt := watershed_weight/sum(watershed_weight),
    by = hybas_id
  ]
  watershed_shift_share_dt = 
    merge(
      shift_share_dt,
      area_weight_dt,
      by = 'GEOID',
      allow.cartesian = TRUE
    )[,-c('GEOID','watershed_weight')] |>
    gby(hybas_id, year) |>
    fmean(w = wt)
  # saving the result 
  write.fst(
    watershed_shift_share_dt, 
    here('data/watershed/glyph-nat-watershed-dt.fst')
  )


#p_load(ggplot2)
#ggplot(shift_share_dt, aes(x = glyphosate_nat_100km)) + 
#  geom_density() + 
#  geom_density(aes(x = glyphosate_nat), color= 'red')
