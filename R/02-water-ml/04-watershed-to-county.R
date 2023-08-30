# --------------------------------------------------------------
# Aggregate upstream glyph from watershed level to county level
# Runtime: 4mins
# --------------------------------------------------------------
pacman::p_load(
  data.table, here, fixest, ggplot2, purrr, 
  tigris, magrittr, sf, tidyr, dplyr, 
  janitor, stars, stringr, tictoc, furrr, fst, collapse,
  units
)

# Loading the watershed data (for local)
watershed_pesticide_dt = 
  read.fst(
    path = here("data-clean/watershed/watershed-pesticide-dt.fst"),
    as.data.table = TRUE
  )
# Loading weighted up/down table
pesticide_upstream_dt = 
  read.fst(
    path = here("data-clean/watershed/pesticide-upstream-dt.fst"),
    as.data.table = TRUE
  )
# Loading ML predictions 
prediction_dt = 
  read.fst(
    path = here('data-clean/ml-water/predictions-watershed.fst'), 
    as.data.table = TRUE
  )

# Loading the weights
pop_weights = 
  rbind(
  #  read.fst(
  #    here("data-clean/watershed/weights/hydrobasin-pop-weights1990.fst"), 
  #    as.data.table = TRUE
  #  ) |>  mutate(year = 1990L),
  #  read.fst(
  #    here("data-clean/watershed/weights/hydrobasin-pop-weights2000.fst"), 
  #    as.data.table = TRUE
  #  ) |>  mutate(year = 2000L),
    read.fst(
      here("data-clean/watershed/weights/hydrobasin-pop-weights2010.fst"), 
      as.data.table = TRUE
    )# |> mutate(year = 2010L)
  )

setnames(pop_weights,"geoid",'GEOID')
#pop_weights[,census_year := year]

# Making sure the weights are correctly calculated
pop_weights[,.(
  tot = sum(pop_weight,na.rm=T), 
  count = .N, 
  sum(is.nan(pop_weight))),
  by = .(GEOID)#,year)
] %>% 
  .[tot < 0.9999 | tot > 1.0001]

# Adding census year to pest/watershed data 
#pesticide_upstream_dt[,
#  census_year := case_when(
#    year %in% 1990:1999 ~ 1990L,
#    year %in% 1999:2009 ~ 2000L,
#    year %in% 2010:2019 ~ 2010L,
#    TRUE ~ year
#)]
pesticide_upstream_dt |> setkey(hybas_id, year, local, dist_km_bin)
pop_weights |> setkey(hybas_id)

# Merging pop weights with the upstream/downstream glyphosate data 
pesticide_watershed_pop_dt = 
  merge(
    pesticide_upstream_dt[local == FALSE],
    pop_weights,
    by = c("hybas_id"),
    allow.cartesian = TRUE
  )

# Columns we want to aggregate
col_vec_p = str_subset(
  colnames(pesticide_watershed_pop_dt),
  "awt|yield_diff|high_(kls|ppt|pct_irrigated)"
)

# Aggregate to the county level and were done!
county_exposure_up_down_dt = 
  pesticide_watershed_pop_dt[,
    lapply(.SD, \(x){sum(x*pop_weight, na.rm = TRUE)}),
    by = .(GEOID, local, year, dist_km_bin),
    .SDcols = col_vec_p
  ]|>
  dcast(
    GEOID + year ~ dist_km_bin,
    value.var = col_vec_p
  )

# Doing simpler calculations for local 
#watershed_pesticide_dt[, 
#  census_year := case_when(
#   year %in% 1990:1999 ~ 1990L,
#   year %in% 1999:2009 ~ 2000L,
#   year %in% 2010:2019 ~ 2010L,
#   TRUE ~ year
#)]

local_pop_watershed_dt = 
  merge(
    watershed_pesticide_dt,
    pop_weights, 
    by = c("hybas_id"),
    allow.cartesian = TRUE
  )

# Columns we want to aggregate
col_vec_w = str_subset(
  colnames(watershed_pesticide_dt),
  "awt|yield_diff|high_(kls|ppt|pct_irrigated)"
)

county_exposure_local_dt = 
  local_pop_watershed_dt[,
    lapply(.SD, \(x){sum(x*pop_weight, na.rm = TRUE)}),
    by = .(GEOID, year),
    .SDcols = col_vec_w
  ] 

# Adding local to the names 
setnames(
  county_exposure_local_dt, 
  old = col_vec_w,
  new = paste0(col_vec_w, "_local")
)

# Merging up and down with the local results
county_exposure_dt = 
  merge(
    county_exposure_up_down_dt,
    county_exposure_local_dt, 
    by = c("GEOID", "year"),
    all = T
  )

# Aggregating the predictions 
county_pred_dt = 
  merge(
    prediction_dt,
    pop_weights, 
    by = c("hybas_id"),
    allow.cartesian = TRUE
  )[,.(
    pred_glyph_in_water_lasso = fsum(pred_glyph_in_water_lasso*pop_weight),
    pred_ampa_in_water_lasso = fsum(pred_ampa_in_water_lasso*pop_weight),
    pred_glyph_in_water_rf = fsum(pred_glyph_in_water_rf*pop_weight),
    pred_ampa_in_water_rf = fsum(pred_ampa_in_water_rf*pop_weight)),  
    keyby = .(GEOID, year, month)
  ]

# Saving the results
write.fst(
  county_exposure_dt, 
  path = here("data-clean/watershed/county-exposure-dt.fst")
)
write.fst(
  county_pred_dt,
  path = here('data-clean/watershed/county-exposure-pred-dt.fst')
)
