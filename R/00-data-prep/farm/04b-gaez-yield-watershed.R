# Aggregating GAEZ by watershed
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

# Now doing it again for watersheds
  # Hydrobasin data
  hydrobasin_sf = 
    read_sf(here("data/watersheds/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
    clean_names() |>
    filter(lake == 0) |>
    st_make_valid()
  # Loading the suitability index data
  aggregate_gaez_watershed = function(crop){
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
    y_hybas_mean = exact_extract(
      x = yield_rast, 
      y = hydrobasin_sf, 
      fun = 'mean'
    )
    # Cleaning up 
    y_diff_dt = 
      data.table(
        hybas_id = hydrobasin_sf$hybas_id,
        y_hybas_mean
      ) |> 
      setnames(c('hybas_id','yield_low','yield_high'))
    y_diff_dt[,':='(
        crop = crop,
        yield_diff = fifelse(yield_high - yield_low >= 0, yield_high - yield_low, 0)
    )]
    # Saving the results 
    write.fst(
      y_diff_dt, 
      path = here(paste0(
        "data/watershed/attainable-yield-hybas/y-diff-",
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
    "data/watershed/attainable-yield-hybas"
    )) |>
    str_extract("(?<=y-diff-)\\w{3}(?=-dt)") |>
    unique()
  # Running!
  plan(multisession(workers = 4))
  y_diff_dt  =
    future_map_dfr(
      crop_list[!(crop_list %in% already_run)],
      aggregate_gaez_watershed
    )
  # Reading the results 
  y_diff_dt = 
    map_dfr(
      list.files(
        here("data/watershed/attainable-yield-hybas"),
        full.names = TRUE
      ),
      \(x){read.fst(path = x, as.data.table = TRUE)}
    )
  # Calculate pecentile for crop
  # State shapefile
  states_sf = 
    states(
      year = 2010,
      cb = TRUE  
    ) |>
    filter( # Limiting to continental US
      !(STATE %in% c("02","15","60","66","69","72","78"))
    ) |>
    clean_names()
  # Create 100th meridian dataset 
  meridian_sf  = 
    data.frame(
      name = "100th meridian",
      geom = st_polygon(
        x = list(tibble(
          long = c(-100,-100,-80, -60, -60, -100),
          lat = c(50, 20, 20, 20, 50, 50)
        ) |> as.matrix())
      ) |> st_sfc()
    ) |>
    st_as_sf() |>
    st_set_crs(value = st_crs(states_sf)) |>
    st_transform(crs = y_crs)  
  states_sf %<>% st_transform(crs = st_crs(hydrobasin_sf))
  sf_use_s2(FALSE)
  # Finding counties east of the 100th meridian in continental US
  hybas_id_in_us = tibble(st_join(states_sf, hydrobasin_sf))$hybas_id
  hybas_id_e100m = tibble(st_join(meridian_sf, hydrobasin_sf))$hybas_id
  y_diff_dt[,in_us := hybas_id %in% hybas_id_in_us]
  y_diff_dt[,e100m := hybas_id %in% hybas_id_e100m & in_us == TRUE]
  # First for entire country
  y_diff_dt[,':='(
    all_yield_high_percentile = frank(yield_high)/.N,
    all_yield_low_percentile = frank(yield_low)/.N,
    all_yield_diff_percentile = frank(yield_diff)/.N), 
    by = .(crop, in_us)
  ]
  # Now for east of the 100th meridian
  y_diff_dt[,':='(
    e100m_yield_high_percentile = frank(yield_high)/.N,
    e100m_yield_low_percentile = frank(yield_low)/.N,
    e100m_yield_diff_percentile = frank(yield_diff)/.N), 
    by = .(crop, e100m)
  ]
  # Setting to NA for west/not in US
  y_diff_dt[,':='(
    all_yield_high_percentile = fifelse(in_us == TRUE, all_yield_high_percentile, NA_real_),
    all_yield_low_percentile = fifelse(in_us == TRUE, all_yield_low_percentile, NA_real_),
    all_yield_diff_percentile = fifelse(in_us == TRUE, all_yield_diff_percentile, NA_real_),
    e100m_yield_high_percentile = fifelse(e100m == TRUE, e100m_yield_high_percentile, NA_real_),
    e100m_yield_low_percentile = fifelse(e100m == TRUE, e100m_yield_low_percentile, NA_real_),
    e100m_yield_diff_percentile = fifelse(e100m == TRUE, e100m_yield_diff_percentile, NA_real_)
  )]
  # Adding gmo as the mean of soy/corn/cotton, then converted into a percentile
  y_diff_dt = 
    rbind(
      y_diff_dt[str_detect(crop,'gmo', negate = TRUE)],
      y_diff_dt[
        crop %in% c('cot','mze','soy'),.(
          crop = 'gmo',
          all_yield_diff_percentile_mean = mean(all_yield_diff_percentile),
          e100m_yield_diff_percentile_mean = mean(e100m_yield_diff_percentile),
          all_yield_diff_percentile_max = max(all_yield_diff_percentile),
          e100m_yield_diff_percentile_max = max(e100m_yield_diff_percentile)),
        keyby = .(hybas_id, e100m, in_us)
      ][,':='(
        all_yield_diff_percentile_mean = frank(all_yield_diff_percentile_mean)/.N,
        all_yield_diff_percentile_max = frank(all_yield_diff_percentile_max)/.N), 
        by = .(crop, in_us)
      ][,':='(
        e100m_yield_diff_percentile_mean = frank(e100m_yield_diff_percentile_mean)/.N,
        e100m_yield_diff_percentile_max = frank(e100m_yield_diff_percentile_max)/.N), 
        by = .(crop, e100m)
      ][,':='(
        all_yield_diff_percentile_mean = fifelse(
          in_us == TRUE, 
          all_yield_diff_percentile_mean, 
          NA_real_
        ),
        all_yield_diff_percentile_max = fifelse(
          in_us == TRUE, 
          all_yield_diff_percentile_max, 
          NA_real_
        ),
        e100m_yield_diff_percentile_mean = fifelse(
          e100m == TRUE, 
          e100m_yield_diff_percentile_mean, 
          NA_real_
        ),
        e100m_yield_diff_percentile_max = fifelse(
          e100m == TRUE, 
          e100m_yield_diff_percentile_max, 
          NA_real_
        )
      )] |>
      melt(
        id.vars = c('hybas_id','e100m','in_us','crop'),
        measure = patterns(
          all_yield_diff_percentile = '^all_yield_diff_percentile_', 
          e100m_yield_diff_percentile = '^e100m_yield_diff_percentile_'
        )
      ) %>% .[,':='(
        crop = ifelse(variable == 1, 'gmo', 'gmo_max'),
        variable = NULL
      )],
      fill = TRUE,
      use.names = TRUE
    )
  # Saving the result   
  write.fst(
    y_diff_dt, 
    here("data/watershed/hydrobasin-y-diff-dt.fst")
  )

