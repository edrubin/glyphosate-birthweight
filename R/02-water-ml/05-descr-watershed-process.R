# -----------------------------------------------------------------------------
# This script has all of the plots showing process for watersheds
# -----------------------------------------------------------------------------
  # Setup
  pacman::p_load(
    data.table, here, fixest, ggplot2, purrr, 
    tigris, magrittr, sf, tidyr, dplyr, 
    janitor, stars, stringr, tictoc, furrr, fst, collapse,
    units, gridExtra
  )
  options(
    tigris_use_cache=TRUE,
    scipen = 999
  )
  # Setting the theme 
  theme_set(
    theme_void(base_size = 14) +
      theme(
        legend.position = 'bottom',
        plot.margin = margin(0,0,0,0),
        legend.key.width = unit(8/6, 'cm'),
        legend.text = element_text(size = 18)
      )
  )

# Loading data ----------------------------------------------------------------
  # Loading HydroBASINS data 
  hydrobasin_detailed_sf = 
    read_sf(here("data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
    st_transform(crs = 2163) |>
    clean_names()
  # Simplifying to plot faster nationally
  hydrobasin_sf = st_simplify(
    hydrobasin_detailed_sf, 
    preserveTopology = FALSE, 
    dTolerance = 0.01
  )
  # Getting state shapes from tigris package
  states_sf = 
    states(year = 2010, cb = TRUE) |>
    filter( # Limiting to continental US
      !(STATE %in% c("02","15","60","66","69","72","78"))
    ) |>
    clean_names()
  # Loading county shapes
  county_sf =
    map_dfr(
      states_sf$state,
      counties,
      year = 2010,
      cb = TRUE
    ) |> 
    clean_names() |>
    st_simplify(dTolerance = 0.001) |>
    mutate(
      GEOID = paste0(statefp,countyfp),
      area_km2 = st_area(geometry) |> set_units(km^2) |> drop_units()
    ) |>
    st_transform(crs = 2163) 
  # Loading area based weights 
  area_weight_dt = 
    read_fst(
      here("data-clean/watershed/weights/hydrobasin-area-weights.fst"),
      as.data.table = TRUE
    ) |>
    setnames(old = "watershed_weight",new = "area_weight") |>
    setkey(hybas_id,geoid)
  # Loading local disaggregated data
  watershed_pesticide_dt = 
    read.fst(
      path = here("data-clean/watershed/watershed-pesticide-dt.fst"),
      as.data.table = TRUE
    )
  # Upstream/downstream watersheds
  upstream_dt = read.fst(
    path = here("data-clean/watershed/upstream-dt-hydrobasin.fst"), 
    as.data.table = TRUE
  )
  # Creating precip and soil erodibility percentiles 
  watershed_pesticide_dt[, ':='(
    kls_percentile = frank(kls)/.N,
    ppt_growing_season_percentile = frank(ppt_growing_season)/.N,
    kffact_percentile = frank(kffact)/.N,
    slopelen_percentile = frank(-slopelen)/.N,
    ppt_off_season_percentile = frank(ppt_off_season)/.N
  )]
  # Merging with annual data 
  soil_sf = 
    merge(
      hydrobasin_sf,
      watershed_pesticide_dt[, .(
        hybas_id, year, e100m, 
        # ML model inputs
        pred_glyphosate_awt,
        pred_glyph_km2_awt,
        kffact, kffact_percentile, 
        slopelen, slopelen_percentile,
        ppt_growing_season, ppt_growing_season_percentile, 
        # Treatment 
        e100m_yield_diff_percentile_gmo,
        all_yield_diff_percentile_gmo,
        percentile_gm_acres,
        # Others
        kls, kls_percentile,
        ppt_off_season, ppt_off_season_percentile
      )],
      by = "hybas_id",
      all.x = TRUE
    )

# Input plots -----------------------------------------------------------------
  # Predicted glyphosate
  pred_glyph_04_p = 
    ggplot() + 
    geom_sf(
      data = soil_sf |> filter(year == 2004), 
      aes(
        fill = pred_glyph_km2_awt, 
        color = pred_glyph_km2_awt 
      )
    ) +
    geom_sf(data = states_sf, fill = NA, color = 'black') + 
    scale_fill_viridis_c(
      '', 
      #breaks = seq(0,1,0.25),
      #labels = scales::label_percent(), 
      aesthetics = c('fill','color')
    )
  ggsave(
    pred_glyph_04_p,
    filename = here("figures/watershed/pred-glyph-km2-2004.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
  # Soil erodibility and slope
  kffact_p = 
    ggplot() + 
    geom_sf(
      data = soil_sf |> filter(year == 1996), 
      aes(
        fill = kffact_percentile, #ifelse(e100m == TRUE, kffact, NA),
        color = kffact_percentile #ifelse(e100m == TRUE, kffact, NA)
      )
    ) +
    geom_sf(data = states_sf, fill = NA, color = 'black') + 
    scale_fill_viridis_c(
      '', 
      breaks = seq(0,1,0.25),
      labels = scales::label_percent(), 
      aesthetics = c('fill','color')
    )
  ggsave(
    kffact_p,
    filename = here("figures/watershed/kffact.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
  slopelen_p = 
    ggplot() + 
    geom_sf(
      data = soil_sf |> filter(year == 1996), 
      aes(
        fill = slopelen_percentile, #ifelse(e100m == TRUE, slopelen, NA),
        color = slopelen_percentile #ifelse(e100m == TRUE, slopelen, NA)
      )
    ) +
    geom_sf(data = states_sf, fill = NA, color = 'black') + 
    scale_fill_viridis_c(
      '', 
      breaks = seq(0,1,0.25),
      labels = scales::label_percent(), 
      aesthetics = c('fill','color')
    )
  ggsave(
    slopelen_p,
    filename = here("figures/watershed/slopelen.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
  kls_p = 
    ggplot() + 
    geom_sf(
      data = soil_sf |> filter(year == 1996), 
      aes(
        fill = kls_percentile, #ifelse(e100m == TRUE, kls, NA),
        color = kls_percentile #ifelse(e100m == TRUE, kls, NA)
      )
    ) +
    geom_sf(data = states_sf, fill = NA, color = 'black') + 
    scale_fill_viridis_c(
      '', 
      breaks = seq(0,1,0.25),
      labels = scales::label_percent(), 
      aesthetics = c('fill','color')
    )
  ggsave(
    kls_p,
    filename = here("figures/watershed/kls.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
  # Precipitation
  ppt_growing_04_p = 
    ggplot() + 
    geom_sf(
      data = soil_sf |> filter(year == 2004), 
      aes(
        fill = ppt_growing_season,#+ ppt_off_season, 
        color = ppt_growing_season# + ppt_off_season 
      )
    ) +
    geom_sf(data = states_sf, fill = NA, color = 'black') + 
    scale_fill_viridis_c(
      '', 
      #breaks = seq(0,1,0.25),
      #labels = scales::label_percent(), 
      aesthetics = c('fill','color')
    )
  ggsave(
    ppt_growing_04_p,
    filename = here("figures/watershed/ppt-growing-2004.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
  ppt_off_04_p = 
    ggplot() + 
    geom_sf(
      data = soil_sf |> filter(year == 2004), 
      aes(
        fill =  ppt_off_season, 
        color =  ppt_off_season 
      )
    ) +
    geom_sf(data = states_sf, fill = NA, color = 'black') + 
    scale_fill_viridis_c(
      '', 
      #breaks = seq(0,1,0.25),
      #labels = scales::label_percent(), 
      aesthetics = c('fill','color')
    )
  ggsave(
    ppt_off_04_p,
    filename = here("figures/watershed/ppt-off-2004.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
  ppt_04_p = 
    ggplot() + 
    geom_sf(
      data = soil_sf |> filter(year == 2004), 
      aes(
        fill = ppt_growing_season + ppt_off_season, 
        color = ppt_growing_season + ppt_off_season 
      )
    ) +
    geom_sf(data = states_sf, fill = NA, color = 'black') + 
    scale_fill_viridis_c(
      '', 
      #breaks = seq(0,1,0.25),
      #labels = scales::label_percent(), 
      aesthetics = c('fill','color')
    )
  ggsave(
    ppt_04_p,
    filename = here("figures/watershed/ppt-2004.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )

# Individual watershed example ------------------------------------------------
  x_cnty = "17189"
  x_options = area_weight_dt[geoid == x_cnty & year == 2000]$hybas_id
  x = 7080577740
  wshd_example =   
    hydrobasin_sf |>
    inner_join(
      upstream_dt[hybas_id == x],
      by= c("hybas_id"="hybas_id2")
    ) |>
    mutate(
      dist_wt_n = case_when(
        local == TRUE ~ "00 - Local",
        dist_wt_05 == 1 ~ "05",
        dist_wt_10 == 1 ~ "10",
        dist_wt_15 == 1 ~ "15",
        TRUE ~ "Farther"
      ),
      dist_km_bin_n = case_when(
        dist_km_bin == 'dnInf'~"(-Inf,-100)",
        dist_km_bin == 'dn100'~'[-100, -50)',
        dist_km_bin == 'dn50'~'[-50, 0)',
        dist_km_bin == 'd0'~'[0, 50)',
        dist_km_bin == 'd50'~'[050, 100)',
        dist_km_bin == 'd100'~'[100, 150)',
        dist_km_bin == 'd150'~'[150, 200)',
        dist_km_bin == 'd200'~'[200, 250)',
        dist_km_bin == 'd250'~'[250, 300)',
        dist_km_bin == 'd300'~'[300, Inf)'
      )
    )
  # Plotting upstream vs downstream watersheds
  downstream_example_map = 
    ggplot() + 
    geom_sf(data = states_sf |> 
              filter(name %in% c(
                "Kentucky","Tennessee","Missouri","Arkansas","Indiana","Illinois",
                "Louisiana","Mississippi","Alabama")),
            fill = "white", color = "black"
    ) +
    geom_sf(data = wshd_example, aes(fill = upstream), color = NA) +
    geom_sf(data = hydrobasin_sf |> filter(hybas_id == x), fill = "red", color = "red") +
    scale_fill_manual(
      values = c("#ffcc77","#4682b4"), 
      labels = c("Upstream","Downstream"), 
      name=""
    )+
    coord_sf(crs = 2163)
  ggsave(
    downstream_example_map,
    filename = here("figures/watershed/downstream-example-map.jpeg"),
    width = 7, height = 9,
    bg = 'white'
  )
  # Now distance bins as well 
  dist_bin_example_map = 
    ggplot() + 
    geom_sf(
      data = states_sf |> 
        filter(name %in% c(
          "Kentucky","Tennessee","Missouri","Arkansas","Indiana","Illinois",
          "Louisiana","Mississippi","Alabama")),
      fill = "white", color = "black"
    ) +
    geom_sf(data = wshd_example, aes(fill = dist_km_bin_n), color = NA) +
    geom_sf(data = hydrobasin_sf |> filter(hybas_id == x), fill = "red", color = "red") +
    scale_fill_viridis_d(name="")+
    coord_sf(crs = 2163) + 
    theme(legend.text = element_text(size = 14))
  ggsave(
    dist_bin_example_map,
    filename = here("figures/watershed/distance-bin-example-map.jpeg"),
    width = 7, height = 9,
    bg = 'white'
  )

# Now plotting weighting scheme for example -----------------------------------
  pesticide_upstream_dt = read.fst(
    path = here("data-clean/watershed/pesticide-upstream-dt.fst"), 
    as.data.table = TRUE
  )
  # Loading the weights
  pop_weights = 
    rbind(
      #read.fst(
      #  here("data-clean/watershed/weights/hydrobasin-pop-weights1990.fst"), 
      #  as.data.table = TRUE
      #) |>  mutate(year = 1990L),
      #read.fst(
      #  here("data-clean/watershed/weights/hydrobasin-pop-weights2000.fst"), 
      #  as.data.table = TRUE
      #) |>  mutate(year = 2000L),
      read.fst(
        here("data-clean/watershed/weights/hydrobasin-pop-weights2010.fst"), 
        as.data.table = TRUE
      ) |> mutate(year = 2010L)
    )
  # Making sure the weights are correctly calculated
  pop_weights[,.(
    tot = sum(pop_weight,na.rm=T), 
    count = .N, 
    sum(is.nan(pop_weight))),
    by = .(geoid,year)
  ] %>% 
    .[tot < 0.9999 | tot > 1.0001]
  # Adding census year to pest/watershed data 
  pesticide_upstream_dt[,
    census_year := case_when(
      year %in% 1990:1999 ~ 1990L,
      year %in% 1999:2009 ~ 2000L,
      year %in% 2010:2019 ~ 2010L,
      TRUE ~ year
    )
  ]
  pesticide_upstream_dt |> setkey(hybas_id,year, dist_km_bin)
  pop_weights |> setkey(hybas_id,year)
  # Merging pop weights with the upstream/downstream glyphosate data 
  pesticide_watershed_pop_dt = 
    merge(
      pesticide_upstream_dt,
      pop_weights,
      by.x = c("hybas_id"),#,"census_year"),
      by.y =c("hybas_id"),#"year"),
      allow.cartesian = TRUE
    )
  # Plotting the example watershed
  pop_weight_ex_sf = 
    inner_join(
      hydrobasin_detailed_sf,
      pop_weights[geoid == x_cnty & year == 2010],
      by = "hybas_id"
    ) |>
    left_join(
      pesticide_upstream_dt[year == 2006 & dist_km_bin == 'd50'],
      by = "hybas_id"
    )
  pop_wt_cnty_ex_p = 
    ggplot() + 
    geom_sf(
      data = pop_weight_ex_sf, 
      aes(fill = pop_weight, color = pop_weight)
    ) +
    geom_sf(
      data = county_sf |> filter(GEOID == x_cnty), 
      fill = NA, 
      color = "black", 
      linewidth = 1.1
    ) +
    geom_sf(
      data = hydrobasin_detailed_sf |> filter(hybas_id == x), 
      fill = NA, 
      color = "red", 
      linewidth = 1.1
    ) +
    scale_fill_viridis_c(
      "Pop Weight", 
      begin = 0.2, 
      aesthetics = c('fill','color')
    ) +
    coord_sf(crs = 2163) 
  up_glyph_cnty_ex_p = 
    ggplot() + 
    geom_sf(
      data = pop_weight_ex_sf, 
      aes(fill = glyph_km2_awt, color = glyph_km2_awt)
    ) +
    geom_sf(
      data = hydrobasin_detailed_sf |> filter(hybas_id == x), 
      fill = NA, 
      color = "red", 
      linewidth = 1.1
    ) +
    geom_sf(
      data = county_sf |> filter(GEOID == x_cnty), 
      fill = NA, 
      color = "black", 
      linewidth = 1.1
    ) +
    scale_fill_viridis_c(
      "Pred Glyph", 
      begin = 0.2, 
      aesthetics = c('fill','color')
    ) +
    coord_sf(crs = 2163) 
  ggsave(
    filename = here("figures/watershed/washington_county_example.jpeg"),
    plot = grid.arrange(pop_wt_cnty_ex_p,up_glyph_cnty_ex_p, ncol = 2),
    height = 7, width = 15
  )

# Now for a map of county level predictions -----------------------------------
  county_exposure_dt = read.fst(
    path = here("data-clean/watershed/county-exposure-pred-dt.fst"), 
    as.data.table = TRUE
  )
  # Make a map of county level exposure in 2006 
  county_exposure_sf =   
    county_sf |>
    left_join(
      county_exposure_dt[
        year == 2004 & month == '07',.(
          GEOID, 
          pred_glyph = pred_glyph_in_water, 
          pred_ampa = pred_ampa_in_water,
          pred_glyph_percentile = frank(pred_glyph_in_water)/.N,
          pred_ampa_percentile = frank(pred_ampa_in_water)/.N
      )],
      by = c("GEOID")
    ) |>
    mutate(
      pred_glyph_percentile = ifelse(is.na(pred_glyph_percentile), 0, pred_glyph_percentile),
      pred_ampa_percentile = ifelse(is.na(pred_ampa_percentile), 0, pred_ampa_percentile),
      pred_glyph = ifelse(is.na(pred_glyph), 0, pred_glyph),
      pred_ampa = ifelse(is.na(pred_ampa), 0, pred_ampa)
    )
  # Map of AMPA
  pred_ampa_cnty_p = 
    ggplot() + 
    geom_sf(
      data = county_exposure_sf, 
      aes(fill = pred_ampa, color = pred_ampa)
    ) +
    geom_sf(data = states_sf, fill = NA, color ="black") +
    scale_fill_viridis_c(
      name = "", 
      aesthetics = c('fill', 'color')
    ) 
  ggsave(
    pred_ampa_cnty_p,
    filename = here("figures/watershed/pred-ampa-county-2004-07.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
  # Map of glyphosate
  pred_glyph_cnty_p = 
    ggplot() + 
    geom_sf(
      data = county_exposure_sf, 
      aes(fill = pred_glyph, color = pred_glyph)
    ) +
    geom_sf(data = states_sf, fill = NA, color ="black") +
    scale_fill_viridis_c(
      name = "", 
      aesthetics = c('fill', 'color')
    ) 
  ggsave(
    pred_glyph_cnty_p,
    filename = here("figures/watershed/pred-glyph-county-2004-07.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
