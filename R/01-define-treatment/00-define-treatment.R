
# Notes ------------------------------------------------------------------------
#   Goal: Generate treatment and control groups based on GAEZ Attainable yield
#         Calculate percentiles for yeild diff 
#   Time:


# Setup ------------------------------------------------------------------------
  # Load packages
  library(pacman)
  p_load(
    fastverse, patchwork, fixest, tigris, dplyr, janitor,
    purrr, parallel, magrittr, here, fst
  )
  fastverse_extend(topics = c('ST', 'SP', 'VI'))
  # theme_set(hrbrthemes::theme_ipsum())
  options(tigris_use_cache = TRUE)

# Load data --------------------------------------------------------------------
  # GAEZ suitability data
  si_dt = read.fst(
    path = here("data/raw/y_diff_dt.fst"),
    as.data.table = TRUE
  )
  # County shapefiles
  county_sf =
    map_dfr(
      unique(si_dt[,.(state_fips = str_sub(GEOID,1,2))]$state_fips),
      counties,
      year = 2010,
      cb = TRUE
    ) |> 
    clean_names() |>
    mutate(GEOID = paste0(statefp,countyfp))  |>
    st_transform(crs = 2163) 
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
    st_transform(crs = 2163)  
    
# Clean suitability data -------------------------------------------------------
  # Finding counties east of the 100th meridian
  east_100m = tibble(st_join(meridian_sf, county_sf))$GEOID
  si_dt[,e100m := GEOID %in% east_100m]
  # Calculating average yield diff across GM crops (soy/corn/cotton)
  si_dt = 
    rbind(
      si_dt[crop != "gmo"],
      si_dt[
        crop %in% c("soy","mze","cot"),
        .(GEOID, e100m,
          yield_diff_std = 
            (yield_diff-mean(yield_diff,na.rm = TRUE))/sd(yield_diff, na.rm = TRUE)
        ),
        by=crop
      ][,.(
          crop = "gmo",
          yield_high = NA, 
          yield_low = NA, 
          yield_diff = mean(yield_diff_std, na.rm = TRUE)
        ), 
        by = .(GEOID, e100m)
      ],
      use.names = TRUE
    )
  # Calculate pecentile for crop
  # First for entire country
  si_dt[,':='(
    all_yield_high_percentile = frank(yield_high)/.N,
    all_yield_low_percentile = frank(yield_low)/.N,
    all_yield_diff_percentile = frank(yield_diff)/.N), 
    by = .(crop)
  ]
  # Now for east of the 100th meridian
  si_dt[,':='(
    e100m_yield_high_percentile = frank(yield_high)/.N,
    e100m_yield_low_percentile = frank(yield_low)/.N,
    e100m_yield_diff_percentile = frank(yield_diff)/.N), 
    by = .(crop, e100m)
  ]
  # Setting to NA for west
  si_dt[,':='(
    e100m_yield_high_percentile = fifelse(e100m == TRUE, e100m_yield_high_percentile, NA_real_),
    e100m_yield_low_percentile = fifelse(e100m == TRUE, e100m_yield_low_percentile, NA_real_),
    e100m_yield_diff_percentile = fifelse(e100m == TRUE, e100m_yield_diff_percentile, NA_real_)
  )]
  # Creating indicator for GM as average of soy, corn, cotton percentile
  gm_perc_dt = 
    si_dt[
      crop %in% c('cot','mze','soy'),.(
        crop = 'gmo',
        all_yield_diff_percentile_mean = mean(all_yield_diff_percentile),
        e100m_yield_diff_percentile_mean = mean(e100m_yield_diff_percentile),
        all_yield_diff_percentile_max = max(all_yield_diff_percentile),
        e100m_yield_diff_percentile_max = max(e100m_yield_diff_percentile)),
      keyby = .(GEOID, e100m)
    ][,':='(
      all_yield_diff_percentile_mean = frank(all_yield_diff_percentile_mean)/.N,
      all_yield_diff_percentile_max = frank(all_yield_diff_percentile_max)/.N), 
      by = .(crop)
    ][,':='(
      e100m_yield_diff_percentile_mean = frank(e100m_yield_diff_percentile_mean)/.N,
      e100m_yield_diff_percentile_max = frank(e100m_yield_diff_percentile_max)/.N), 
      by = .(crop, e100m)
    ][,':='(
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
      id.vars = c('GEOID','e100m','crop'),
      measure = patterns(
        all_yield_diff_percentile = '^all_yield_diff_percentile_', 
        e100m_yield_diff_percentile = '^e100m_yield_diff_percentile_'
      )
    ) %>% .[,':='(
      crop = ifelse(variable == 1, 'gmo', 'gmo_max'),
      variable = NULL
    )]
  # Adding back into table 
  si_dt = 
    rbind(
      si_dt[str_detect(crop,'gmo', negate = TRUE)],
      gm_perc_dt, 
      use.names = TRUE, 
      fill = TRUE
    )
  # Saving table but with gmo and e100 indicators
  write.fst(
    si_dt, 
    path = here("data/raw/y_diff_e100m_dt.fst")
  )

# Define treatment variable(s) -------------------------------------------------
  # Define potential cut points
  splits = seq(0.25, 0.75, by = 0.05)
  splits_p = percent(splits, accuracy = 1) |> str_remove("%")
  # Buffers: Always include 0 and only use even numbers
  buffers = c(0,0.1,0.5) 
  
  # Cartesian join of splits and buffers
  splits_buffers = CJ(splits, buffers)[,':='(
    split_l = splits - buffers/2, 
    split_h = splits + buffers/2,
    splits_p = percent(splits, accuracy = 1) |> str_remove("%"),
    buffers_p = percent(buffers, accuracy = 1) |> str_remove("%")
  )][,':='(
    split_l_p = percent(split_l, accuracy = 1) |> str_remove("%"),
    split_h_p = percent(split_h, accuracy = 1) |> str_remove("%")
  )]
  
  # Finding the unique quantiles we have to calculate
  all_splits_p = c(splits, splits_buffers$split_l, splits_buffers$split_h) |>
    percent(accuracy = 1) |> str_remove("%") |> sort() |> unique()
  all_splits = as.numeric(all_splits_p)/100

  # Option 1: Quantiles for ALL of US
  # yield_diff_quant = 
  #   si_dt[,.(
  #     quantile = all_splits_p,
  #     value_all = quantile(
  #       yield_diff, 
  #       probs = all_splits,
  #       na.rm = TRUE
  #     )),
  #     by = crop
  #   ]
  # # Option 2: Only counties that are east of 100th meridian
  # e100m_yield_diff_quant = 
  #   si_dt[
  #     e100m == TRUE,.(
  #     quantile = all_splits_p,
  #     value_e100m = quantile(
  #       yield_diff, 
  #       probs = all_splits,
  #       na.rm = TRUE
  #     )),
  #     by = crop
  #   ]
  
  # Crops we want to keep
  crop_vec = c("gmo","soy","cot","mze","olv",'gmo_max','wpo','cab','srg','bck')
  # Potential placebo crops: "srg","sub","yam","rcw","olv","bck"
  # Calculating correlation between 
  #merge(
  #  si_dt[e100m == TRUE,.(GEOID, crop, yield_diff)] |> na.omit(), 
  #  si_dt[e100m == TRUE & crop %in% crop_vec] |> 
  #    dcast(GEOID ~ crop, value.var = "yield_diff") |> 
  #    na.omit(),
  #  by = "GEOID"
  #)[,.(
  #  cor_cot = cor(yield_diff, cot),
  #  cor_soy = cor(yield_diff, soy),
  #  cor_mze = cor(yield_diff, mze),
  #  cor_gmo = cor(yield_diff, gmo)
  #  ), 
  #  keyby = crop
  #][order(cor_gmo)]
  
  # Calculating above/below quantiles 
  trt_long_dt = # First cross joining w/splits buffers table
    splits_buffers[,
      as.list(si_dt[crop %in% crop_vec]),
      by=splits_buffers
    ][,':='(
      all_yield_diff = fcase(
          all_yield_diff_percentile >= split_h, TRUE,
          all_yield_diff_percentile < split_l, FALSE,
          default = NA
        ),
        e100m_yield_diff = fcase(
          e100m_yield_diff_percentile >= split_h, TRUE,
          e100m_yield_diff_percentile < split_l, FALSE,
          default = NA
        )
    )]
    # |>
    #   merge( # add lower bound quantiles
    #     yield_diff_quant[
    #       quantile %in% splits_buffers$split_l_p,
    #       .(crop, split_l_p = quantile, value_all_l = value_all)
    #     ],
    #     by = c("crop","split_l_p")
    #   ) |>
    #   merge(
    #     e100m_yield_diff_quant[
    #       quantile %in% splits_buffers$split_l_p,
    #       .(crop, split_l_p = quantile, value_e100m_l = value_e100m)
    #     ],
    #     by = c("crop","split_l_p")
    #   )|>
    #   merge( # add upper bound quantiles
    #     yield_diff_quant[
    #       quantile %in% splits_buffers$split_h_p,
    #       .(crop, split_h_p = quantile, value_all_h = value_all)
    #     ],
    #     by = c("crop","split_h_p"),
    #     allow.cartesian = TRUE
    #   ) |>
    #   merge(
    #     e100m_yield_diff_quant[
    #       quantile %in% splits_buffers$split_h_p,
    #       .(crop, split_h_p = quantile, value_e100m_h = value_e100m)
    #     ],
    #     by = c("crop","split_h_p")
    #   ) %>% .[,':='(
    #     all_yield_diff = fcase(
    #       yield_diff >= value_all_h, TRUE,
    #       yield_diff < value_all_l, FALSE,
    #       default = NA
    #     ),
    #     e100m_yield_diff = fcase(
    #       e100m == TRUE & yield_diff >= value_e100m_h, TRUE,
    #       e100m == TRUE & yield_diff < value_e100m_l, FALSE,
    #       default = NA
    #     ),
    #  )]  
  # An alternative way to calculate GM aggregate
  # If above cutoff for any of soy, corn, cotton
  trt_med_dt =   
    dcast(
      trt_long_dt,
      formula = GEOID + e100m + splits_p + buffers_p ~ crop,
      value.var = c("all_yield_diff","e100m_yield_diff")
    ) #%>% 
    #.[,':='(
    #  all_yield_diff_gm = all_yield_diff_soy|all_yield_diff_cot|all_yield_diff_mze,
    #  e100m_yield_diff_gm = e100m_yield_diff_soy|e100m_yield_diff_cot|e100m_yield_diff_mze
    #)] 
  
  # casting into wide format 
  trt_dt = 
    dcast(
      trt_med_dt, 
      formula = GEOID + e100m ~ splits_p + buffers_p,
      value.var = colnames(trt_med_dt)[-c(1:4)]
    ) |>
    # Merging with continuous measures
    merge(
      dcast(
        si_dt[crop %in% crop_vec],
        formula = GEOID ~ crop,
        value.var = c('all_yield_diff_percentile','e100m_yield_diff_percentile')
      ),
      by = c('GEOID')
    )  
  # Save the treatment definitions 
  write_fst(
    x = trt_dt,
    path = here('data/clean/trt-dt.fst')
  )

# Plot treatment definitions ---------------------------------------------------
  #  # Merge treatment/suitability dataset with county data
  #  trt_sf = 
  #    left_join(
  #      county_sf,
  #      trt_dt,
  #      by = "GEOID"
  #    )
  #  # Map: T/C for all US (median cut)
  #  p1 = ggplot(data = trt_sf, aes(fill = as.character(all_yield_diff_gmo_50_0))) +
  #    geom_sf(size = 0.1, color = NA) +
  #    scale_fill_manual(
  #      name = "", 
  #      values = c("#5f6880","#ec7662"),
  #      labels = c("Control","Treated"),
  #      na.value = "#B4BCC2"
  #    ) +
  #    theme_minimal()
  #  # Map: T/C east of 100th meridian (median cut)
  #  p2 = ggplot(data = trt_sf, aes(fill = as.character(e100m_yield_diff_gmo_50_0))) +
  #    geom_sf(size = 0.1, color = NA) + 
  #    scale_fill_manual(
  #      name = "", 
  #      values = c("#5f6880","#ec7662"),
  #      labels = c("Control","Treated"),
  #      na.value = "#B4BCC2"
  #    ) +
  #    theme_minimal()
  #  # Map: T/C east of 100th meridian (median cut) with 'separation'
  #  p3 = ggplot(data = trt_sf, aes(fill = as.character(e100m_yield_diff_gmo_50_10))) +
  #    geom_sf(size = 0.1, color = NA) + 
  #    scale_fill_manual(
  #      name = "", 
  #      values = c("#5f6880","#ec7662"),
  #      labels = c("Control","Treated"),
  #      na.value = "#B4BCC2"
  #    ) +
  #    theme_minimal()
   # Plot together
   #p1 / p2 / p3
   #ggsave(
   #  plot = p1,
   #  filename = here("figures/did/trt-yield-diff-gmo-50-0.jpeg"),
   #  height = 4.5, width = 8
   #)
   #ggsave(
   #  plot = p2,
   #  filename = here("figures/did/trt-e100-yield-diff-gmo-50-0.jpeg"),
   #  height = 4.5, width = 8
   #)
   #ggsave(
   #  plot = p3,
   #  filename = here("figures/did/trt-e100-yield-diff-gmo-50-10.jpeg"),
   #  height = 4.5, width = 8
   #)
   #ggplot(data = trt_sf, aes(fill = as.character(e100m_yield_diff_olv_75_0))) +
   #  geom_sf(size = 0.1, color = NA) + 
   #  scale_fill_manual(
   #    name = "", 
   #    values = c("#5f6880","#ec7662"),
   #    labels = c("Control","Treated"),
   #    na.value = "#B4BCC2"
   #  ) +
   #  theme_minimal()