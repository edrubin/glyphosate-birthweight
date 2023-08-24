# --------------------------------------------------------------
# Aggregate glyph/trt vars by upstream/downstream
# Runtime: <1 min
# --------------------------------------------------------------
library(pacman)
p_load(
  data.table, here, fixest, ggplot2, purrr, 
  tigris, magrittr, sf, tidyr, dplyr, 
  janitor, stars, stringr, tictoc, furrr, fst, collapse,
  units
)

options(
  tigris_use_cache=TRUE,
  scipen = 999
)

# TODO: CSDL crop weighting

# --------------------------------------------------------------
# First, we need to get variables for each watershed,
# using one of the weights (area or crop) already calculated
# --------------------------------------------------------------
  # Loading area based weights 
  area_weight_dt = 
    read_fst(
      path = here("data-clean/watershed/weights/hydrobasin-area-weights.fst"),
      as.data.table = TRUE
    ) |>
    setnames(
      old = c("geoid","watershed_weight"),
      new = c("GEOID","area_weight")
    ) |>
    setkey(hybas_id, GEOID)
  # Loading HydroBASINS data limiting to those in the continental US
  hydrobasin_sf = 
    read_sf(here("data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
    st_transform(crs = 2163) |>
    clean_names()
  # Turning it into a data table to run faster
  hydrobasin_dt = 
    hydrobasin_sf |>
    mutate(hybas_area_km2 = st_area(geometry) |> set_units(km^2) |> drop_units())|>
    data.table() %>% 
    .[,geometry:=NULL]
  # Loading glyph, pred glyph, and treatment definitions 
  fs_dt = read.fst(here('data-clean/fs-dt.fst'), as.data.table = TRUE) 
  trt_dt = read.fst(here('data-clean/trt-dt.fst'), as.data.table = TRUE)
  #crop_dt = read.fst(path = here('data/crops/crop-acre-percentile-90-95.fst'), as.data.table = TRUE)
  # Crop irrigation percentages
  all_crop_irrigated_dt = 
    read.fst(
      here("data/crops/all-crop-irrigated-dt.fst"), 
      as.data.table = TRUE
    )[,.(
      GEOID, year, 
      pct_irrigated_corn, pct_irrigated_soy, 
      pct_irrigated_cotton, pct_irrigated_gm
    )]
  # Grabbing soil and rainfall data
  usle_dt =  read.fst(
    here('data-clean/watershed/usle-dt.fst'),
    as.data.table = TRUE
  )[,hybas_id := as.numeric(hybas_id)]
  # Upstream/downstream watersheds
  upstream_dt = read.fst(
    path = here("data-clean/watershed/upstream-dt-hydrobasin.fst"), 
    as.data.table = TRUE
  )
  # loading crop weights, forgot to add year to the raw data, oops 
  #crop_weight_dt = 
  # map_dfr(
  #  1999:2018,
  #  \(yr){
  #    read_fst(here(paste0("data-clean/weights/watershed-weights",yr,".fst"))) |> 
  #      mutate(year = yr)
  #  }
  #) |> data.table() |>
  #  setkey(pfaf_code,geoid,year)

  # Merging together ----------------------------------------------------------
    watershed_pesticide_dt_all = 
      merge(
        fs_dt,
        trt_dt[,.(GEOID, e100m)],
        by = 'GEOID'
      ) |>
      merge(
        all_crop_irrigated_dt,
        by = c('GEOID','year'),
        all.x = TRUE
      ) |>
      #merge(
      #  crop_dt, 
      #  by = 'GEOID',
      #  all.x = TRUE
      #) |>
      merge(
        area_weight_dt,
        by = "GEOID",
        allow.cartesian = TRUE
      ) |>
      # merge(
      #    crop_weight_dt,
      #    by = c("geoid","year")
      #  ) |>
      merge(
        hydrobasin_dt,
        by = "hybas_id"
      ) 

# Data prep work --------------------------------------------------------------
  # Columns with pesticide variables 
  pest_cols = colnames(fs_dt)[
    str_which(colnames(fs_dt), "alachlor$"):str_which(colnames(fs_dt), "other_pesticide$")
  ]
  # Area weighting of pesticide variables  
  watershed_pesticide_dt_all[,
    (paste0(pest_cols,"_awt")) := lapply(.SD, \(x){x*area_weight}),
    .SDcols = pest_cols
  ][,# Dividing by sub area to get intensity measures
    (paste0(pest_cols,"_km2_awt")) := lapply(.SD, \(x){x*area_weight/sub_area}),
    .SDcols = pest_cols
  ][,':='(# Slightly different for pred_glyph_km2
    pred_glyph_km2_awt = pred_glyph_km2*area_km2*area_weight/sub_area,
    pred_glyphosate_awt = pred_glyph_km2*area_km2*area_weight
  )]
  setnames(watershed_pesticide_dt_all, "glyphosate_km2_awt","glyph_km2_awt")
  # Area weighting the crop variables 
  crop_cols = str_subset(colnames(fs_dt), '^.{2,6}_acres$')
  watershed_pesticide_dt_all[,
    (paste0(crop_cols,"_awt")) := lapply(.SD, \(x){x*area_weight}),
    .SDcols = crop_cols
  ]
  # Summarizing by watershed 
  # (some watersheds show up twice if in multiple counties)
  watershed_pesticide_dt = 
    merge( 
      # First columns we want to take sum of 
      watershed_pesticide_dt_all[,
        lapply(.SD, sum, na.rm = TRUE),
        by = .(hybas_id, year),
        .SDcols = str_subset(colnames(watershed_pesticide_dt_all), "awt")
      ],
      # Next columns we want to take averages of 
      watershed_pesticide_dt_all[,
        lapply(.SD, mean, na.rm = TRUE),
        by = .(hybas_id, year),
        .SDcols = str_subset(
          colnames(watershed_pesticide_dt_all), 
          "yield_diff|pct_irrigated|hybas_area_km2|e100m|percentile"
        )
      ],
      by = c("hybas_id", "year")
    ) |>
    merge(
      usle_dt, 
      by = c("hybas_id", "year"),
      all.x = TRUE
    )
  # Creating indicators for high-erodibility,rainfall,irrigation
  kls_quant = 
    quantile(
      watershed_pesticide_dt[e100m >= 0.5]$kls, 
      probs = seq(0,1,0.05), 
      na.rm = TRUE
    )
  grow_quant = 
    quantile(
      watershed_pesticide_dt[e100m >= 0.5]$ppt_growing_season, 
      probs = seq(0,0.8,by = 0.2), 
      na.rm = TRUE
    )
  off_quant = 
    quantile(
      watershed_pesticide_dt[e100m >= 0.5]$ppt_off_season, 
      probs = seq(0,0.8,by = 0.2), 
      na.rm = TRUE
    )
  gm_quant = 
    quantile(
      watershed_pesticide_dt[e100m >= 0.5]$pct_irrigated_gm, 
      probs = seq(0,1,by = 0.05),
      na.rm = TRUE
    )
  soy_quant = 
    quantile(
      watershed_pesticide_dt[e100m >= 0.5]$pct_irrigated_soy, 
      probs = seq(0,1,by = 0.05),
      na.rm = TRUE
    )
  corn_quant = 
    quantile(
      watershed_pesticide_dt[e100m >= 0.5]$pct_irrigated_corn, 
      probs = seq(0,1,by = 0.05),
      na.rm = TRUE
    )
  cotton_quant = 
    quantile(
      watershed_pesticide_dt[e100m >= 0.5]$pct_irrigated_cotton, 
      probs = seq(0,1,by = 0.05),
      na.rm = TRUE
    )
  # Adding the indicators
  watershed_pesticide_dt[,':='(
    high_kls = kls > kls_quant['80%'],
    high_ppt_growing_season = ppt_growing_season > grow_quant['20%'],
    high_ppt_off_season = ppt_off_season > off_quant['20%'],
    high_pct_irrigated_gm = pct_irrigated_gm > gm_quant['50%'],
    high_pct_irrigated_soy = pct_irrigated_soy > soy_quant['50%'],
    high_pct_irrigated_corn = pct_irrigated_corn > corn_quant['50%'],
    high_pct_irrigated_cotton = pct_irrigated_cotton > cotton_quant['50%'],
    q_ppt_growing_season = findInterval(ppt_growing_season, grow_quant),
    q_ppt_off_season = findInterval(ppt_growing_season, off_quant)
  )]
  # Creating interactions between the variables 
  watershed_pesticide_dt[, ':='(
    # A column of ones to count total up/down stream watersheds 
    i = 1,
    # Interactions between soil variables and glyphosate 
    high_kls_ppt_growing_glyph_awt = high_kls*high_ppt_growing_season*glyphosate_awt,
    high_kls_ppt_off_glyph_awt = high_kls*high_ppt_off_season*glyphosate_awt,
    high_kls_irr_gm_glyph_awt = high_kls*high_pct_irrigated_gm*glyphosate_awt,
    high_kls_irr_soy_glyph_awt = high_kls*high_pct_irrigated_soy*glyphosate_awt,
    high_kls_irr_corn_glyph_awt = high_kls*high_pct_irrigated_corn*glyphosate_awt,
    high_kls_irr_cot_glyph_awt = high_kls*high_pct_irrigated_cotton*glyphosate_awt,
    # Indicator variables for being either high rain or high irrigation
    high_ppt_growing_irr_gm = high_ppt_growing_season==TRUE|high_pct_irrigated_gm >= 0.5,
    high_ppt_growing_irr_soy = high_ppt_growing_season==TRUE|high_pct_irrigated_soy>= 0.5,
    high_ppt_growing_irr_corn = high_ppt_growing_season==TRUE|high_pct_irrigated_corn>= 0.5,
    high_ppt_growing_irr_cotton = high_ppt_growing_season==TRUE|high_pct_irrigated_cotton >= 0.5,
    high_ppt_off_irr_gm = high_ppt_off_season==TRUE|high_pct_irrigated_gm >= 0.5,
    high_ppt_off_irr_soy = high_ppt_off_season==TRUE|high_pct_irrigated_soy>= 0.5,
    high_ppt_off_irr_corn = high_ppt_off_season==TRUE|high_pct_irrigated_corn>= 0.5,
    high_ppt_off_irr_cotton = high_ppt_off_season==TRUE|high_pct_irrigated_cotton >= 0.5
  )]
  # Interactions for treatment indicators 
  trt_cols = str_subset(colnames(watershed_pesticide_dt), "yield_diff")
  watershed_pesticide_dt[,
    (paste0('high_kls_ppt_growing_',trt_cols)) := 
      lapply(.SD, \(x){x*high_kls*high_ppt_growing_season}),
    .SDcols = trt_cols
  ]
  # Merging with the pesticide data to aggregate
  upstream_dt |> setkey(hybas_id2)
  watershed_pesticide_dt |> setkey(year, hybas_id)
  # Columns we want to aggregate  
  col_vec = 
    str_subset(
      colnames(watershed_pesticide_dt),
      "awt|yield_diff|high_(kls|ppt|pct_irrigated)"
    ) |>
    str_subset(
      'bck|cab|olv|srg|wpo',
      negate = TRUE
    ) |> 
    c('i','hybas_area_km2') 
  # Columns we want to normalize
  norm_vec = str_subset(
    col_vec,
    "(e100m|all)_yield_diff|high_(kls|ppt|pct_irrigated)"
  )
  # Aggregates 
  aggregate_up_down = function(year_in){
    # Aggregating by distance bins
    pesticide_upstream_dt_yr_wide =   
      merge(
        upstream_dt, 
        watershed_pesticide_dt[.(year_in)],
        by.x = "hybas_id2",
        by.y = "hybas_id",
        allow.cartesian = TRUE
      ) 
    # Aggregating 
    pesticide_upstream_dt_yr = 
      pesticide_upstream_dt_yr_wide[,
        lapply(.SD, sum, na.rm = T),
        keyby = .(hybas_id, local, year, dist_km_bin),
        .SDcols = col_vec
      ]
    # Normalizing treatmeant variables by number of watersheds
    pesticide_upstream_dt_yr_norm = 
      cbind(
        pesticide_upstream_dt_yr[,-..norm_vec],
        pesticide_upstream_dt_yr[,
            lapply(.SD,\(x){x/i}),
            .SDcols = norm_vec
          ]
      )
    print(paste(year_in, 'done'))
    return(pesticide_upstream_dt_yr_norm)
  }

# Running for all years -------------------------------------------------------
pesticide_upstream_dt = 
  map_dfr(
    #list(1992:1996, 1997:2001, 2002:2006, 2007:2011, 2012:2017),
    1992:2017,
    aggregate_up_down
  )

# Saving the results 
write.fst(
  watershed_pesticide_dt, 
  path = here("data-clean/watershed/watershed-pesticide-dt.fst")
)
# Saving the results 
write.fst(
  pesticide_upstream_dt, 
  path = here("data-clean/watershed/pesticide-upstream-dt.fst")
)

# Started 04:18:14
# Finished 04:18:57







