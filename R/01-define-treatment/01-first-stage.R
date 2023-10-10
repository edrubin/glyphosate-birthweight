# -----------------------------------------------------------------------
# First stage predictions of glyphosate
# -----------------------------------------------------------------------
# Loading required packages
library(pacman)
p_load(
  magrittr, data.table, here, fst, dplyr, fixest, purrr, stringr, qs
)

# Loading the data ------------------------------------------------------------
  # glyphosate 
  pest_dt = 
    read.fst(
      here("data/raw/est_pest_use.fst"),
      as.data.table = TRUE
    )[,census_year := str_sub(year, 1,3)]
  # attainable yield
  trt_dt = read.fst( 
      path = here("data/clean/trt-dt.fst"),
      as.data.table = TRUE
  )[,.(
    GEOID,
    all_yield_diff_percentile_cot,
    all_yield_diff_percentile_gmo,      
    all_yield_diff_percentile_gmo_max,
    all_yield_diff_percentile_mze,      
    all_yield_diff_percentile_soy,
    e100m_yield_diff_percentile_cot,     
    e100m_yield_diff_percentile_gmo,    
    e100m_yield_diff_percentile_gmo_max, 
    e100m_yield_diff_percentile_mze,    
    e100m_yield_diff_percentile_soy
   )]
  # Crop data 
  crop_dt = 
    read.fst(
      path = here('data/clean/crop-acre-percentile-90-95.fst'), 
      as.data.table = TRUE
    )
  # National glyphosate trends 
  glyph_nat_dt =  
    read.fst(
      here('data/clean/glyph-nat-dt.fst'),
      as.data.table = TRUE
    )
  # Size of county 
  cnty_area_dt = 
    read.fst(
      path = here("data/download-script/cnty-area-dt.fst"),
      as.data.table = TRUE
    )[census_year == '201', .(GEOID, area_km2)]


# Prepping the data -----------------------------------------------------
  # Merging together
  fs_dt = 
    merge(
      pest_dt, 
      trt_dt, 
      by = c('GEOID')
    )|>
    merge(
      cnty_area_dt,
      by = c("GEOID")
    ) |>
    merge(
      crop_dt, 
      by = 'GEOID'
    ) |>
    merge(
      glyph_nat_dt, 
      by  = c('GEOID','year')
    )

  # Adding some variables 
  fs_dt[,':='(
    glyph_km2 = glyphosate/area_km2,
    alachlor_km2 = alachlor/area_km2, 
    atrazine_km2 =  atrazine/area_km2,
    cyanazine_km2 = cyanazine/area_km2,
    fluazifop_km2 = fluazifop/area_km2, 
    metolachlor_km2 = metolachlor/area_km2, 
    metribuzin_km2 =  metribuzin/area_km2,
    nicosulfuron_km2 = nicosulfuron/area_km2,
    yrs_since_1995 = ifelse(year > 1995, year - 1995, 0),
    post_1995 = as.integer(year > 1995)
  )]

# Fitting the model -----------------------------------------------------
mod_fs_test = feols(
  data = fs_dt,
  fml = glyph_km2 ~ sw(
    # YEAR INTERACTIONS ---------------------------------------------
      # First just the suitability
        all_yield_diff_percentile_soy:i(year, ref = 1995)
      + all_yield_diff_percentile_mze:i(year, ref = 1995)
      + all_yield_diff_percentile_cot:i(year, ref = 1995),
      # Then just the pre-period acreage
       percentile_soy_acres:i(year, ref = 1995)
      + percentile_corn_acres:i(year, ref = 1995)
      + percentile_cotton_acres:i(year, ref = 1995),
      # Then both 
        all_yield_diff_percentile_soy:i(year, ref = 1995)
      + all_yield_diff_percentile_mze:i(year, ref = 1995)
      + all_yield_diff_percentile_cot:i(year, ref = 1995)
      + percentile_soy_acres:all_yield_diff_percentile_soy:i(year, ref = 1995)
      + percentile_corn_acres:all_yield_diff_percentile_mze:i(year, ref = 1995)
      + percentile_cotton_acres:all_yield_diff_percentile_cot:i(year, ref = 1995),
      # What about individual crop acreage
      percentile_soy_acres:i(year, ref = 1995),
      percentile_corn_acres:i(year, ref = 1995),
      percentile_cotton_acres:i(year, ref = 1995),
    # SHIFT SHARE IV -------------------------------------------------
      # First just the suitability
      glyphosate_nat_100km
      + all_yield_diff_percentile_soy:glyphosate_nat_100km
      + all_yield_diff_percentile_mze:glyphosate_nat_100km
      + all_yield_diff_percentile_cot:glyphosate_nat_100km,
      # Then just the pre-period acreage
      glyphosate_nat_100km
      + percentile_soy_acres:glyphosate_nat_100km
      + percentile_corn_acres:glyphosate_nat_100km
      + percentile_cotton_acres:glyphosate_nat_100km,
      # Then both 
      glyphosate_nat_100km
      + all_yield_diff_percentile_soy:glyphosate_nat_100km
      + all_yield_diff_percentile_mze:glyphosate_nat_100km
      + all_yield_diff_percentile_cot:glyphosate_nat_100km
      + percentile_soy_acres:all_yield_diff_percentile_soy:glyphosate_nat_100km
      + percentile_corn_acres:all_yield_diff_percentile_mze:glyphosate_nat_100km
      + percentile_cotton_acres:all_yield_diff_percentile_cot:glyphosate_nat_100km,
      # What about individual crop acreage
      glyphosate_nat_100km + percentile_soy_acres:glyphosate_nat_100km,
      glyphosate_nat_100km + percentile_corn_acres:glyphosate_nat_100km,
      glyphosate_nat_100km + percentile_cotton_acres:glyphosate_nat_100km,
      # What about national glyphosate by itself
      glyphosate_nat_100km
    ) | year + GEOID,
  se = "het"
)

# Checking R2 for different models 
purrr::map(
  1:length(mod_fs_test),
  \(i){r2(mod_fs_test[[i]], type  = c('r2','wr2'))}
)

# Picking one to use
mod_fs = feols(
  data = fs_dt,
  fml = glyph_km2 ~ 
    glyphosate_nat_100km
      + all_yield_diff_percentile_soy:glyphosate_nat_100km
      + all_yield_diff_percentile_mze:glyphosate_nat_100km
      + all_yield_diff_percentile_cot:glyphosate_nat_100km
      + percentile_soy_acres:all_yield_diff_percentile_soy:glyphosate_nat_100km
      + percentile_corn_acres:all_yield_diff_percentile_mze:glyphosate_nat_100km
      + percentile_cotton_acres:all_yield_diff_percentile_cot:glyphosate_nat_100km
    | year + GEOID,
  se = "het"
)

# Saving the results
qsave(
  x = mod_fs,
  file = here("data/results/20230823/mod-fs.qs")
)

# Grabbing predictions --------------------------------------------------
fs_dt[,
  pred_glyph_km2 := fitted.values(mod_fs)
][,
  pred_glyphosate := pred_glyph_km2*area_km2
]

# Saving the results  ---------------------------------------------------
write.fst(
  fs_dt, 
  path = here("data/clean/fs-dt.fst")
)

