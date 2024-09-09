#-------------------------------------------------------
# This script puts together all of the non-health county
# level data
# Runtime: 1 min
#-------------------------------------------------------
# Loading required packages
library(pacman)
p_load(
  magrittr, data.table, janitor, tidyr, dplyr, 
  readr, here, stringr, glue, fst, purrr, readxl,
  tigris, sf, crosswalkr
)
options(tigris_use_cache = TRUE)

# Function to calculate percents
calc_percent = function(numerator, denominator, scale = 1000){
  # Setting to 0 if numerator is NA but denominator is not
  ifelse(
    is.na(numerator) & !is.na(denominator),
    0,
    numerator*scale/denominator
  )
}

# Function to check for GEOIDS that may have switched
check_missing_geoids = function(dt){
  tmp = merge(
    county_year_dt[,.(GEOID, year, in_panel = 1)], 
    dt[,.(GEOID, year, in_dt = 1)],
    by = c('GEOID','year'),
    all = TRUE
  )
  return(tmp[is.na(in_panel)])
}

create_comb_cnty_dt = function(yr_start = 1990, yr_end = 2017, water_exposure = FALSE){
  
# Creating a balanced panel of counties 
  # State fips codes 
  state_sf = states(year = 2010, cb = TRUE) |>
    filter( # Limiting to continental US
      !(STATE %in% c("02","15","60","66","69","72","78"))
    ) 
  county_sf =
    map_dfr(
      state_sf$STATE,
      counties,
      year = 2010,
      cb = TRUE
    ) |> 
    mutate(GEOID = paste0(STATEFP,COUNTYFP)) 
  county_year_dt = CJ(GEOID = county_sf$GEOID, year = yr_start:yr_end)
  # Size of county 
  cnty_area_dt = 
    read.fst(
      path = here("data/download-script/cnty-area-dt.fst"),
      as.data.table = TRUE
    )[census_year == '201', .(GEOID, area_km2)]
  # Treatment
  trt_dt = read.fst(
    path = here('data/clean/trt-dt.fst'),
    as.data.table = TRUE
  )
  # Pesticide data
  pest_dt = 
    read.fst(
      here("data/raw/est_pest_use.fst"),
      as.data.table = TRUE
    )[,census_year := str_sub(year, 1,3)] 
  # First stage 
  fs_dt = read.fst(
    path = here("data/clean/fs-dt.fst"),
    as.data.table = TRUE
  )[year %in% yr_start:yr_end, .(
    GEOID, year, 
    pred_glyph_km2, 
    pred_glyphosate
  )]


  # Now for crop data ---------------------------------------------------------------
  # ALL crop acreage
  all_crop_acre_dt = read.fst(
    here('data/raw/all-crop-acre-dt.fst'), 
    as.data.table = TRUE
  )[year %in% yr_start:yr_end]
  # Crop yields 
  all_crop_yield_dt = read.fst(
    here("data/raw/all-crop-yield-dt.fst"), 
    as.data.table = TRUE
  )[year %in% yr_start:yr_end & str_sub(GEOID,1,2) != '15']
  # Percent irrigated
  all_crop_irrigated_dt = read.fst(
    here("data/raw/all-crop-irrigated-dt.fst"), 
    as.data.table = TRUE
  )[year %in% yr_start:yr_end]
  setnames(
    all_crop_irrigated_dt,
    old = c('tot_acres_corn','tot_acres_cotton','tot_acres_soy'),
    new = c('corn_acres_irrigated','cotton_acres_irrigated','soy_acres_irrigated')
  )
  # Fertilizer data 
  annual_fert_dt = read.fst(
    here('data/raw/fertilizer-dt-interpolated.fst'), 
    as.data.table = TRUE
  )
  # Rest of data --------------------------------------------------------------------
  # County population
  cnty_pop_dt = read.fst(
    here("data/download-script/cnty-pop-dt.fst"),
    as.data.table = TRUE
  )[year %in% yr_start:yr_end] 
  # Fixing wrong codes 
  cnty_pop_dt = 
    cnty_pop_dt[,.(
      tot_pop = sum(tot_pop),
      tot_pop_hisp = sum(tot_pop_hisp)),
      keyby = .(
        GEOID = fcase(
          GEOID == '46102', '46113',
          GEOID == '51560', '51005',
          GEOID != '46102', GEOID
        ),
        year
      )
    ]
  
  # Now employment data
  farm_empl_dt = read.fst(
    here("data/download-script/farm-empl-dt.fst"),
    as.data.table = TRUE
  ) %>% 
    .[!(str_sub(GEOID, 1,2) %in% c("02","15","60","66","69","72","78"))] %>%
    .[year %in% yr_start:yr_end] %>% 
    .[,year := as.numeric(year)] %>% 
    .[,tot_pop := NULL]
  
  # Now education data
  edu_dt = read.fst(
    here("data/raw/edu-dt.fst"),
    as.data.table = TRUE
  ) %>% 
    .[census_year %in% str_sub(yr_start:yr_end,1,3)]
  
  # Labor force data
  labor_dt = read.fst(
    here("data/raw/labor-dt.fst"),
    as.data.table = TRUE
  ) %>% 
    .[year %in% yr_start:yr_end]
  labor_dt[,
    GEOID := fcase(
      GEOID == '46102', '46113',
      GEOID != '46102', GEOID
    )
  ]
  # Rural Urban Codes 
  rural_dt = 
    data.table(read_xls(here("data/download-manual/ruralurbancodes2003.xls")))[,.(
      GEOID = `FIPS Code`,
      ruc = `2003 Rural-urban Continuum Code`
    )] 
    #|>
    #clean_names() |>
    #select(
    #  GEOID = fips_code, 
    #  ruc_199 = x1993_rural_urban_continuum_code, 
    #  ruc_200 = x2003_rural_urban_continuum_code
    #) |>
    #setDT() |>
    #melt(id.vars = "GEOID", variable.name = "census_year", value = "ruc") %>% 
    #.[,census_year := str_remove(census_year, "ruc_")]
  # Fixing bad codes
  rural_dt[,
    GEOID := fcase(
      GEOID == '51560', '51005',
      GEOID != '51005', GEOID
    )
  ]
  # Merging everything --------------------------------------------------------------
  county_year_dt[,census_year := str_sub(year, 1,3)]
  comb_dt = 
    county_year_dt |>
    merge(
      pest_dt[,-c('census_year')], 
      by = c('GEOID','year'),
      all.x = TRUE
    ) |>
    merge( 
      fs_dt, 
      by = c('GEOID','year'),
      all.x = TRUE
    ) |>
    merge(
      trt_dt, 
      by = "GEOID",
      all.x = TRUE
    ) |>
    merge(
      all_crop_acre_dt, 
      by = c("GEOID","year"),
      all.x = T
    ) |>
    merge(
      all_crop_yield_dt, 
      by = c("GEOID","year"),
      all.x = T
    ) |>
    merge(
      cnty_pop_dt, 
      by = c("GEOID","year"),
      all.x = T
    )|>
    merge(
      farm_empl_dt, 
      by = c("GEOID","year"),
      all.x = T
    )|>
    merge(
      labor_dt, 
      by = c("GEOID","year"),
      all.x = T
    ) |>
    merge(
      edu_dt, 
      by = c("GEOID","census_year"),
      all.x = T
    )|>
    merge(
      rural_dt, 
      by = c("GEOID"),
      all.x = T
    ) |>
    merge(
      all_crop_irrigated_dt,
      by = c("GEOID","year"),
      all.x = T
    ) |>
    merge(
      annual_fert_dt,
      by = c("GEOID","year"),
      all.x = T
    ) |>
    merge(
      cnty_area_dt,
      by = 'GEOID'
    )  
  # Adding some variables 
  comb_dt[,':='(
    glyph_km2 = glyphosate/area_km2,
    alachlor_km2 = alachlor/area_km2, 
    atrazine_km2 =  atrazine/area_km2,
    cyanazine_km2 = cyanazine/area_km2,
    fluazifop_km2 = fluazifop/area_km2, 
    metolachlor_km2 = metolachlor/area_km2, 
    metribuzin_km2 =  metribuzin/area_km2,
    nicosulfuron_km2 = nicosulfuron/area_km2
  )]
  # Fertilizer colnames
  fert_cols = colnames(annual_fert_dt)[-(1:2)]
  # For data in these tables, missing values are considered zero's
  zero_vars = c(
    colnames(fs_dt)[-(1:2)],
    colnames(pest_dt)[-(1:2)],
    colnames(all_crop_acre_dt)[-(1:4)],
    colnames(all_crop_yield_dt)[-(1:4)],
    fert_cols
  )
  # Filling in NA's with zeros
  for (j in zero_vars) {
    set(
      x = comb_dt,
      i = which(is.na(comb_dt[[j]])),
      j,
      value = 0
    )
  }
  # Adding fertilizer km2 normalizations 
  comb_dt[,
    (paste0(fert_cols,'_km2')) := lapply(.SD, \(x) x/area_km2), 
    .SDcols = fert_cols
  ]
  # Creating pct variables 
  comb_dt[,':='(
    pct_soy_tot = calc_percent(soy_acres, soy_acres + corn_acres + other_acres, scale = 100),
    pct_soy_rc = calc_percent(soy_acres, soy_acres + corn_acres, scale = 100),
    pct_corn_tot = calc_percent(corn_acres, soy_acres + corn_acres + other_acres, scale = 100),
    pct_corn_rc = calc_percent(corn_acres, soy_acres + corn_acres, scale = 100),
    tot_acres = soy_acres + corn_acres + other_acres,
    tot_rc_acres = soy_acres + corn_acres,
    state_fips = str_sub(GEOID,1,2)
  )]
  
  # States with any soy production reported
  soy_states = comb_dt[soy_acres > 0, state_fips] |> unique()
  soy_counties = comb_dt[soy_acres > 0, GEOID] |> unique()
  
  # Some other loose ends
  comb_dt[,':='(
    pct_farm_empl = farm_empl/tot_empl,
    pct_hisp = tot_pop_hisp/tot_pop,
    pop_density = tot_pop/area_km2,
    rural = ruc > 3,
    sample = fcase(
      str_sub(GEOID,1,2) %in% c(
        "38","46","31","20","40","27","19","29","05",
        "22","55","17","26","39","18","21","47","28",
        "01","36","42","24","10","51","37","45","34"
      ),"Primary Soy",
      str_sub(GEOID,1,2) %in% c(
        "02","04","06","08","15","16",
        "30","32","35","41","49","53",
        "56","60","66","69","72","78"
      ),"West of 100m",
      default = "East of 100m"),
    soy_state = state_fips %in% soy_states, 
    soy_county = GEOID %in% soy_counties
  )]

  # Census region and divisions 
  comb_dt = 
    merge(
      comb_dt[, -c('census_region', 'census_division')],
      data.table(stcrosswalk)[,.(
        state_fips = str_pad(stfips, 2, 'left','0'),
        census_region = cenregnm, 
        census_division = cendivnm
      )],
      by = 'state_fips',
      all.x = TRUE
    )

  if(water_exposure == TRUE){
    # Upstream/downstream Exposure data
    county_exposure_dt = 
      read.fst(
        path = here("data/watershed/county-exposure-dt.fst"), 
        as.data.table = TRUE
      ) 
    # Merging 
    comb_dt = 
      merge(
        comb_dt,
        county_exposure_dt, 
        by = c("GEOID","year"),
        all = TRUE
      ) 
    # Setting missing to zero 
    water_vars = colnames(county_exposure_dt)[-(1:2)]
    # Filling in NA's with zeros
    for (j in water_vars) {
      set(
        x = comb_dt,
        i = which(is.na(comb_dt[[j]])),
        j,
        value = 0
      )
    }
  }
  
  # Returning the results 
  return(comb_dt[year %in% yr_start:yr_end])
}

# Running it!
comb_cnty_dt = 
  create_comb_cnty_dt(
    yr_start = 1990, 
    yr_end = 2017,
    water_exposure = FALSE
  )
write.fst(
  comb_cnty_dt,
  here("data/clean/comb-cnty-dt.fst")
)

