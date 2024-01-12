# Takes raw crop acreage/yield/irrigation data and turns it into county-year panel
# Distributes ag-district aggregations to rest of counties 
library(pacman)
p_load(
  here, fst, data.table, magrittr, readr, janitor, 
  purrr, stringr
)

# Loading the ag-district crosswalk 
asd_county_xwalk = 
  data.table(read_fwf(
    here('data/download-manual/county_list.txt'),
    col_positions = fwf_positions(
      start = c(1,6,11,17),
      end = c(2,7,13,100)
    ),
    skip = 12
  ))[!is.na(X1),.(
    state_fips = X1, 
    asd_code = X2, 
    county_fips = X3, 
    GEOID = paste0(X1,X3),
    name = str_split_i(X4, '\\\t{3,6}', i = 1), 
    historical = str_split_i(X4, '\\\t{3,6}', i = 2)
  )]
asd_county_xwalk[,
  GEOID := fcase(
    GEOID == '46102', '46113',
    GEOID != '46102', GEOID
  )
]
# Getting table of counties that have switched asd codes 
asd_county_xwalk[,n := .N,by = .(GEOID)]
asd_switch = 
  dcast(
    asd_county_xwalk[n > 1 & !(county_fips %in% c(888,999))],
    formula = state_fips + county_fips ~ historical, 
    value.var = 'asd_code'
  ) |>
  setnames(
    old = c('1','2'), 
    new = c('asd_code_new','asd_code_old')
  )
# Loading county sizes 
cnty_area_dt = 
  read.fst(
    path = here("data/download-script/cnty-area-dt.fst"),
    as.data.table = TRUE
  )[census_year == '201', .(GEOID, area_km2)] 

# Function to clean one of acres/yield/irrigation -----------------------------
clean_nass_data = function(type, asd_county_xwalk, asd_switch, cnty_area_dt){
  # Path where the raw data lives
  path_survey = fcase(
    type == 'acres', "data/download-script/nass-survey/acres-planted",
    type == 'yield', "data/download-script/nass-survey/yield",
    type == 'irrigated', "data/download-script/nass-survey/irrigated"
  )
  # Reading the raw data
  raw_all_crop = 
    map(
      here(paste0(path_survey,"/",list.files(path = here(path_survey)))),
      read.fst,
      as.data.table = TRUE
    ) |>
    rbindlist(use.names = TRUE) |> 
    clean_names()
  # Fixing asd codes for places that have changed
  raw_all_crop =
    merge(
      raw_all_crop,
      asd_switch,
      by.x = c('state_fips_code', 'county_ansi','asd_code'),
      by.y = c('state_fips','county_fips','asd_code_old'),
      all.x = TRUE
    )[,':='(
      asd_code = fifelse(!is.na(asd_code_new), asd_code_new, asd_code),
      asd_code_new = NULL
    )]  
  # Aggregating crops
  raw_all_crop_dt = 
    raw_all_crop[
      # Excluding silage and pima cotton (no GM for pima)
      str_detect(short_desc, "SILAGE", negate = TRUE) &
      str_detect(short_desc, 'PIMA', negate = TRUE) & 
      # Excluding rye and sunflower because they stop reporting in 2008
      str_detect(commodity_desc, 'SUNFLOWER', negate = TRUE)& 
      str_detect(commodity_desc, 'RYE', negate = TRUE)
      , .(
      year = as.numeric(year), 
      state_fips = state_fips_code, 
      county_fips = county_ansi,
      GEOID = paste0(state_fips_code,county_ansi),
      asd_code,
      crop = fcase( 
        commodity_desc == "CORN", paste0("corn_",type),
        commodity_desc == "SOYBEANS", paste0("soy_",type),
        commodity_desc == "COTTON", paste0("cotton_",type),
        default =  paste0("other_",type)
      ),
      # Will always be FALSE for type != 'irrigated'
      irrigated = prodn_practice_desc == 'IRRIGATED', 
      value_int = parse_number(as.character(value))
    )][,.(
      total = sum(value_int, na.rm = TRUE)),
      by = .(year, state_fips, county_fips, GEOID, asd_code, crop, irrigated)
    ]
  # Fixing wrong codes 
  raw_all_crop_dt[,
    GEOID := fcase(
      GEOID == '46102', '46113',
      GEOID != '46102', GEOID
    )
  ]
  # Allocate district level:
  # Merging county-level to full list 
  county_crop_dt = 
    CJ(
      GEOID = cnty_area_dt$GEOID, 
      year = unique(raw_all_crop_dt$year),
      crop = unique(raw_all_crop_dt$crop),
      irrigated = unique(raw_all_crop_dt$irrigated)
    ) |>
    merge(
      raw_all_crop_dt[county_fips != ''], 
      by = c('GEOID','year','crop','irrigated'),
      all.x = TRUE
    ) |>
    merge(
      cnty_area_dt, 
      by = 'GEOID'
    )
  county_crop_dt[, district_flag := FALSE]
  # First getting county-year-crops with missing data
  district_crop_dt = 
    merge(
      asd_county_xwalk[
        historical == 1 &
        !(county_fips %in% c(888,999))
      ], 
      county_crop_dt[is.na(total),.(GEOID, year, crop, irrigated, area_km2)],
      by = 'GEOID',
      all.y = TRUE
    ) |>
    # Now merging those with district values
    merge(
      raw_all_crop_dt[
        county_fips == '',-c('county_fips','GEOID')
      ],
      by = c('year','state_fips','asd_code','crop','irrigated'),
      all.x = TRUE
    )
  # Distributing district values according to county size 
  district_crop_dt[,
    area_km2_district := sum(area_km2),
    by = .(year, state_fips, asd_code, crop, irrigated)
  ][,':='(
    total_new = total*area_km2/area_km2_district,
    district_flag = !is.na(total)
  )][,':='(
    total_new = fifelse(is.na(total_new), 0, total_new)
  )]
  # Now adding back to original table 
  all_crop_dt = 
    rbind(
      county_crop_dt[
        !is.na(total),
        .(GEOID, year, crop, irrigated, total, district_flag)
      ],
      district_crop_dt[,
        .(GEOID, year, crop, irrigated, total = total_new, district_flag)
      ]
    ) 
  # Calculating percent for irrigation 
  if(type == 'irrigated'){
    # this just fixes the column names so they match legacy code
    all_crop_dt[,
      crop := paste0('irrigated_',str_remove(crop,'_irrigated'))
    ]
    # Calculating percent and totals, casting to wide
    all_crop_dt = 
      dcast(
        all_crop_dt,
        formula = year + GEOID + crop ~ irrigated, 
        value.var = "total",
        fill = 0
      )[,.(
        year, GEOID, crop, 
        pct = `TRUE`/(`TRUE`+`FALSE`),
        tot_acres = `TRUE`+`FALSE`
      )] |> 
      dcast(
        formula = year + GEOID ~ crop, 
        value.var = c('pct',"tot_acres"),
        fill = 0
      )
    # More column name fixing...
    setnames(
      all_crop_dt, 
      old = paste0('tot_acres_irrigated_',c('corn','soy','cotton')), 
      new = paste0('tot_acres_',c('corn','soy','cotton'))
    )
    # Adding GM pct irrigated 
    all_crop_dt[,':='(
      pct_irrigated_gm = 
        (pct_irrigated_corn*tot_acres_corn + 
          pct_irrigated_soy*tot_acres_soy + 
          pct_irrigated_cotton*tot_acres_cotton
        )/(tot_acres_corn + tot_acres_soy + tot_acres_cotton)
    )]
  }else{
    # Casting to wide format for acres/yield
    all_crop_dt = 
      dcast(
        all_crop_dt,
        formula = year + GEOID ~ crop, 
        value.var = "total",
        fill = 0
      ) |>
      setkey(GEOID, year)
  }
  # Saving the results: have to change "acres" into "acre" 
  # to keep downstream things from breaking
  write.fst(
    all_crop_dt,
    here(paste0(
      'data/raw/all-crop-',
      str_remove(type, '(?<=acre)s'),
      '-dt.fst'
    ))
  )
  # Now aggregating to 
}

# Running it! 
map(
  c('acres','yield','irrigated'),
  clean_nass_data,
  asd_county_xwalk, 
  asd_switch, 
  cnty_area_dt
)


