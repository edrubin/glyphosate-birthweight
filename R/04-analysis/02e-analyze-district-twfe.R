# This script runs the aggregated 
library(pacman)
p_load(
  here, data.table, fst, fixest, readr, stringr, collapse, qs
)

# Function to aggregate to the ag district level 
aggregate_cnty_to_asd = function(comb_cnty_health_dt){
  # Loading the crosswalk
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
  # Calculating total yield 
  comb_cnty_health_dt[,':='(
    corn_tot_yield = corn_yield*corn_acres,
    soy_tot_yield = soy_yield*soy_acres,
    cotton_tot_yield = cotton_yield*cotton_acres
  )]
  # Merging with comb county table 
  comb_cnty_health_dt = 
    merge(
      comb_cnty_health_dt, 
      asd_county_xwalk[historical == 1 & !(county_fips %in% c(888,999))],
      by = c('GEOID', 'state_fips')
    )
  comb_asd_dt = 
    merge(
      # variables to take mean over
      get_vars(
        comb_cnty_health_dt,
        c('^year$', '^asd_code$', '^state_fips$', '^area_km2$', '_yield_diff_'),
        regex = TRUE
      ) |>
      gby(year, state_fips, asd_code) |>
      fmean(w = area_km2),
      # variables to sum over
      get_vars(
        comb_cnty_health_dt,
        c('^year$', '^asd_code$', '^state_fips$', 
          '_acres$', '_tot_yield$','^tot_inf_births$', 'tot_pop$',
          'alachlor$', 'atrazine$', 'cyanazine$', 'fluazifop$',
          'metolachlor$', 'metribuzin$', 'nicosulfuron$', 
          'area_km2'
          ),
        regex = TRUE
      ) |>
      gby(year, state_fips, asd_code) |>
      fsum(),
      by = c('year','state_fips','asd_code')
    )
  # Calculating yield 
  comb_asd_dt[,':='(
    corn_yield = corn_tot_yield/corn_acres,
    soy_yield = soy_tot_yield/soy_acres,
    cotton_yield = cotton_tot_yield/cotton_acres,
    csc_acres = corn_acres + soy_acres + cotton_acres,
    alachlor_km2 = alachlor/area_km2,
    atrazine_km2 = atrazine/area_km2,
    cyanazine_km2 = cyanazine/area_km2,
    fluazifop_km2 = fluazifop/area_km2,
    metolachlor_km2 = metolachlor/area_km2,
    metribuzin_km2 = metribuzin/area_km2,
    nicosulfuron_km2 = nicosulfuron/area_km2,
    GEOID_asd = paste0(state_fips, asd_code)
  )]
  return(comb_asd_dt)
}


# Now function to estimate the regression 
est_twfe_asd = function(outcomes_in, trt_in, est_dt, name){
  path = paste0('data/results/county-level/ag-district/',name)
  # Creating directory to put results in
  if(!dir.exists(here(path))) dir.create(here(path), recursive = TRUE)
  # Estimating event study for all outcomes (including pesticides)
  event_mods = feols(
    data = est_dt,
    weight = ~tot_inf_births,
    fml = paste0(
      "c(", paste(outcomes_in, collapse = ","), ") ~ sw(",
        paste(paste0("i(year,",trt_in,", ref = 1995)"), collapse = ","),
      ") | year + GEOID_asd"
    ) |> as.formula()
  )
  qsave(event_mods, here(path,'event-mods.qs'))
  rm(event_mods)
  gc()
  # Estimating DiD for all outcomes (including pesticides) 
  did_mods = feols(
    data = est_dt,
    weight = ~tot_inf_births,
    fml = paste0(
      "c(", paste(outcomes_in, collapse = ","),") ~ sw(",
      paste(paste0("i(year>1995,",trt_in,", ref = FALSE)"), collapse = ","),
      ") | year + GEOID_asd"
    ) |> as.formula()
  )
  qsave(did_mods, here(path,'did-mods.qs'))
  rm(did_mods)
  gc()
  return('yay')
}

alt_outcomes = c(  
  # Farm varibles
  'soy_acres', 
  'corn_acres', 
  'cotton_acres', 
  'tot_acres', 
  'other_acres',
  'csc_acres',
  'soy_yield', 
  'corn_yield', 
  'cotton_yield'
)
trt = c(
  #"e100m_yield_diff_percentile_bck",
  #"e100m_yield_diff_percentile_cab",
  #"e100m_yield_diff_percentile_olv",
  #"e100m_yield_diff_percentile_srg",
  #"e100m_yield_diff_percentile_wpo" 
  #'e100m_yield_diff_soy_50_10',
  #'e100m_yield_diff_soy_60_0',
  #'e100m_yield_diff_soy_40_0',
  #'e100m_yield_diff_gmo_50_10',
  #'e100m_yield_diff_gmo_40_0',
  #'e100m_yield_diff_gmo_60_0',
  #'all_yield_diff_gmo_50_0',
  "all_yield_diff_percentile_gmo",
  "all_yield_diff_percentile_gmo_max",
  "all_yield_diff_percentile_soy",
  "all_yield_diff_percentile_mze",
  "all_yield_diff_percentile_cot",
  "e100m_yield_diff_percentile_gmo",
  "e100m_yield_diff_percentile_gmo_max",
  "e100m_yield_diff_percentile_soy",
  "e100m_yield_diff_percentile_mze",
  "e100m_yield_diff_percentile_cot",
  'e100m_yield_diff_gmo_50_0',
  'e100m_yield_diff_soy_50_0',
  'all_yield_diff_gmo_50_0',
  'all_yield_diff_soy_50_0',
  "all_yield_diff_percentile_bck",
  "all_yield_diff_percentile_cab",
  "all_yield_diff_percentile_olv",
  "all_yield_diff_percentile_srg",
  "all_yield_diff_percentile_wpo"
)


# Loading data for estimation
comb_cnty_health_dt = read_fst(
  path = here('data/clean/comb-cnty-health-dt.fst'),
  as.data.table = TRUE
)
comb_asd_dt = aggregate_cnty_to_asd(comb_cnty_health_dt)

# Running the estimation 
est_twfe_asd(
  alt_outcomes, 
  trt, 
  comb_asd_dt, 
  name = 'asd-main'
)

