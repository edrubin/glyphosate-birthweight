# Some appendix figures 
library(pacman)
p_load(
  here, data.table, fst, ggplot2, readr, 
  stringr, collapse, purrr
)

# Function to aggregate to ag district level
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
        c('^year$', '^asd_code$', '^state_fips$', '^area_km2$', 
        '_yield_diff_', '^yield_'),
        regex = TRUE
      ) |>
      gby(year, state_fips, asd_code) |>
      fmean(w = area_km2),
      # variables to sum over
      get_vars(
        comb_cnty_health_dt,
        c('^year$', '^asd_code$', '^state_fips$', 
          '_acres$', '_tot_yield$','^tot_inf_births$', 'tot_pop$'),
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
    GEOID_asd = paste0(state_fips, asd_code)
  )]
  return(comb_asd_dt)
}

# First correlation between GAEZ and acreage ---------------------------------
  # Loading raw GAEZ data
  gaez_dt = 
    read.fst(
      here("data/raw/y_diff_dt.fst"),
      as.data.table = TRUE
    )[crop %in% c('mze','cot','soy')] |>
    dcast(
      formula = GEOID ~ crop,
      value.var = c('yield_low', 'yield_high') 
    )
  # Adding to rest of data 
  comb_cnty_dt = 
    read_fst(
      path = here('data/clean/comb-cnty-dt.fst'),
      as.data.table = TRUE
    ) |>
    merge(
      gaez_dt, 
      by = 'GEOID',
      all.x = TRUE
    )
  # Aggregating to district
  comb_asd_dt = aggregate_cnty_to_asd(comb_cnty_dt)
  # Calculating pre-period mean
  pre_acre_pctl_dt = 
    comb_asd_dt[year < 1996] |>
    gby(GEOID_asd) |>
    fselect(corn_acres, cotton_acres, soy_acres, csc_acres) |>
    fmean() |>
    merge(
      comb_asd_dt[year == 1995, .(
        GEOID_asd, 
        yield_low_cot,
        yield_low_mze, 
        yield_low_soy,
        yield_high_cot,
        yield_high_mze, 
        yield_high_soy
      )],
      by = 'GEOID_asd'
    )
  # Adding csc average 
  pre_acre_pctl_dt[,':='(
    yield_low_csc = (yield_low_cot + yield_low_mze + yield_low_soy)/3,
    yield_high_csc = (yield_high_cot + yield_high_mze + yield_high_soy)/3
  )]
  # Now calculating percentiles 
  pre_acre_pctl_dt[,':='(
    corn_acres_pctl = frank(corn_acres)/.N,
    cotton_acres_pctl = frank(cotton_acres)/.N,
    soy_acres_pctl = frank(soy_acres)/.N,
    csc_acres_pctl = frank(csc_acres)/.N,
    corn_yield_low_pctl = frank(yield_low_mze)/.N,
    corn_yield_high_pctl = frank(yield_high_mze)/.N,
    cotton_yield_low_pctl = frank(yield_low_cot)/.N,
    cotton_yield_high_pctl = frank(yield_high_cot)/.N,
    soy_yield_low_pctl = frank(yield_low_soy)/.N,
    soy_yield_high_pctl = frank(yield_high_soy)/.N,
    csc_yield_low_pctl = frank(yield_low_csc)/.N,
    csc_yield_high_pctl = frank(yield_high_csc)/.N
  )]
  
# Making the plots 
gaez_crop_acreage_correlation = function(crop_in, pre_acre_pctl_dt){
  low_p =   
    ggplot(
      data = pre_acre_pctl_dt, 
      aes(
        y = .data[[paste0(crop_in, '_acres_pctl')]], 
        x = .data[[paste0(crop_in, '_yield_low_pctl')]]
      )
    ) + 
    geom_point() + 
    geom_smooth() + 
    theme_minimal(base_size = 16) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
    scale_y_continuous(
      name = 'Pre-period Acreage Percentile', 
      label = scales::label_percent()  
    )+ 
    scale_x_continuous(
      name = 'GAEZ Yield Percentile (Low)', 
      label = scales::label_percent()  
    )
  ggsave(
    low_p, 
    filename = here(
      'figures/descriptive/gaez-acreage',
      paste0('gaez-acreage-low-',crop_in,'.jpeg')
    ),
    width = 6, height = 4
  )
  high_p =   
    ggplot(
      data = pre_acre_pctl_dt, 
      aes(
        y = .data[[paste0(crop_in, '_acres_pctl')]], 
        x = .data[[paste0(crop_in, '_yield_high_pctl')]]
      )
    ) + 
    geom_point() + 
    geom_smooth() + 
    theme_minimal(base_size = 16) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
    scale_y_continuous(
      name = 'Pre-period Acreage Percentile', 
      label = scales::label_percent()  
    )+ 
    scale_x_continuous(
      name = 'GAEZ Yield Percentile (High)', 
      label = scales::label_percent()  
    )
  ggsave(
    high_p, 
    filename = here(
      'figures/descriptive/gaez-acreage',
      paste0('gaez-acreage-high-',crop_in,'.jpeg')
    ),
    width = 6, height = 4
  )
  return(high_p)

}

# Running it
map(
  c('cotton','corn','soy','csc'),
  gaez_crop_acreage_correlation,
  pre_acre_pctl_dt = pre_acre_pctl_dt
)

# Regressions of GAEZ on acreage 
p_load(fixest)
feols(
  data = pre_acre_pctl_dt, 
  fml = csc_acres_pctl ~ csc_yield_high_pctl
)
feols(
  data = pre_acre_pctl_dt, 
  fml = corn_acres_pctl ~ corn_yield_high_pctl
)
feols(
  data = pre_acre_pctl_dt, 
  fml = soy_acres_pctl ~ soy_yield_high_pctl
)
feols(
  data = pre_acre_pctl_dt, 
  fml = cotton_acres_pctl ~ cotton_yield_high_pctl
)
feols(
  data = pre_acre_pctl_dt, 
  fml = log(corn_acres) ~ log(yield_high_mze)
)
feols(
  data = pre_acre_pctl_dt, 
  fml = log(soy_acres) ~ log(yield_high_soy)
)
feols(
  data = pre_acre_pctl_dt, 
  fml = log(cotton_acres) ~ log(yield_high_cot)
)

# Now the balance table 
library(pacman)
p_load(
  data.table, fixest, fst, here, ggplot2, modelsummary,
  stringr,dplyr, janitor, purrr, magrittr,
  vtable
)

# Reading the data ---------------------------------------------------
comb_dt = 
  read.fst(
    here("data/clean/comb-cnty-health-dt.fst"),
    as.data.table = TRUE
  )[,
    trt := fcase(
      rural == TRUE & all_yield_diff_gmo_50_0 == TRUE, "High GM Yield",
      rural == TRUE & all_yield_diff_gmo_50_0 == FALSE, "Low GM Yield",
      rural == FALSE, "Urban"
    )
  ][,
    num_counties := uniqueN(GEOID),by = trt
  ][,
    pct_male := tot_male_births/tot_inf_births
  ]

vars = c(
  "num_cnty",
  "median_birth_wt", 
  "pct_low_bw", 
  "pct_male", 
  "inf_mort", 
  "tot_inf_births",
  "glyph_km2",
  "tot_km2",
  "tot_pop",
  "pct_hisp",
  "unemployment_rate",
  "hs_some_pct",
  "hs_deg_pct",
  "college_some_pct",
  "college_deg_pct", 
  "inc_per_cap")

sum_dt = comb_dt[
  year %in% 1992:1995 & !is.na(trt), .(
    median_birth_wt = mean(median_birth_wt, na.rm = TRUE),
    pct_low_bw = mean(pct_low_bw, na.rm = TRUE),
    pct_male = 100*mean(pct_male, na.rm = TRUE),
    inf_mort = mean(inf_mort, na.rm = TRUE),
    tot_inf_births = mean(tot_inf_births, na.rm = TRUE),
    glyph_km2  = mean(glyph_km2, na.rm = TRUE),
    tot_km2 = 0.00404686*mean(tot_acres, na.rm = TRUE),
    tot_pop = mean(tot_pop, na.rm = TRUE)/1000,
    pct_hisp = 100*mean(pct_hisp,na.rm = TRUE), 
    unemployment_rate = 100*mean(unemployment_rate,na.rm = TRUE),
    hs_some_pct = mean(hs_some_pct, na.rm = TRUE),
    hs_deg_pct = mean(hs_deg_pct, na.rm = TRUE),
    college_some_pct = mean(college_some_pct, na.rm = TRUE),
    college_deg_pct = mean(college_deg_pct, na.rm = TRUE),
    inc_per_cap = mean(inc_per_cap_farm + inc_per_cap_nonfarm, na.rm = TRUE) 
  ),
  by = .(GEOID,trt)
][,num_cnty := uniqueN(GEOID), by = trt]


sumtable(
  data = sum_dt,
  group = 'trt',
  vars = vars,
  summ = c('mean(x)','sd(x)'),
  labels = c(
    "Number of Counties",
    "Birth Weight ($g$)", 
    "Pct Low Birth Weight", 
    "Percent Male", 
    "Infant Mortality", 
    "Total Births",
    "Glyphosate ($kg/km^2$)",
    "Total Crop Area ($km^2$)",
    "Total Pop (1000's)",
    "Percent Hispanic",
    "Unemployment Rate",
    "Pct Some HS Degree",
    "Pct HS Degree",
    "Pct Some College",
    "Pct College Degree", 
    "Income per Capita"),
  title = "Summary Statistics for high- and low-attainable yield counties between 1992 and 1995",
  digits = 3,
  out = "latex",
  note = "Means and standard deviations are calculated on county level averages between 1992 and 1995, which is the period prior to the release of GM crops."
)

# What percentage of glyphosate is used east of 100th meridian
comb_dt[year %in% 1992:2004,
  sum(ifelse(e100m == TRUE,glyphosate,0),na.rm = TRUE)
  /sum(glyphosate,na.rm = TRUE)
]

