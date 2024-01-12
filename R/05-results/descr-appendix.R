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
        x = .data[[paste0(crop_in, '_acres_pctl')]], 
        y = .data[[paste0(crop_in, '_yield_low_pctl')]]
      )
    ) + 
    geom_point() + 
    geom_smooth() + 
    theme_minimal(base_size = 16) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
    scale_x_continuous(
      name = 'Pre-period Acreage Percentile', 
      label = scales::label_percent()  
    )+ 
    scale_y_continuous(
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
        x = .data[[paste0(crop_in, '_acres_pctl')]], 
        y = .data[[paste0(crop_in, '_yield_high_pctl')]]
      )
    ) + 
    geom_point() + 
    geom_smooth() + 
    theme_minimal(base_size = 16) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
    scale_x_continuous(
      name = 'Pre-period Acreage Percentile', 
      label = scales::label_percent()  
    )+ 
    scale_y_continuous(
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
