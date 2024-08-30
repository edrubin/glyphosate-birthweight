# Cleaning the USGS 5-year county fertilizer data 
library(pacman)
p_load(
  here, data.table, fst, janitor, stringr, purrr
)

# Reading the farm + nonfarm data ---------------------------------------------
farmfert_dt_raw = 
  here('data/download-manual/Tabularcounty_l/fert-farm-1987-2017.txt') |>
  fread() |>
  clean_names() 
nonfarmfert_dt_raw = 
  here('data/download-manual/Tabularcounty_l/fert-nonfarm-1987-2017.txt') |>
  fread() |>
  clean_names() 
totfert_dt_raw = 
  here('data/download-manual/Tabularcounty_l/fert-total-1950-1982.txt') |>
  fread() |>
  clean_names() 
# Now adding the manure data 
fp_in = 
  list.files(
    here('data/download-manual/Tabularcounty_l'), 
    full.names = TRUE
  ) |>
  str_subset('manure\\d{4}\\.txt')
manure_dt = 
  map_dfr(
    fp_in, 
    \(x){
      tmp = fread(x)[,year := str_extract(x, '\\d{4}') |> as.numeric()] 
      tmp |> setnames( 
        old = colnames(tmp), 
        new = colnames(tmp) |> str_remove('\\d{4}') |> make_clean_names()
      )
      return(tmp)
    }
  )[,.(
    GEOID = str_pad(stcofips, width = 5, side = 'left', pad = '0'), 
    year, 
    n_manure = total_n_kg, 
    p_manure = total_p_kg
  )]
# Combining and putting years into rows
fert_dt = 
  merge(
    farmfert_dt_raw, 
    nonfarmfert_dt_raw, 
    by = c('stcofips','fips_int','county_name','state') 
  ) |>
  merge(
    totfert_dt_raw, 
    by = c('stcofips','fips_int','county_name','state') 
  ) |>
  melt(
    id.vars = 'stcofips', 
    measure.vars = patterns('fert')
  ) %>%
  .[,.(
    GEOID = str_pad(stcofips, width = 5, side = 'left', pad = '0'), 
    farm = fcase(
      str_detect(variable, 'nonf'), 'nonfarm',
      str_detect(variable, '^farm'), 'farm',
      str_detect(variable, 'tot_fert'), 'commercial'
    ), 
    nutrient = fifelse(str_detect(variable, '_n_'), 'n','p'), 
    year = str_extract(variable, '\\d{4}$') |> as.numeric(), 
    value
  )] %>%
  dcast(
    formula = GEOID + year ~ nutrient + farm, 
    value.var = 'value'
  ) %>%
  merge(
    manure_dt, 
    by = c('GEOID','year')
  )
# Calculating totals w/manure data
fert_dt[,':='(
  n_commercial = fifelse(
    is.na(n_commercial), 
    n_farm + n_nonfarm, 
    n_commercial 
  ),
  p_commercial = fifelse(
    is.na(p_commercial), 
    p_farm + p_nonfarm , 
    p_commercial 
  )
)][,':='(
  n_total = n_commercial + n_manure, 
  p_total = p_commercial + p_manure
)]
# Saving the results 
write.fst(
  fert_dt, 
  here('data/raw/fertilizer-dt.fst')
)


# Trying out some regressions 
p_load(ggplot2, fixest, collapse)

trt_dt = read.fst(here('data/clean/trt-dt.fst'), as.data.table = TRUE)
comb_cnty_health_dt = 
  read.fst(here('data/clean/comb-cnty-health-dt.fst'), as.data.table = TRUE)[
    year %in% c(1992:2012),
    .(tot_inf_births = fsum(tot_inf_births)),
    keyby = .(GEOID, rural, area_km2)
  ]

est_dt = 
  merge(
    fert_dt, 
    trt_dt, 
    by = 'GEOID'
  ) |>
  merge(
    comb_cnty_health_dt, 
    by = 'GEOID'
  )
est_dt[,state_fips := str_sub(GEOID,1,2)]

# First a simple plot to show what is going on 
ggplot(
  melt(
    est_dt, 
    id.vars = c('GEOID','year','all_yield_diff_gmo_50_0','area_km2'), 
    measure.vars = colnames(fert_dt)[-(1:2)]
  )[,
    .(avg = fmean(value)), 
    by = .(variable, year, all_yield_diff_gmo_50_0)
  ], 
  aes(x = year, y = avg, color = all_yield_diff_gmo_50_0)
) + 
geom_vline(xintercept = 1995, linetype = 'dashed') +
geom_line() +
geom_point() +
scale_color_viridis_d(
  name = 'Suitability', 
  option = 'magma', 
  labels = c('Low','High'), 
  begin = 0.2, end = 0.8
) +
scale_y_continuous(
  name = 'Nutrient weight (kg e-6)', 
  labels = scales::label_number(scale = 1e-6, big.mark = ',', )
) +
scale_x_continuous(name = 'Year') + 
theme_minimal() + 
facet_wrap(
  ~variable, 
  scales = 'free'
)



fert_mod_logs = feols(
  data = est_dt, 
  split = ~rural, 
  weight = ~tot_inf_births,
  fml = c(log(n_commercial/area_km2), log(p_commercial/area_km2), log(n_total/area_km2), log(p_total/area_km2), log(n_manure/area_km2), log(p_manure/area_km2)) ~ i(year, all_yield_diff_percentile_gmo, ref = '1992') | year + GEOID, 
  cluster = ~state_fips + year
)
fert_mod_levels = feols(
  data = est_dt, 
  split = ~rural, 
  weight = ~tot_inf_births,
  fml = c(n_commercial/area_km2, p_commercial/area_km2, n_total/area_km2, p_total/area_km2, n_manure/area_km2, p_manure/area_km2) ~ i(year, all_yield_diff_percentile_gmo, ref = '1992') | year + GEOID, 
  cluster = ~state_fips + year
)

# For rural places
fert_result_dt = data.table(coeftable(fert_mod_logs))[,.(
  id, 
  rural = sample, 
  lhs,
  nutrient = fifelse(str_detect(lhs, 'n_'), 'Nitrogen', 'Phosphorous'),
  type = fcase(
    str_detect(lhs, 'commercial'), 'Commercial', 
    str_detect(lhs, 'manure'), 'Manure', 
    str_detect(lhs, 'total'), 'Total'
  ) |> factor(levels = c('Commercial','Manure','Total')), 
  year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.numeric(), 
  estimate = Estimate, 
  ci_l = Estimate + qnorm(0.025)*`Std. Error`, 
  ci_h = Estimate + qnorm(0.975)*`Std. Error`
)][,lhs_name := paste(nutrient, type)]
# Add the reference bin 
fert_result_dt =
  rbind(
    fert_result_dt[year != 1992],
    fert_result_dt[,.(
      id, rural, lhs, nutrient, type, lhs_name,
      year = 1992, 
      estimate = 0, ci_l = 0, ci_h = 0
    )] |> unique()
  )

ggplot(fert_result_dt[rural == TRUE], aes(x = year, y = estimate, color = nutrient, fill = nutrient)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 1995, linetype = 'dashed') + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = ci_l, ymax = ci_h), alpha = 0.3, color = NA) + 
  theme_minimal() + 
  scale_y_continuous(
    name = 'Partial Elasticity', 
    label = scales::label_percent()
  ) +
  scale_x_continuous(
    name = '', breaks = seq(1950,2020, by = 10)
  ) +
  scale_color_brewer(
    name = 'Nutrient',
    palette = 'Dark2',
    aesthetics = c('color','fill')
  ) +
  facet_wrap(~type, scales = 'free') + 
  theme(legend.position = 'bottom')


# For rural places
fert_result_dt_levels = data.table(coeftable(fert_mod_levels))[,.(
  id, 
  rural = sample, 
  lhs,
  nutrient = fifelse(str_detect(lhs, 'n_'), 'Nitrogen', 'Phosphorous'),
  type = fcase(
    str_detect(lhs, 'commercial'), 'Commercial', 
    str_detect(lhs, 'manure'), 'Manure', 
    str_detect(lhs, 'total'), 'Total'
  ) |> factor(levels = c('Commercial','Manure','Total')), 
  year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.numeric(), 
  estimate = Estimate, 
  ci_l = Estimate + qnorm(0.025)*`Std. Error`, 
  ci_h = Estimate + qnorm(0.975)*`Std. Error`
)][,lhs_name := paste(nutrient, type)]
# Add the reference bin 
fert_result_dt_levels =
  rbind(
    fert_result_dt_levels[year != 1992],
    fert_result_dt_levels[,.(
      id, rural, lhs, nutrient, type, lhs_name,
      year = 1992, 
      estimate = 0, ci_l = 0, ci_h = 0
    )] |> unique()
  )

ggplot(fert_result_dt_levels[rural == TRUE], aes(x = year, y = estimate, color = nutrient, fill = nutrient)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 1995, linetype = 'dashed') + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = ci_l, ymax = ci_h), alpha = 0.3, color = NA) + 
  theme_minimal() + 
  scale_y_continuous(
    name = 'Estimate (kg/km2)'
    #label = scales::label_number(scale = 1e-6)
  ) +
  scale_x_continuous(
    name = '', breaks = seq(1950,2020, by = 10)
  ) +
  scale_color_brewer(
    name = 'Nutrient',
    palette = 'Dark2',
    aesthetics = c('color','fill')
  ) +
  facet_wrap(~nutrient + type, scales = 'free') + 
  theme(legend.position = 'bottom')
