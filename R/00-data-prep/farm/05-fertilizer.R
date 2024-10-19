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

# Function to interpolate data for a single county
interpolate_county = function(GEOID_in, variable_in, fert_dt_long){
  print(paste('starting', GEOID_in, variable_in))
  # Filter to just county and variable
  county_fert_dt = fert_dt_long[
    GEOID == GEOID_in & 
    variable == variable_in & 
    !is.na(value)
  ]
  # Fit and make predictions
  spline_fit = smooth.spline(county_fert_dt$year, county_fert_dt$value)
  pred_value = predict(
    spline_fit, 
    seq(min(county_fert_dt$year), max(county_fert_dt$year), by = 1)
  )
  return(data.table(
    GEOID = GEOID_in, 
    variable = variable_in,
    year = pred_value$x, 
    value = pred_value$y
  ))
}
# Running the interpolation 
p_load(purrr)
fert_dt_long = melt(fert_dt, id.vars = c('GEOID','year'))[!is.na(value)]
to_fit = fert_dt_long[,.N,keyby = .(GEOID, variable)][N >= 4] 
annual_fert_dt_long = 
  pmap_dfr(
    to_fit[,.(GEOID, variable)], 
    interpolate_county, 
    fert_dt_long = fert_dt_long
  )
annual_fert_dt = dcast(
  annual_fert_dt_long, 
  formula = GEOID + year ~ variable, 
  value.var = 'value'
)
# Saving the results 
write.fst(
  annual_fert_dt, 
  here('data/raw/fertilizer-dt-interpolated.fst')
)

# Plotting interpolation for select counties ----------------------------------
p_load(ggplot2, fixest, collapse)
dir.create('figures/descriptive/fertilizer', showWarnings = FALSE)
annual_fert_dt = read.fst(
  here('data/raw/fertilizer-dt-interpolated.fst'), 
  as.data.table = TRUE
)
tmp = merge(
  annual_fert_dt, 
  fert_dt_long, 
  by = c('GEOID','variable','year'), 
  all.x = TRUE
)
var_dt = 
  fert_dt_long[,
    .(variance = var(value)), 
    keyby = .(GEOID, variable)
  ][,rank := rank(-variance), by = variable]
interp_p = 
  merge(
    tmp, 
    var_dt[str_detect(variable, 'commercial|manure') & rank <= 3], 
    by = c('GEOID','variable')
  ) |>
  ggplot(aes(x = year, color = GEOID)) + 
  geom_line(aes(y = value.x)) + 
  geom_point(aes(y = value.y)) + 
  facet_wrap(~str_replace(variable, '_',' ') |> str_to_title(), scales = 'free') + 
  scale_y_continuous(
    name = "Nutrient Mass (kg e-7)", 
    label = scales::label_number(scale = 1e-7)
  ) + 
  scale_x_continuous(name = '', breaks = seq(1950,2020, by = 20)) + 
  scale_color_brewer(name = 'County FIPS', palette = 'Dark2') +
  theme_minimal(base_size = 12)
ggsave(
  interp_p, 
  filename = here('figures/descriptive/fertilizer/interpolation.pdf'), 
  device = cairo_pdf(), 
  dpi = 600, 
  width = 6, height = 4 
)