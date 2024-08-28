# Cleaning the USGS 5-year county fertilizer data 
library(pacman)
p_load(
  here, data.table, fst, readxl, janitor, stringr
)

# Reading the farm + nonfarm data ---------------------------------------------
farmfert_dt_raw = 
  here('data/download-manual/Tabularcounty_l/N-P_from_fertilizer_1950-2017-july23-2020.xlsx') |>
  read_xlsx(sheet = 'farm') |>
  data.table() |>
  clean_names() 
nonfarmfert_dt_raw = 
  here('data/download-manual/Tabularcounty_l/N-P_from_fertilizer_1950-2017-july23-2020.xlsx') |>
  read_xlsx(sheet = 'nonfarm') |>
  data.table() |>
  clean_names() 
# Combining and putting years into rows
fert_dt = 
  merge(
    farmfert_dt_raw, 
    nonfarmfert_dt_raw, 
    by = c('stcofips','fips_int','county_name','state') 
  ) |>
  melt(
    id.vars = 'stcofips', 
    measure.vars = patterns('fert')
  ) %>%
  .[,.(
    GEOID = stcofips, 
    farm = fifelse(str_detect(variable, 'nonf'), 'nonfarm','farm'), 
    nutrient = fifelse(str_detect(variable, '_n_'), 'nitrogen','phosphorous'), 
    year = str_extract(variable, '\\d{4}$') |> as.numeric(), 
    value
  )] %>%
  dcast(
    formula = GEOID + year ~ nutrient + farm, 
    value.var = 'value'
  ) 

p_load(ggplot2)
ggplot(fert_dt, aes(x = year, y = nitrogen_farm, group = GEOID)) + 
geom_line() 

ggplot(fert_dt, aes(x = nitrogen_farm, y = phosphorous_farm, color = as.character(year))) + 
geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + 
#geom_point(alpha = 0.15) + 
geom_smooth(method = 'loess', se = FALSE) + 
scale_color_viridis_d(
  option = 'magma', 
  name = 'Year', 
  end = 0.9
) + 
theme_minimal()

# Reading the manure data -----------------------------------------------------