# Comparing use intensities for US, EU, Brazil 
library(pacman)
p_load(
  here, data.table, fst, haven, collapse, readxl, janitor
)

# EU data: Table 3 of https://www.mdpi.com/2071-1050/12/14/5682 
  eu_gly_dt = data.table(
    year = 2013:2017,
    gly_ton = c(41814,43355,42000,43319,44250)
  )
  # France: c(9370,10070,9110,9110,9324)
  #eu_acre_dt =
  #  read_xlsx(here(
  #    'data/download-manual/replication_package_MS30253',
  #    'eu-acre-data.xlsx'
  #  )) |>
  #  clean_names() |>
  #  data.table() 
  #
  eu_dt = 
    eu_gly_dt[,.(
      year, 
      country = 'EU',
      gly_ton, 
      temp_ha = 166120*1000, 
      gly_kg_ha = (gly_ton*1000)/(166120*1000)
    )]
# Loading Dias et al data 
  # Here is annual national GLY data from here('Codes/data_glyph_pot_subbasins.do') 
  dias_gly_dt = data.table(
    year = 2000:2010,
    gly_ton = c(39515,44467,43691,57614,77068,70954,82836,94719,106602,118485,127586)
  )
  dias_crop_dt = 
    read_xlsx(
      here(
        'data/download-manual/replication_package_MS30253/Data',
        'Originais/area_temp_1996_2010.xlsx'
      ),
      skip = 4
    ) |>
    setnames(c('level','code','mun',1996:2010)) |>
    data.table() |>
    melt(
      id.vars = c('level','code','mun'),
      variable.name = 'year', 
      value.name = 'temp_ha'
    ) %>%
    .[level == 'MU']
  # Turn into number 
  dias_crop_dt[,temp_km2 := as.numeric(temp_ha)*0.01]
  dias_crop_dt[,year := as.integer(as.character(year))]
  dias_dt = 
    merge(
      dias_gly_dt,
      dias_crop_dt[,.(temp_km2 = sum(temp_km2, na.rm = TRUE)), keyby = year],
      by = 'year'
    )[,':='(
      country = 'Brazil',
      gly_kg_ha = (gly_ton*1000)/(temp_km2*100)
    )]

# Loading US version of this 
us_dt =
  read.fst(
    here('data/clean/comb-cnty-dt.fst'), 
    as.data.table = TRUE
  )[,.(
    gly_ton = sum(glyphosate, na.rm = TRUE), 
    temp_km2 = sum(tot_acres, na.rm = TRUE)*0.00404686),
    keyby = year
  ][,':='(
    country = 'US',
    gly_kg_ha = (gly_ton*1000)/(temp_km2*100)
  )]




all_dt = rbind(dias_dt, us_dt, eu_dt, fill = TRUE)

p_load(ggplot2)

gly_intensity_p = 
  ggplot(data = all_dt, aes(x = year, y = gly_kg_ha, color = country)) + 
  geom_point() + 
  geom_line() +
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2', name = '') + 
  labs(y = 'GLY (kg/hectare)', x = 'Year') + 
  theme(legend.position = 'bottom')
ggsave(
  gly_intensity_p, 
  filename = here('figures/descriptive/country-gly-intensity.jpeg'), 
  width = 6, height = 4, 
  bg = 'white'
)
