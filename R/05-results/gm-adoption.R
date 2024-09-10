# Cleans the state-level GM adoption data 
library(pacman)
p_load(
  here, data.table, fst, ggplot2, janitor, tigris, 
  sf, dplyr, stringr
)
options(tigris_use_cache = TRUE)

# Reading GM data
gm_dt = fread(here('data/download-manual/BiotechCropsAllTables2024.csv')) |>
  clean_names()
gm_dt[,':='(
  crop = str_extract(attribute,'corn|soy|cotton') |> str_to_title(), 
  gm_type = fcase(
    str_detect(attribute, 'All GE varieties'), 'Any', 
    str_detect(attribute, 'Herbicide'), 'HT', 
    str_detect(attribute, 'Insect'), 'BT', 
    str_detect(attribute, 'Stacked'), 'Stacked'
  ), 
  value = fifelse(value == '*', 0.5, as.numeric(value))
)]
# Getting state shapes 
states_sf = states(cb = TRUE, year = 2021) |> 
  st_transform(crs = 9311) |>
  clean_names() |>
  filter( # Limiting to continental US
    !(statefp %in% c("02","15","60","66","69","72","78"))
  ) 
gm_sf = left_join(states_sf, gm_dt, by = c('name'  ='state'))
# Exploring data
gm_dt[,.N,keyby = attribute]
gm_dt[,.N,keyby = year]
gm_dt[,.N,keyby = state]

ggplot() + 
  geom_sf(data = states_sf, fill = NA, color = 'black') + 
  geom_sf(
    data = gm_sf |>
      filter(year %in% c(2000,2005,2010) & gm_type == 'Any'), 
    aes(fill = value/100)
  ) +
  scale_fill_viridis_c(
    name = 'Percent Adoption',
    labels = scales::label_percent(),
    option = 'magma'
  ) + 
  theme_void() + 
  facet_grid(cols = vars(crop), rows = vars(year)) + 
  theme(legend.position = 'bottom')

ggplot(
  gm_dt, 
  aes(x = year, y = as.numeric(value)/100, group = state)
) + 
geom_line(alpha = 0.25) + 
facet_grid(rows = vars(gm_type), cols = vars(crop)) + 
theme_minimal() + 
scale_x_continuous(
  name = '', breaks = seq(2000, 2020, by = 5) 
) + 
scale_y_continuous(
  name = 'Adoption', 
  labels = scales::label_percent()
)


zip_sf = zctas(cb = TRUE, year = 2020) |> st_transform(crs = 9311)

ggplot() + 
  geom_sf(data = zip_sf, aes(fill = ALAND20), color = NA) +
  geom_sf(data= states_sf, color = 'black', fill = NA)
