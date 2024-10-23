# Cleans the state-level GM adoption data 
library(pacman)
p_load(
  here, data.table, fst, ggplot2, janitor, tigris, 
  sf, dplyr, stringr
)
options(tigris_use_cache = TRUE)

# National data 
nat_dt = data.table(
  state = 'United States',
  year = rep(1996:1999, 3), 
  crop = rep(c('Soy','Corn','Cotton'), each = 4), 
  gm_type = rep('Any', 12),
  value = c(
    7.4 , 17, 44.2,55.8, # Soy
    1.4+3, 4.3+7.6, 9+19.1, 8+25.9, # Corn
    14.6, 15, 26.2, 42.1 # Cotton
  ) 
)
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

gm_adopt_map = 
  ggplot() + 
  geom_sf(
    data = states_sf, 
    fill = 'grey50', 
    color = 'black'
  ) + 
  geom_sf(
    data = gm_sf |>
      filter(year %in% c(2000,2005,2010) & gm_type == 'Any'), 
    aes(fill = value/100), 
    color = 'black'
  ) +
  scale_fill_viridis_c(
    name = 'GM Adoption Rate',
    labels = scales::label_percent(),
    option = 'magma', 
    direction = 1, 
    na.value = 'grey50'
  ) + 
  theme_void() + 
  facet_grid(cols = vars(crop), rows = vars(year)) + 
  theme(
    legend.position = 'bottom', 
    strip.text = element_text(size = 14)
  )
ggsave(
  plot = gm_adopt_map, 
  filename = here('figures/descriptive/gm-adoption-rate-map.jpeg'), 
  width = 8, height = 6
)


gm_adopt_ts = 
  ggplot(
    gm_dt[gm_type == 'Any' & state !='United States'], 
    aes(x = year, y = as.numeric(value)/100, group = state)
  ) + 
  geom_line(alpha = 0.25) + 
  geom_line(
    data = gm_dt[gm_type == 'Any' & state == 'United States'] |> rbind(nat_dt, use.names = TRUE, fill = TRUE), 
    linewidth = 1
  ) + 
  facet_grid(
  #  rows = vars(gm_type), 
    cols = vars(crop)
  ) + 
  theme_minimal() + 
  scale_x_continuous(
    name = 'Year', 
    breaks = seq(1995, 2020, by = 5) 
  ) + 
  scale_y_continuous(
    name = 'GM Adoption Rate', 
    labels = scales::label_percent()
  ) + 
  theme(
    panel.grid.minor = element_blank(), 
    strip.text = element_text(size = 14)
  )
ggsave(
  plot = gm_adopt_ts,
  filename = here('figures/descriptive/gm-adoption-rate-ts.jpeg'), 
  width = 8, height = 3
)


