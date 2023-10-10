# This aggregates the prism precipitation data to the watershed level 
library(pacman)
p_load(
  here, data.table, stars, sf, tigris, dplyr, janitor, 
  stringr, lubridate, magrittr, exactextractr, fst, collapse
)
options(tigris_use_cache=TRUE)
sf_use_s2(FALSE)

# List of the monthly file names 
all_files = list.files(here('data/watershed/prism'), full.names = TRUE)
all_months = str_extract(all_files, "(?<=4kmM3_)\\d{6}(?=_bil)")
file_paths = here(paste0(
  "data/watershed/prism/", 
  all_files,
  "/PRISM_ppt_stable_4kmM3_",all_months,"_bil.bil"
))

# Reading in all data---adding the date as an attribute
ppt_month_st = 
  read_stars(
    file_paths, 
    along = list(time = ym(all_months))
  ) |>
  setNames("ppt")

# Getting state shapes from tigris package
states_sf = 
  states(year = 2000) |>
  filter( # Limiting to continental US
    !(STATEFP00 %in% c("02","15","60","66","69","72","78"))
  ) |>
  st_transform(crs = st_crs(ppt_month_st)) |>
  clean_names()

# CONUS sf
cont_sf = st_union(states_sf) |> st_as_sf() |> mutate(in_us = TRUE)

# Loading HydroBASINS data limiting to those in the CONUS
watershed_sf = 
  read_sf(here("data/watershed/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
  st_transform(crs =  st_crs(ppt_month_st)) |>
  clean_names() |>
  st_join(cont_sf) |>
  filter(in_us == TRUE)|>
  st_make_valid()

# Aggregating by quarter
ppt_qtr_st = aggregate(
  ppt_month_st, 
  by = 'quarter', 
  FUN = mean,
  na.rm = TRUE
)

# Aggregating by watershed
ppt_qtr_wshd_st = aggregate(
  ppt_qtr_st, 
  by = watershed_sf,
  FUN = mean,
  na.rm = TRUE
)

# Turning it into an long data.table 
ppt_qtr_wshd_dt = 
  ppt_qtr_wshd_st |>
  st_as_sf(long = TRUE) |>
  data.table() %>% 
  .[,.(
    hybas_id = rep(watershed_sf$hybas_id, times = 30),
    year = year(time), 
    quarter = quarter(time), 
    ppt
  )]

# Aggregating by growing season (Q2/3) and by year
ppt_season_wshd_dt = 
  ppt_qtr_wshd_dt[,
    .(ppt = fsum(ppt)), 
    keyby = .(hybas_id, year, growing_season = quarter %in% 2:3)
  ]
ppt_yr_wshd_dt = 
  ppt_qtr_wshd_dt[,
    .(ppt = fsum(ppt)), 
    keyby = .(hybas_id, year)
  ]

# Saving the results
write.fst(
  ppt_qtr_wshd_dt,
  here('data/watershed/ppt-qtr-wshd-dt.fst')
)
write.fst(
  ppt_season_wshd_dt,
  here('data/watershed/ppt-season-wshd-dt.fst')
)
write.fst(
  ppt_yr_wshd_dt,
  here('data/watershed/ppt-yr-wshd-dt.fst')
)

# Seems like 600 watersheds have missing data: 
# This is because aggreate only assigns grid cells to a single
# geometry, can't get exact to work...
#ppt_yr_wshd_dt[,.(avg_missing = mean(is.na(ppt))), by = hybas_id][avg_missing>0]
#p_load(ggplot2)
#ggplot() + 
#  geom_stars(data = ppt_yr_wshd_st[,,1]) + 
#  scale_fill_viridis_c()

