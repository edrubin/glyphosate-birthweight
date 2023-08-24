# Creating a panel of consistent fips codes
library(pacman)
p_load(
  here, fst, data.table, tigris, stringr, sf, dplyr, janitor,
  purrr, collapse
)
options(tigris_use_cache = TRUE)
# Loading county fips codes from tigris ---------------------------------------
  # State fips codes 
  state_sf = states(year = 2010, cb = TRUE)
  state_dt = 
    data.table(state_sf)[ # Limiting to continental US
      !(STATE %in% c("02","15","60","66","69","72","78")),
      .(state_fips = STATE, name = NAME)
    ]
  county_sf =
    map_dfr(
      state_dt$state_fips,
      counties,
      year = 2010,
      cb = TRUE
    ) |> 
    clean_names() |>
    st_simplify(dTolerance = 0.001) |>
    mutate(
      GEOID = paste0(statefp,countyfp)
    ) |>
    st_transform(crs = 2163) 

# Converting into a balanced panel --------------------------------------------
  county_dt = data.table(county_sf)[,.(
    GEOID, state_fips = statefp
  )]
  county_year_dt = CJ(GEOID = county_dt$GEOID, year = 1990:2017)

# Creating crosswalk for fips codes that may have changed ---------------------

change_dt = data.table(
  GEOID = c(12086, 51083, 46113, 51005),
  other_geoid = c(12025, 51780, 46102, 51560),
  year_switch = c(2006, , 2013)
)


p_load(ggplot2)
ggplot(county_sf) + geom_sf()



# First checking pesticide data -----------------------------------------------
  pest_dt = fread(here("data/pesticides/est_pest_use.csv"))
  tmp_pest = 
    merge(
      county_year_dt[year %in% 1992:2017,.(GEOID, year, in_panel = 1)],
      pest_dt[,.(GEOID = str_pad(GEOID,5,'left','0'), year, in_pest = 1)],
      by = c('GEOID','year'),
      all = TRUE
    )
  tmp_pest[is.na(in_panel)]

# Now checking health data ----------------------------------------------------
  health_dt = read.fst(here("data-clean/health-dt.fst"), as.data.table = TRUE)
  tmp_health =
    merge(
      county_year_dt[year %in% 1990:2013,.(GEOID, year, in_panel = 1)],
      health_dt[
        !(str_sub(GEOID,1,2) %in% c("02","15","60","66","69","72","78")),
        .(GEOID, year, in_health = 1)
      ],
      by = c('GEOID','year'),
      all = TRUE
    )
  tmp_health[is.na(in_panel),.N,keyby = GEOID]
  # 1 infant death in 13999 
  # Many in "NA000" 2003-2013
  health_dt[GEOID == 'NA000']
  # Many in "00000" 1990-2002
  health_dt[GEOID == '00000']
  # Some in "NA999" 2003
  health_dt[GEOID == 'NA999']
  # Todo: fix NA/000,999, 12025->12086, 51560->51005, 51780->51083
  health_dt[GEOID %in% c(12025, 12086), .N, keyby = year][,.N,by = N]
  health_dt[GEOID %in% c(51560, 51005), .N, keyby = year][,.N,by = N]
  health_dt[GEOID %in% c(51780,51083), .N, keyby = year][,.N,by = N]


# Now checking the instruments ------------------------------------------------
  trt_dt = read.fst(
      path = here('data-clean/trt-dt.fst'),
      as.data.table = TRUE
  )
  tmp_trt =
    merge(
      county_year_dt[,.(GEOID, year, in_panel = 1)],
      trt_dt[,.(GEOID, in_trt = 1)],
      by = c('GEOID'),
      all = TRUE
    )
  tmp_trt[is.na(in_panel)]
  tmp_trt[is.na(in_trt)]


# What happens when we merge all of these together ----------------------------

tmp_all = 
    merge(
      county_year_dt[,.(GEOID, year, in_panel = 1)],
      pest_dt[,.(GEOID = str_pad(GEOID,5,'left','0'), year, in_pest = 1)],
      by = c('GEOID','year'),
      all = TRUE
    ) |>
    merge(
      health_dt[
        !(str_sub(GEOID,1,2) %in% c("02","15","60","66","69","72","78")),
        .(GEOID, year, in_health = 1, tot_inf_births)
      ],
      by = c('GEOID','year'),
      all = TRUE
    ) |>
    merge(
      trt_dt[,.(GEOID, in_trt = 1)],
      by = c('GEOID'),
      all = TRUE
    )

tmp_all[,.(
  in_panel = sum(!is.na(in_panel)),
  in_all = sum(!is.na(in_panel + in_pest + in_health + in_trt)),
  in_pest = sum(!is.na(in_panel + in_pest)),
  in_health = sum(!is.na(in_panel + in_health)),
  in_trt = sum(!is.na(in_panel + in_trt)),
  total = .N,
  pct_pest = fsum(ifelse(!is.na(in_panel + in_pest), tot_inf_births, 0))/fsum(tot_inf_births),
  pct_health = fsum(ifelse(!is.na(in_panel + in_health), tot_inf_births, 0))/fsum(tot_inf_births),
  pct_trt = fsum(ifelse(!is.na(in_panel + in_trt), tot_inf_births, 0))/fsum(tot_inf_births)
), 
  keyby = year
  ]



# Load the processed double-difference dataset
comb_cnty_health_dt = read_fst(
  path = here('data-clean/comb-cnty-health-dt.fst'),
  as.data.table = TRUE
)[,state_fips := str_sub(GEOID, 1,2)]

test_dt = 
  merge(
    comb_cnty_health_dt[,.(year, GEOID, state_fips, glyph_km2, tot_inf_births)],
    state_dt,
    by = 'state_fips',
  )

# First cbecking where glyphosate is missing 
test_dt[is.na(glyph_km2)]

test_dt[GEOID == '12025']


test_dt[is.na(tot_inf_births)]

test_dt[GEOID == '12086']


# Lets plot where the pesticide data is missing 
ggplot() + 
  geom_sf(data = state_sf |> filter(!(STATE %in% c("02","15","60","66","69","72","78"))), fill = NA, color = 'black') + 
  geom_sf(data = county_sf |> filter(GEOID %in% unique(tmp_pest[is.na(in_pest)]$GEOID)), fill = 'red')

ggplot() + 
  geom_sf(data = state_sf |> filter(!(STATE %in% c("02","15","60","66","69","72","78"))), fill = NA, color = 'black') + 
  geom_sf(data = county_sf |> filter(GEOID %in% unique(tmp_health[is.na(in_health)]$GEOID)), fill = 'orange')
