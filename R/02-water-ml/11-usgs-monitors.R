
# This script looks at the correlation between glyph water samples and 
# local/upstream glyphosate sprayed
library(pacman)
p_load(
  here, data.table, janitor, lubridate, sf, tigris, dplyr, purrr, units,
  fst, ggplot2, fixest,latex2exp, stringr
)
theme_set(theme_minimal())
options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# Loading all of the data ---------------------------------------------------
# Loading the USGS water sampling data 
site_dt = fread(here(
  "data/watershed/usgs-monitoring/Table1. NWQN.SiteInformation.txt"
  )) |> clean_names()
sample_dt = fread(here(
  "data/watershed/usgs-monitoring/Table2. Glyphosate.AMPA.SampleConc.txt"
  )) |> clean_names()
annual_dt = fread(here(
  "data/watershed/usgs-monitoring/Table3. Glyphosate.AMPA.AnnualData.txt"
  )) |> clean_names()
landuse_dt = fread(here(
  "data/watershed/usgs-monitoring/Table5. LandUseClass.txt"
  )) |> clean_names()

# Getting state shapes from tigris package
states_sf = 
  states(year = 2000, cb = TRUE) |>
  filter( # Limiting to continental US
    !(STATE %in% c("02","15","60","66","69","72","78"))
  ) |>
  clean_names() |>
  st_transform(crs = 2163)
# Loading county shapes
county_sf =
  map_dfr(
    states_sf$state,
    counties,
    year = 2010,
    cb = TRUE
  ) |> 
  clean_names() |>
  mutate(
    geoid = paste0(statefp,countyfp),
    area_km2 = st_area(geometry) |> set_units(km^2) |> drop_units()
  )  |>
  st_simplify(dTolerance = 0.01) |>
  st_transform(crs = 2163) 
# Loading HydroBASINS data limiting to those in the continental US
hydrobasin_sf = 
  read_sf(here("data/watershed/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
  st_transform(crs = 2163) |>
  #st_simplify(dTolerance = 500) |>
  clean_names()

# Loading upstream pesticides 
pesticide_upstream_dt = read.fst(
  path = here("data/watershed/pesticide-upstream-dt.fst"),
  as.data.table = TRUE
)#[local == FALSE]

# Checking percent of samples that gly/ampa detected in -----------------------
  sample_dt[,.(
    detect_gly = mean(gly_rem != '<'),
    detect_ampa = mean(ampa_rem != '<'),
    detect_either = mean(gly_rem != '<' | ampa_rem != '<'),
    detect_both = mean(gly_rem != '<' & ampa_rem != '<')
  )]


# Data cleaning and prep ----------------------------------------------------
# Adding site lat/lon to sample data and grabbing relevant variables 
monitor_dt = 
  merge(
    sample_dt[!is.na(site_no), .(
      site_no,
      sample_date = dmy(sample_start_date),
      quarter = quarter(dmy(sample_start_date)),
      year = ifelse(
        quarter(dmy(sample_start_date)) == 1, 
        year(dmy(sample_start_date)) - 1,
        year(dmy(sample_start_date))
      ),
      gly_result = ifelse(gly_rem == '<',0, gly_result), 
      ampa_result = ifelse(ampa_rem == '<',0, ampa_result)
    )],
    site_dt,
    by = "site_no"
  )

# Converting them into an sf object 
site_sf = st_as_sf(site_dt, coords = c("longitude","latitude")) |>
  st_set_crs("NAD83") |>
  st_transform(crs = 2163)
monitor_sf = st_as_sf(monitor_dt, coords = c("longitude","latitude")) |>
  st_set_crs("NAD83") |>
  st_transform(crs = 2163)

# Creating a monitor-hydroshed crosswalk
site_watershed_dt = 
  st_join(site_sf,hydrobasin_sf) |>
  st_join(states_sf) |>
  as.data.table() %>% 
  .[,.(site_no, hybas_id, state = name)]

# Merging sample data to watershed data
monitor_watershed_dt = 
  merge(
    monitor_dt,
    site_watershed_dt, 
    by = "site_no"
  ) |>
  merge(
    pesticide_upstream_dt,
    by = c("hybas_id","year")
  )


# Visualizations and such ---------------------------------------------------
# Checking where the sample sites are 
ggplot() + 
  geom_sf(data = states_sf, color = "black", fill = "white") + 
  geom_sf(data = site_sf, aes(color = region))

# Histogram of sample results 
ggplot(monitor_dt, aes(x = gly_result)) + 
  scale_x_log10() + 
  geom_histogram(binwidth = 0.1)+ 
  labs(x = 'Glyphosate Concentration', y = 'Count')

# Monthly concentrations by land use class
ggplot(
  monitor_dt, 
  aes(x = month(sample_date), y = gly_result, color = wshed_land_use_class)
) +
  geom_smooth() +
  scale_x_continuous(labels = 1:12, breaks = 1:12) + 
  scale_color_viridis_d("Land Use") + 
  labs(x = "Month", y = "Glyphosate Concentration") 

# Monthly concentrations by region
glyph_month_p = 
  ggplot(monitor_dt, aes(x = month(sample_date), y = gly_result, color = region)) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(labels = 1:12, breaks = 1:12) + 
  scale_color_viridis_d("Region") + 
  labs(x = "Month", y = "Glyphosate Concentration") +
  theme_minimal(base_size = 18)
ggsave(
  plot = glyph_month_p,
  filename = here("figures/descriptive/glyph_concentrations.jpeg"),
  width = 10, height = 7
)

# Avg monthly concentrations for glyph and ampa
month_avg_dt = 
  monitor_dt[!(region %in% c('Pacific','West')),.(
    median_glyph = median(gly_result),
    avg_glyph = mean(gly_result),
    sd_glyph = sd(gly_result),
    median_ampa = median(ampa_result),
    avg_ampa = mean(ampa_result),
    sd_ampa = sd(ampa_result),
    glyph_25 = quantile(gly_result, probs = 0.25),
    glyph_75 = quantile(gly_result, probs = 0.75),
    ampa_25 = quantile(ampa_result, probs = 0.25),
    ampa_75 = quantile(ampa_result, probs = 0.75),
    glyph_det = mean(gly_result > 0),
    ampa_det = mean(ampa_result > 0),
    n = .N), 
    keyby = .( month = month(sample_date, label = TRUE))
  ][,':='(
    l_glyph = median_glyph + sd_glyph*qt(0.025,n-1),
    h_glyph = median_glyph + sd_glyph*qt(0.975,n-1),
    l_ampa = median_ampa + sd_ampa*qt(0.025,n-1),
    h_ampa = median_ampa + sd_ampa*qt(0.975,n-1)
  )] 

month_distr_p = 
  cbind(
    melt(
      month_avg_dt,
      id.vars = c('month'),
      measure.vars = c('median_glyph','median_ampa'),
      value.name = 'mean',
      variable.name = 'analyte'
    ),
    melt(
      month_avg_dt,
      id.vars = c('month'),
      measure.vars = c('glyph_25','ampa_25'),
      value.name = 'ci_l'
    )[,.(ci_l)],
    melt(
      month_avg_dt,
      id.vars = c('month'),
      measure.vars = c('glyph_75','ampa_75'),
      value.name = 'ci_h'
    )[,.(ci_h)] 
    ) |>
    ggplot(aes(x = month, y = mean, color = analyte))  +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_linerange(size = 1.2, aes(ymin = ci_l, ymax = ci_h), position = position_dodge(width = 0.5)) + 
    geom_hline(yintercept = 0) +
    scale_color_viridis_d(name='Analyte', labels = c('Glyphosate','AMPA'), begin = 0.15, end = 0.85, direction = -1)+
    labs(
      x = "Month", y = TeX("Sample Concentration ($\\mu g/L$)"),
      caption = 'Points show the median and lines the 25th to 75th percentile.'
    ) 
ggsave(
  plot = month_distr_p + theme_minimal(base_size = 16),
  filename = here("figures/descriptive/month_gly_ampa_concentrations.jpeg"),
  width = 8, height = 5,
  bg = 'white'
)

# Saving tables 
monitor_dt[,.( #!(region %in% c('Pacific','West'))
  median_glyph = median(gly_result),
  avg_glyph = mean(gly_result),
  sd_glyph = sd(gly_result),
  median_ampa = median(ampa_result),
  avg_ampa = mean(ampa_result),
  sd_ampa = sd(ampa_result),
  glyph_p25 = quantile(gly_result, probs = 0.25),
  glyph_p75 = quantile(gly_result, probs = 0.75),
  ampa_p25 = quantile(ampa_result, probs = 0.25),
  ampa_p75 = quantile(ampa_result, probs = 0.75),
  pr_glyph_det = mean(gly_result > 0),
  pr_ampa_det = mean(ampa_result > 0),
  n = .N), 
  keyby = .( 
    month = month(sample_date), 
    month_lab = month(sample_date, label = TRUE)
  )
] |>
fwrite(here('data/watershed/month-avg-concentration-all.csv'))
monitor_dt[!(region %in% c('Pacific','West')),.( 
  median_glyph = median(gly_result),
  avg_glyph = mean(gly_result),
  sd_glyph = sd(gly_result),
  median_ampa = median(ampa_result),
  avg_ampa = mean(ampa_result),
  sd_ampa = sd(ampa_result),
  glyph_p25 = quantile(gly_result, probs = 0.25),
  glyph_p75 = quantile(gly_result, probs = 0.75),
  ampa_p25 = quantile(ampa_result, probs = 0.25),
  ampa_p75 = quantile(ampa_result, probs = 0.75),
  pr_glyph_det = mean(gly_result > 0),
  pr_ampa_det = mean(ampa_result > 0),
  n = .N), 
  keyby = .( 
    month = month(sample_date), 
    month_lab = month(sample_date, label = TRUE)
  )
] |>
fwrite(here('data/watershed/month-avg-concentration-no_west.csv'))
monitor_dt[,.( 
  median_glyph = median(gly_result),
  avg_glyph = mean(gly_result),
  sd_glyph = sd(gly_result),
  median_ampa = median(ampa_result),
  avg_ampa = mean(ampa_result),
  sd_ampa = sd(ampa_result),
  glyph_p25 = quantile(gly_result, probs = 0.25),
  glyph_p75 = quantile(gly_result, probs = 0.75),
  ampa_p25 = quantile(ampa_result, probs = 0.25),
  ampa_p75 = quantile(ampa_result, probs = 0.75),
  pr_glyph_det = mean(gly_result > 0),
  pr_ampa_det = mean(ampa_result > 0),
  n = .N), 
  keyby = .( 
    month = month(sample_date), 
    month_lab = month(sample_date, label = TRUE),
    region
  )
] |>
fwrite(here('data/watershed/month-avg-concentration-region.csv'))



# Regression of upstream variables on glyphosate concentrations 
upstream_mod = feols(
  data = monitor_watershed_dt[
    region %in% c('Midwest','South','Northeast') 
    #&dist_km_bin != 'dn50'
  ],
  fml = c(log(gly_result), log(ampa_result)) ~ 
    sw(
      #i(dist_km_bin,glyphosate_awt),
      #i(dist_km_bin,glyphosate_awt/hybas_area_km2),
      #i(dist_km_bin,glyph_km2_awt),
      i(dist_km_bin,high_kls_ppt_growing_glyph_awt),
      i(dist_km_bin,high_kls_ppt_growing_e100m_yield_diff_gmo_50_0),
      i(dist_km_bin,high_kls*high_ppt_growing_season*glyphosate_awt)
    ) | month(sample_date)^year(sample_date) + site_no,
  cluster = ~ state + month(sample_date)^year(sample_date)
)

iplot(upstream_mod[lhs = 'gly', rhs = 1, sample = "Full"])
iplot(upstream_mod[lhs = 'ampa', rhs = 1, sample = "Full"])

iplot(upstream_mod[lhs = 'gly', rhs = 2])
iplot(upstream_mod[lhs = 'ampa', rhs = 2])

iplot(upstream_mod[lhs = 'gly', rhs = 3])
iplot(upstream_mod[lhs = 'ampa', rhs = 3])

upstream_pred_p = 
  map_dfr(
    c('gly','ampa'),
    \(analyte_in){
      upstream_mod[lhs = analyte_in, rhs = 2] |>
        broom::tidy() |>
        mutate(
          analyte = analyte_in != 'gly',
          trt = str_extract(term, '(all|e100m)_yield_diff_.*_\\d*_\\d{1,2}'),
          crop = str_extract(trt, '(?<=yield_diff_)\\w{3}(?=_)'),
          split = str_extract(trt, '(?<=yield_diff_\\w{3}_)\\d{2}(?=_)'),
          buffer = str_extract(trt, '\\d{1,2}$'),
          distance = str_extract(term, '(?<=d).{1,4}(?=:high)') |>
            str_replace('n','-') |>
            as.numeric(),
          distance = ifelse(distance == -Inf, -100, distance + 50) |> as.factor(),
          ci_l = estimate + std.error*qnorm(0.025),
          ci_h = estimate + std.error*qnorm(0.975)
        ) |>
        select(
          analyte, crop, split, buffer, distance, 
          estimate, ci_l, ci_h
        ) |> 
        filter(!is.na(crop)) |>
        data.table()
    }
  )|>
  ggplot(aes(x = distance, color = analyte)) + 
  geom_point(size = 2, aes(y = estimate), position = position_dodge(width = 0.5)) +
  geom_linerange(size = 1.2, aes(ymin = ci_l, ymax = ci_h), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = 'solid') +
  scale_color_viridis_d(name='Analyte', labels = c('Glyphosate','AMPA'), begin = 0.15, end = 0.85, direction = -1)+
  labs(
    x = 'Distance Bin (km)', y = TeX("Effect on Sample Concentration ($\\mu g/L$)")
  )
ggsave(
  plot = upstream_pred_p + theme_minimal(base_size = 16),
  filename = here("figures/descriptive/upstream_pred_p.jpeg"),
  width = 9, height = 5,
  bg = 'white'
)
ggsave(
  plot = upstream_pred_p,
  filename = here("figures/descriptive/upstream_pred_p.pdf"),
  width = 9, height = 5,
  device = cairo_pdf
)



