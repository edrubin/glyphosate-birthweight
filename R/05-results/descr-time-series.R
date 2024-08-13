# Time series plots of birthweight and glyphosate
library(pacman)
p_load(
  fst, here, data.table, janitor, readxl, magrittr,
  tigris, sf, ggplot2, dplyr, purrr,
  viridis, scales, RColorBrewer
)
options(tigris_use_cache = TRUE)
theme_set(theme_minimal(base_size = 16))

# Reading the data 
comb_dt = read.fst(
  here("data/clean/comb-cnty-dt.fst"),
  as.data.table = TRUE
)

# Creating grouping variable based on treatment defintion
#comb_dt = 
  #merge(
  #  comb_dt[,-"quintile"],
  #  yield_quintile_dt,
  #  by = "GEOID",
  #  all.x = TRUE
  #) %>%
  #comb_dt[,':='(
  #  #trt = fcase(
  #  #  rural == TRUE & e100m == TRUE & quintile == 5, "High GMO Yield",
  #  #  rural == TRUE & e100m == TRUE & quintile == 1, "Low GMO Yield",
  #  #  rural == TRUE & e100m == TRUE, "Middle GMO Yield",
  #  #  rural == FALSE & e100m == TRUE, "Urban",
  #  #  e100m == FALSE, "West 100m"
  #  #)
  #  trt = fcase(
  #    rural == TRUE & e100m_yield_diff_gmo_50_0 == TRUE, "CSC",
  #    rural == TRUE & e100m_yield_diff_gmo_50_0 == FALSE, "Non-CSC",
  #    rural == FALSE & e100m == TRUE, "Urban",
  #    e100m ==  FALSE, "West"
  #  )
  #)]


# Plotting the glyphosate time series
glyph_ts_p =
  comb_dt[year %in% 1992:2012,.(
    avg_glyph_km2 = sum(glyph_km2, na.rm = TRUE)), 
    by = .(year, trt = all_yield_diff_gmo_50_0)
  ][!is.na(trt)] |>
  ggplot(aes(
    x = year, 
    y = avg_glyph_km2, 
    color = trt,
    group = interaction(year <= 1995, trt)
  )) + 
  geom_line(size = 1.2) +
  geom_point(size = 3) + 
  geom_vline(xintercept = 1995.5, col = 'black', size = 1, linetype ='dashed') +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d(
    option = 'magma', 
    end = 0.9, begin = 0.2,
    name = "GM crop suitability", 
    labels = c('Low','High')
  ) + 
  scale_y_continuous('GLY (kg/km²)') +
  scale_x_continuous('', breaks = seq(1990, 2015, 5))
glyph_ts_p
ggsave(
  plot = glyph_ts_p + 
    theme_minimal(base_size = 24) + 
    theme(
      legend.text=element_text(size=20), 
      legend.position = 'bottom',
      legend.margin=margin(t=-25), 
      panel.grid.minor = element_blank()
    ),
  filename = here("figures/descriptive/ts-glyph.jpeg"),
  width = 8*0.8, height = 6*0.8,
  bg = 'white'
)  
# Time series without groups 
glyph_ts_tot_p =
  comb_dt[year %in% 1992:2017,.(
    avg_glyph_km2 = mean(glyph_km2, na.rm = TRUE)), 
    by = .(year)
  ] |>
  ggplot(aes(x = year, y = avg_glyph_km2)) + 
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Glyphosate (kg per sq km)"
  ) +
  ylim(0,0.025) +
  geom_vline(xintercept = 1996, linetype = 'dashed') +
  geom_label(
    aes(x = 1996.05, y = 0.025, label = 'Release of GM crops'), 
    hjust = 'left', 
    label.size = NA,
    size = 5
  ) + 
  scale_x_continuous(labels = seq(1995,2015,5), breaks = seq(1995,2015,5))
ggsave(
  plot = glyph_ts_tot_p + 
    theme_minimal(base_size = 20),
  filename = here("figures/descriptive/ts-glyph-tot.jpeg"),
  width = 6, height = 5,
  bg = 'white'
)  

# Now doing time series for mean birth weight
small_comb_dt = comb_dt[,.(GEOID,year,trt = all_yield_diff_gmo_50_0)]
setkey(small_comb_dt, year, GEOID)

# Processing the health data
bw_dt = map_dfr(
  1990:2013,
  \(yr){
     natality_yr_dt = read.fst(
        path = here(paste0("data/health-restricted/period-clean/natality-",yr,".fst")),
        as.data.table = TRUE
      ) |>
       merge(
         small_comb_dt[.(yr)],
         by = c("GEOID","year")
       ) %>% 
       .[,.(
          mean_bw = mean(dbwt, na.rm = TRUE),
          median_bw = median(dbwt, na.rm = TRUE),
          bw_10 = quantile(dbwt, probs = 0.10, na.rm = TRUE),
          bw_20 = quantile(dbwt, probs = 0.20, na.rm = TRUE),
          bw_30 = quantile(dbwt, probs = 0.30, na.rm = TRUE),
          bw_40 = quantile(dbwt, probs = 0.40, na.rm = TRUE),
          bw_50 = quantile(dbwt, probs = 0.50, na.rm = TRUE),
          bw_60 = quantile(dbwt, probs = 0.60, na.rm = TRUE),
          bw_70 = quantile(dbwt, probs = 0.70, na.rm = TRUE),
          bw_80 = quantile(dbwt, probs = 0.80, na.rm = TRUE),
          bw_90 = quantile(dbwt, probs = 0.90, na.rm = TRUE)
          ), 
          by = .(year, trt)
        ]
  })


# BW time series 
bw_ts_p = 
  bw_dt[year %in% 1992:2017 & !is.na(trt)] |>
  ggplot(aes(
    x = year, 
    y = mean_bw, 
    color = trt,
    group = interaction(year <= 1995, trt)
  )) + 
  geom_line(size = 1.2) +
  geom_point(size = 3) + 
  geom_vline(xintercept = 1995.5, col = 'black', size = 1, linetype ='dashed') +
  #geom_hline(yintercept = 0) +
  scale_color_brewer(
    name = "Attainable Yield", 
    palette = 'Dark2',
    labels = c('Low','High')
  ) + 
  scale_y_continuous('Mean Birth Weight (g)') +
  scale_x_continuous('', breaks = seq(1990, 2015, 5))
bw_ts_p
ggsave(
  plot = bw_ts_p + 
    theme_minimal(base_size = 24) + 
    theme(
      legend.text=element_text(size=20), 
      legend.position = 'bottom',
      legend.margin=margin(t=-25)
    ),
  filename = here("figures/descriptive/ts-mean-bw.jpeg"),
  width = 8, height = 6,
  bg = 'white'
)  