# Descriptive Maps 
library(pacman)
p_load(
  here, fst, data.table, sf, tigris, janitor, dplyr, purrr,
  ggplot2, tidyr, scales, stringr, collapse
)
options(tigris_use_cache = TRUE)
# Setting the theme 
  theme_set(
    theme_void(base_size = 14) +
      theme(
        legend.position = 'bottom',
        plot.margin = margin(0,0,0,0),
        legend.key.width = unit(8/6, 'cm'),
        legend.text = element_text(size = 18)
      )
  )

# Loading data ----------------------------------------------------------------
  # County and state shapes
  states_sf = 
    states(
      year = 2010,
      cb = TRUE  
    ) |>
    filter( # Limiting to continental US
      !(STATE %in% c("02","15","60","66","69","72","78"))
    ) |>
    clean_names()
  county_sf =
    map_dfr(
      states_sf$state,
      counties,
      year = 2010,
      cb = TRUE
    ) |> 
    clean_names() |>
    st_simplify(dTolerance = 0.001) |>
    mutate(GEOID = paste0(statefp,countyfp)) |>
    st_transform(crs = 2163) 
  # County level data
  comb_cnty_dt = read.fst(
    here("data/clean/comb-cnty-dt.fst"),
    as.data.table = TRUE
  )

# Plotting yield diff percentiles ---------------------------------------------
  # Function to plot the yield diff percentiles for each crop
  plot_yield_diff_percentile = function(var_in, plot_out = FALSE){
    # Creating the plot 
    percentile_cnty_yr_p = 
      left_join(
        county_sf, 
        melt(
          comb_cnty_dt[year == 1995],
          id.vars = 'GEOID',
          measure = patterns('percentile')
        )[variable == var_in],
        by = 'GEOID'
      ) |>
      ggplot() + 
      geom_sf(aes(fill = value, color = value)) + 
      geom_sf(
        data = states_sf, 
        color = "grey80", 
        fill = NA, 
        linewidth = 0.05
      ) +
      scale_color_viridis_c(
        name = "", 
        option = 'magma',
        direction = 1,
        aesthetics = c('fill','color'),
        labels = scales::label_percent(), 
        breaks = seq(0,1, by = 0.25),
        na.value = 'grey70'
      ) 
    # Saving the plot
    ggsave(
      percentile_cnty_yr_p,
      filename = here(
        "figures/descriptive/yield-diff-percentile",
        paste0(str_replace(var_in, "_yield_diff_percentile_",'-'),".jpeg")
      ),
      width = 8, height = 8/1.4,
      bg = 'white'
    )
    ggsave(
      percentile_cnty_yr_p,
      filename = here(
        "figures/descriptive/yield-diff-percentile",
        paste0(str_replace(var_in, "_yield_diff_percentile_",'-'),".eps")
      ),
      device = cairo_ps,
      width = 8, height = 8/1.4,
      bg = 'white'
    )
    # Printing if specified
    if(plot_out == TRUE) print(percentile_cnty_yr_p)
    # Also making one with rural counties only 
    # Creating the plot 
    percentile_cnty_yr_rural_p = 
      left_join(
        county_sf, 
        melt(
          comb_cnty_dt[year == 1995],
          id.vars = c('GEOID','rural'),
          measure = patterns('percentile')
        )[variable == var_in],
        by = 'GEOID'
      ) |>
      ggplot() + 
      geom_sf(aes(
        fill = fifelse(rural == TRUE, value, NA), 
        color = fifelse(rural == TRUE, value, NA)
      )) + 
      geom_sf(
        data = states_sf, 
        color = "grey80", 
        fill = NA, 
        linewidth = 0.05
      ) +
      scale_color_viridis_c(
        name = "", 
        option = 'magma',
        direction = 1,
        aesthetics = c('fill','color'),
        labels = scales::label_percent(), 
        breaks = seq(0,1, by = 0.25), 
        na.value = 'grey70'
      ) 
    # Saving the plot
    ggsave(
      percentile_cnty_yr_rural_p,
      filename = here(
        "figures/descriptive/yield-diff-percentile",
        paste0(str_replace(var_in, "_yield_diff_percentile_",'-'),"-rural.jpeg")
      ),
      width = 8, height = 8/1.4,
      bg = 'white'
    )
    ggsave(
      percentile_cnty_yr_rural_p,
      filename = here(
        "figures/descriptive/yield-diff-percentile",
        paste0(str_replace(var_in, "_yield_diff_percentile_",'-'),"-rural.eps")
      ),
      device = cairo_ps,
      width = 8, height = 8/1.4,
      bg = 'white'
    )
  }
  # Making the maps 
  map(
    str_subset(colnames(comb_cnty_dt), 'percentile'),
    plot_yield_diff_percentile
  )

# Plotting change in glyphosate -----------------------------------------------
gly_change_dt = 
  comb_cnty_dt[
    year %in% c(1995, 2012), 
    .(GEOID, rural, year = as.factor(year), glyph_km2)
  ] |>
  gby(GEOID, rural) |>
  fdiff(t = year) %>% 
  .[!is.na(glyph_km2)]
# Censoring 
gly_change_dt[,
  glyph_km2 := fcase(
    glyph_km2 < fnth(gly_change_dt$glyph_km2, 0.01), fnth(gly_change_dt$glyph_km2, 0.01),
    glyph_km2 > fnth(gly_change_dt$glyph_km2, 0.99), fnth(gly_change_dt$glyph_km2, 0.99),
    glyph_km2 >= fnth(gly_change_dt$glyph_km2, 0.01) & 
    glyph_km2 <= fnth(gly_change_dt$glyph_km2, 0.99), glyph_km2
  )
]
# Make the map
change_glyph_p = 
  left_join(
    county_sf, 
    gly_change_dt,
    by = 'GEOID'
  )|>
  ggplot() + 
  geom_sf(aes(fill = glyph_km2, color = glyph_km2)) + 
  geom_sf(
    data = states_sf, 
    color = "grey80", 
    fill = NA, 
    linewidth = 0.05
  ) +
  scale_color_viridis_c(
    name = "", 
    option = 'magma',
    direction = 1,
    aesthetics = c('fill','color')
  ) 
ggsave(
  change_glyph_p,
  filename = here("figures/descriptive/glyph-km2-diff-9512.jpeg"),
  width = 8, height = 8/1.4,
  bg = 'white'
)

# Make the map
change_glyph_rural_p = 
  left_join(
    county_sf, 
    gly_change_dt,
    by = 'GEOID'
  )|>
  ggplot() + 
  geom_sf(aes(
    fill = fifelse(rural == TRUE, glyph_km2, NA), 
    color = fifelse(rural == TRUE, glyph_km2, NA)
  )) + 
  geom_sf(
    data = states_sf, 
    color = "grey80", 
    fill = NA, 
    linewidth = 0.05
  ) +
  scale_color_viridis_c(
    name = "", 
    option = 'magma',
    direction = 1,
    aesthetics = c('fill','color'),
    na.value = 'grey70'
  ) 
ggsave(
  change_glyph_rural_p,
  filename = here("figures/descriptive/glyph-km2-diff-9512-rural.jpeg"),
  width = 8, height = 8/1.4,
  bg = 'white'
)
ggsave(
  change_glyph_rural_p,
  filename = here("figures/descriptive/glyph-km2-diff-9512-rural.eps"),
  device = cairo_ps,
  width = 8, height = 8/1.4,
  bg = 'white'
)

# Map of rural counties 
rural_p = 
  left_join(
    county_sf, 
    gly_change_dt,
    by = 'GEOID'
  )|>
  ggplot() + 
  geom_sf(aes(fill = rural, color = rural)) + 
  geom_sf(
    data = states_sf, 
    color = "black", 
    fill = NA, 
    linewidth = 0.5
  ) +
  scale_color_manual(
    labels = c('Non-rural', 'Rural'),
    values = c('gray90', 'grey60'),
    aesthetics = c('fill','color'),
    name = ""
  ) 
ggsave(
  rural_p,
  filename = here("figures/descriptive/rural.jpeg"),
  width = 8, height = 8/1.4,
  bg = 'white'
)
ggsave(
  change_glyph_rural_p,
  filename = here("figures/descriptive/glyph-km2-diff-9512-rural.eps"),
  device = cairo_ps,
  width = 8, height = 8/1.4,
  bg = 'white'
)


crop_instr_dt = read.fst(
  path = here('data/clean/crop-acre-percentile-90-95.fst'), 
  as.data.table = TRUE
)

merge(
  crop_instr_dt, 
  comb_cnty_dt[,.(GEOID, rural)] |> unique(), 
  by = 'GEOID'
)[,
  .(gm_acres = sum(gm_acres)), 
  keyby = rural
]
