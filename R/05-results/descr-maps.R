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
      geom_sf(data = states_sf, color = "black", fill = NA) +
      scale_color_viridis_c(
        name = "", 
        aesthetics = c('fill','color'),
        labels = scales::label_percent(), 
        breaks = seq(0,1, by = 0.25)
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
    # Printing if specified
    if(plot_out == TRUE) print(percentile_cnty_yr_p)
  }
  # Making the maps 
  map(
    str_subset(colnames(comb_cnty_dt), 'percentile'),
    plot_yield_diff_percentile
  )

# Plotting change in glyphosate -----------------------------------------------
  change_glyph_p = 
    left_join(
      county_sf, 
      comb_cnty_dt[
        year %in% c(1995, 2012), 
        .(GEOID, year = as.factor(year), glyph_km2)
      ] |>
        gby(GEOID) |>
        fdiff(t = year) %>% 
        .[!is.na(glyph_km2)],
      by = 'GEOID'
    )|>
    ggplot() + 
    geom_sf(aes(fill = glyph_km2, color = glyph_km2)) + 
    geom_sf(data = states_sf, color = "black", fill = NA) +
    scale_color_viridis_c(
      name = "", 
      aesthetics = c('fill','color')
    ) 
  ggsave(
    change_glyph_p,
    filename = here("figures/descriptive/glyph-km2-diff-9512.jpeg"),
    width = 8, height = 8/1.4,
    bg = 'white'
  )
