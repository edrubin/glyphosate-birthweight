


plot_yield_diff = function(crop_in){
  y_diff_p = 
    inner_join(
      county_sf,
      y_diff_dt[crop == crop_in,.(
        GEOID, 
        `Difference` = yield_diff/1e2, 
        `High Inputs` = yield_high/1e2, 
        `Low Inputs` = yield_low/1e2
      )]|>
        melt(id.vars = "GEOID"),
      by=c("geoid"="GEOID")
    ) |>
    filter(variable == "Difference") |>
    ggplot() + 
    geom_sf(aes(fill = value, color = value)) + 
    geom_sf(data = states_sf, fill = NA, color = "black") +
    scale_color_viridis_c(name = "Yield")+ 
    scale_fill_viridis_c(name = "Yield") +
    coord_sf(crs = 2163) +
    theme_minimal()
  ggsave(
    y_diff_p,
    filename = here(paste0(
      "figures/descriptive/crop-maps/attainable-yield/diff/",crop_in,"-yield-diff.jpeg"
    )),
    width = 10, height = 6,
    bg = 'white'
  )
}
  
map(
  unique(y_diff_dt$crop)[1],
  plot_yield_diff
)


# Plotting the instruments 
plot_yield_hilo = function(crop_in){
  y_instr_p = 
    inner_join(
      county_sf,
      y_diff_dt[crop == crop_in,.(
        GEOID, 
        `Difference` = yield_diff/1e2, 
        `High Inputs` = yield_high/1e2, 
        `Low Inputs` = yield_low/1e2
      )]|>
        melt(id.vars = "GEOID"),
      by=c("geoid"="GEOID")
    ) |>
    filter(variable %in% c("High Inputs","Low Inputs")) |>
    ggplot() + 
    geom_sf(aes(fill = value, color = value)) + 
    geom_sf(data = states_sf, fill = NA, color = "black") +
    scale_color_viridis_c(name = "Yield")+ 
    scale_fill_viridis_c(name = "Yield") +
    coord_sf(crs = 2163) +
    facet_wrap(~variable, nrow = 1, ncol = 2) +
    theme_minimal()
  ggsave(
    y_instr_p,
    filename = here(paste0(
      "figures/descriptive/crop-maps/attainable-yield/hilo/",crop_in,"-yield-hilo.jpeg"
    )),
    width = 14, height = 7,
    bg = 'white'
  )
}

map(
  unique(y_diff_dt$crop),
  plot_yield_hilo
)