# Making event study figures from county data
library(pacman)
p_load(
  here, data.table, fst, ggplot2, fixest, qs, 
  stringr, janitor, purrr
)

# Loading the models
alt_event_mods = qread(here('data/results/county-level/rural/cnty-alt/event-mods.qs'))
main_event_mods = qread(here('data/results/county-level/rural/cnty-main/event-mods.qs'))

# Getting everything into a nice table 
coef_dt = 
  rbind(
    data.table(coeftable(main_event_mods, cluster = ~GEOID + year)),
    data.table(coeftable(alt_event_mods, cluster = ~GEOID + year)),
    use.names = TRUE,
    fill = TRUE
  ) |>
  clean_names()
# Doing some cleaning
coef_dt[,':='(
  year = str_extract(coefficient, '(?<=year::)\\d{4}')|> as.integer(),
  lhs_n = str_remove(lhs, '_km2') |> 
    str_to_title() |>
    str_replace('Glyph','Glyphosate'),
  rhs_n = fifelse(
    is.na(rhs), 
    'all_yield_diff_percentile_gmo',
    str_extract(rhs, '(?<=year, ).*(?=, ref = 1995)')
  )
)]
# Adding 1995 as reference year 
coef_dt = 
  rbind(
    coef_dt[year != 1995],
    unique(coef_dt[,.(
      id, 
      lhs, lhs_n, 
      rhs, rhs_n,
      coefficient = 'ref', 
      estimate = 0, std_error = 0, 
      t_value = NA, pr_t = NA, 
      year = 1995
    )])
  )
# Finding mean and std deviation of each variable 
std_dt = 
  read_fst(
    path = here('data/clean/comb-cnty-health-dt.fst'),
    as.data.table = TRUE
  )[# Restrict years
    between(year, 1990, 2013)
  ][,':='(
    alachlor_km2 = alachlor/area_km2,
    cyanazine_km2 = cyanazine/area_km2,
    fluazifop_km2 = fluazifop/area_km2,
    metolachlor_km2 = metolachlor/area_km2,
    metribuzin_km2 = metribuzin/area_km2,
    nicosulfuron_km2 = nicosulfuron/area_km2,
    herbicide_km2 = herbicide/area_km2,
    insecticide_km2  = insecticide/area_km2
  )][,
    lapply(.SD, function(x) c(mean = mean(x,na.rm = TRUE), sd = sd(x,na.rm = TRUE))),
    .SDcols = unique(coef_dt$lhs)
  ] |>
  t() |>
  data.table(keep.rownames = TRUE) |>
  setnames(c('lhs','mean','sd'))
# Merging with coef table 
coef_dt = 
  merge(
    coef_dt[,-c('estimate_std','std_error_std','sd','mean')], 
    std_dt,
    by = 'lhs'
  )[,':='(
    estimate_std = (estimate)/sd,
    std_error_std = std_error/sd
  )]

# Plotting first stage for many pesticides 
pest_fs_p = 
  ggplot(
    data = coef_dt[str_detect(lhs, 'km2') & year  >= 1992],# == outcome_in],
    aes(x = year, y = estimate_std)
  ) +
  geom_point() + 
  geom_line() + 
  geom_ribbon(
    aes(
      ymin = estimate_std + qnorm(0.025)*std_error_std,
      ymax = estimate_std + qnorm(0.975)*std_error_std
    ),
    alpha = 0.3, 
    fill = 'black',
    color = NA
  ) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 1995, linetype = 'dashed') + 
  theme_minimal(base_size = 16) + 
  labs(
    x = 'Year', 
    y = 'Estimate and 95% CI', 
    caption= 'Standard errors clustered by county and year.'
  ) + 
  facet_wrap(~lhs_n)
pest_fs_p
ggsave(
  plot = pest_fs_p, 
  filename = here('figures/county-level/fs-event-pesticides.jpeg'),
  width = 8, height = 6, 
  bg = 'white'
)    


# Making other health outcome plots 
plot_county_health_outcome = function(outcome_in, trt_in, coef_dt){
  out_p = 
    ggplot(
      data = coef_dt[lhs == outcome_in & rhs_n == trt_in],
      aes(x = year, y = estimate_std)
    ) +
    geom_point() + 
    geom_line() + 
    geom_ribbon(
      aes(
        ymin = estimate_std + qnorm(0.025)*std_error_std,
        ymax = estimate_std + qnorm(0.975)*std_error_std
      ),
      alpha = 0.3, 
      fill = 'black',
      color = NA
    ) + 
    geom_hline(yintercept = 0) + 
    geom_vline(xintercept = 1995, linetype = 'dashed') + 
    theme_minimal(base_size = 16) + 
    labs(
      x = 'Year', 
      y = 'Estimate and 95% CI', 
      caption= 'Standard errors clustered by county and year.'
    )
  out_p
  ggsave(
    plot = out_p, 
    filename = here(
      'figures/county-level',
      paste0(
        str_replace_all(paste0(outcome_in,'-',trt_in), '_','-'),
        '.jpeg'
      )
    ),
    width = 7, height = 4, 
    bg = 'white'
  )    
}

# Making the plots for each outcome 
pmap(
  coef_dt[
    str_detect(lhs, 'km2',negate = TRUE),
    .(outcome_in = lhs, trt_in = rhs_n)
  ] |> unique(),
  plot_county_health_outcome,
  coef_dt = coef_dt
)
