# Combining rainfall, soil erodibility, and slope for each watershed
library(pacman)
p_load(
  here, fst, data.table, ggplot2, magrittr, collapse
)

# Precipitation data -- Grabbing growing vs non-growing, casting into wide format
rain_dt = 
  read.fst(
    here('data/watershed/ppt-season-wshd-dt.fst'), 
    as.data.table = TRUE
  ) %>% 
  .[,hybas_id := as.character(hybas_id)] |>
  dcast(
    hybas_id + year ~ growing_season,
    value.var = 'ppt',
  ) |>
  setnames(
    old = c('TRUE','FALSE'),
    new = c('ppt_growing_season', 'ppt_off_season')
  )
# Soil data
soil_dt = fread(here('data/watershed/soil-quality/watershed-soil-factors.csv'))[,.(
  hybas_id = as.character(HYBAS_ID), 
  kffact, slopelen = slopelenusle_r,
  kls = kffact*slopelenusle_r
)]

# Determining high erodibility (Target = top 15-20% of watersheds)
#ggplot(soil_dt, aes(x = kls)) + 
#  geom_histogram(binwidth = 1) + 
#  geom_vline(xintercept = kls_quant['80%'], linetype = 'dashed') + 
#  theme_minimal() + 
#  labs(x = 'Soil Erodibility x Slope', y = 'Count')

# Merging together
usle_dt = 
  merge(
    rain_dt, 
    soil_dt, 
    by = 'hybas_id'
  ) |> setkey(hybas_id, year)

# Saving the results 
write.fst(
  usle_dt, 
  here('data/watershed/usle-dt.fst')
)

