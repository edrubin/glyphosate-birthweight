# Generating predictions for glyph/AMPA concentrations 
library(pacman)
p_load(
  here, fst, data.table, tidymodels, ranger, qs, stringr, 
  lubridate, collapse
)

# Vector of months we will use
sample_watershed_dt = 
  read.fst(
    path = here('data-clean/ml-water/sample-watershed-dt.fst'),
    as.data.table = TRUE
  )
# Loading the models 
glyph_lasso_fit = qread(here('data-clean/ml-water/final-fit-lasso-glyph.qs'))
ampa_lasso_fit = qread(here('data-clean/ml-water/final-fit-lasso-ampa.qs'))
glyph_rf_fit = qread(here('data-clean/ml-water/final-fit-rf-glyph.qs'))
ampa_rf_fit = qread(here('data-clean/ml-water/final-fit-rf-ampa.qs'))
# Month-year panel 
pesticide_upstream_dt =
  CJ(
    month = unique(sample_watershed_dt$month), 
    year = 1992:2017
  ) |>
  merge(
    read.fst(
      path = here('data-clean/ml-water/pesticide-upstream-dt.fst'),
      as.data.table = TRUE
    ),
    by = 'year',
    allow.cartesian = TRUE
  )
# Adding columns so predict function works 
pesticide_upstream_dt[,':='(
    sample_month = ymd(paste(year, month, '01')),
    sample_date = ymd(paste(year, month, '01')),
    gly_result = NA,
    ampa_result = NA,
    # Filling in some missing values, 
    kffact_d50 = ifelse(
      is.na(kffact_d50), 0, kffact_d50
    ),
    ppt_off_season_d0 = ifelse(
      is.na(ppt_off_season_d0), 0, ppt_off_season_d0
    ), 
    ppt_off_season_d50 = ifelse(
      is.na(ppt_off_season_d50), 0, ppt_off_season_d50
    ), 
    ppt_off_season_d100 = ifelse(
      is.na(ppt_off_season_d100), 0, ppt_off_season_d100
    ), 
    ppt_growing_season_d0 = ifelse(
      is.na(ppt_growing_season_d0), 0, ppt_growing_season_d0
    ), 
    ppt_growing_season_d50 = ifelse(
      is.na(ppt_growing_season_d50), 0, ppt_growing_season_d50
    ), 
    ppt_growing_season_d100 = ifelse(
      is.na(ppt_growing_season_d100), 0, ppt_growing_season_d100
    )
)]
# Making predictions 
pesticide_upstream_dt[,':='(
  pred_ampa_in_water_lasso = 
    predict(
      ampa_lasso_fit,
      new_data = pesticide_upstream_dt
    )$.pred,
  pred_glyph_in_water_lasso = 
    predict(
      glyph_lasso_fit,
      new_data = pesticide_upstream_dt
    )$.pred,
  pred_ampa_in_water_rf = 
    predict(
      ampa_rf_fit,
      new_data = pesticide_upstream_dt
    )$.pred,
  pred_glyph_in_water_rf = 
    predict(
      glyph_rf_fit,
      new_data = pesticide_upstream_dt
    )$.pred
)]

pesticide_upstream_dt[,
  month := match(month, month.abb) |>
    str_pad(width = 2, side = 'left',pad = '0')
]

# Saving the results 
write.fst(
  pesticide_upstream_dt[,.(
    hybas_id, 
    year,
    month, 
    pred_glyph_in_water_lasso, 
    pred_ampa_in_water_lasso, 
    pred_glyph_in_water_rf, 
    pred_ampa_in_water_rf
  )],
  path = here('data-clean/ml-water/predictions-watershed.fst')
)
