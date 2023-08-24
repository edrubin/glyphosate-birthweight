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
glyph_lasso_fit = qread(here('data-clean/ml-water/glyph-lasso-final.qs'))
ampa_lasso_fit = qread(here('data-clean/ml-water/ampa-lasso-final.qs'))
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
    ampa_result = NA
)]
# Making predictions 
pesticide_upstream_dt[,':='(
    pred_ampa_in_water = 
        predict(
            ampa_lasso_fit,
            new_data = pesticide_upstream_dt
        )$.pred,
    pred_glyph_in_water = 
        predict(
            glyph_lasso_fit,
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
    pred_glyph_in_water, 
    pred_ampa_in_water
  )],
  path = here('data-clean/ml-water/predictions-watershed.fst')
)