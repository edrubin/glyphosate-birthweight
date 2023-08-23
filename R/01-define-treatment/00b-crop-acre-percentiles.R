
# Creating an instrument that is percentile of 1990-95 crop acreage 
library(pacman)
p_load(
    here, fst, data.table, collapse, stringr, ggplot2
)

# Loading crop data
crop_acre_dt = read.fst(
    here('data/crops/all-crop-acre-dt.fst'), 
    as.data.table = TRUE
)
# Loading county sizes 
cnty_area_dt = read.fst(
    here('data/pop-area-empl/cnty-area-dt.fst'),
    as.data.table = TRUE
)[census_year == 201,.(GEOID, area_km2)]

# Taking 90-95 average by county 
pre_gm_crop_dt = 
    crop_acre_dt[
        year %in% 1990:1995,
        -c('year','state_fips','county_fips')
    ] |>
    gby(GEOID) |>
    fmean() |>
    merge(
        cnty_area_dt, 
        by = 'GEOID',
        all.y = TRUE
    )
# Setting NA values to zero
acre_cols = str_subset(colnames(pre_gm_crop_dt),'acres')
pre_gm_crop_dt[,
    (acre_cols) := lapply(.SD, \(x){ifelse(is.na(x), 0, x)}),
    .SDcols = acre_cols
]
# Creating GM aggregate 
pre_gm_crop_dt[,
    gm_acres := soy_acres + corn_acres + cotton_acres
]
# Calculating crop acres/total county size
acre_cols = str_subset(colnames(pre_gm_crop_dt),'acres')
pre_gm_crop_dt[,
    (paste0(acre_cols,'_pct_cnty')) :=
        lapply(.SD, \(x){x*0.00404686/area_km2}),
    .SDcols = acre_cols
]
pre_gm_crop_dt[,area_km2:= NULL]
# Turning into long table and taking percentiles 
crop_instr_dt = 
    melt(
        pre_gm_crop_dt, 
        id.vars = 'GEOID'
    )[, percentile := frank(value)/.N,
        by = variable
    ] |>
    dcast(
        GEOID ~ variable,
        value.var = c('value','percentile')
    )
# Cleaning up column names 
setnames(
    crop_instr_dt, 
    old = colnames(crop_instr_dt),
    new = str_remove(colnames(crop_instr_dt),'value_')
)
# Saving the results 
write.fst(
    x = crop_instr_dt, 
    path = here('data/crops/crop-acre-percentile-90-95.fst')
)


# Is this correlated with GAEZ data 
# trt_dt = read.fst( 
#     path = here("data-clean/trt-dt.fst"),
#     as.data.table = TRUE
# )
# tmp =
#     merge(
#         crop_instr_dt, 
#         trt_dt, 
#         by = 'GEOID',
#         all =TRUE
#     )
# ggplot(tmp, aes(x = percentile_gm_acres_pct_cnty, all_yield_diff_percentile_gmo)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp$percentile_gm_acres_pct_cnty, tmp$all_yield_diff_percentile_gmo)


# ggplot(tmp, aes(x = percentile_soy_acres_pct_cnty, all_yield_diff_percentile_soy)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp$percentile_soy_acres_pct_cnty, tmp$all_yield_diff_percentile_soy)

# ggplot(tmp, aes(x = percentile_corn_acres_pct_cnty, all_yield_diff_percentile_mze)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp$percentile_corn_acres_pct_cnty, tmp$all_yield_diff_percentile_mze)

# ggplot(tmp, aes(x = percentile_cotton_acres_pct_cnty, all_yield_diff_percentile_cot)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp$percentile_cotton_acres_pct_cnty, tmp$all_yield_diff_percentile_cot)

# tmp2 = 
#     merge(
#         crop_instr_dt, 
#         si_dt, 
#         by = 'GEOID'
#     )


# ggplot(tmp2[crop=='soy'], aes(x = percentile_soy_acres_pct_cnty, y = all_yield_high_percentile)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp2[crop=='soy']$percentile_soy_acres_pct_cnty, tmp2[crop=='soy']$all_yield_high_percentile)


# p_load(fixest)
# mods = 
# list(
#     feols(
#         data = tmp2[crop %in%c('soy')], 
#         fml = all_yield_diff_percentile ~ percentile_soy_acres_pct_cnty
#     ),
#     feols(
#         data = tmp2[crop %in%c('mze')], 
#         fml = all_yield_diff_percentile ~ percentile_corn_acres_pct_cnty
#     ),
#     feols(
#         data = tmp2[crop %in%c('cot')], 
#         fml = all_yield_diff_percentile ~ percentile_cotton_acres_pct_cnty
#     ),
#     feols(
#         data = tmp2[crop %in%c('gmo')], 
#         fml = all_yield_diff_percentile ~ percentile_gm_acres_pct_cnty
#     )
# )
# mods