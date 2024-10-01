
# Creating an instrument that is percentile of 1990-95 crop acreage 
library(pacman)
p_load(
  here, fst, data.table, collapse, stringr, ggplot2
)

# Loading crop data
crop_acre_dt = read.fst(
  here('data/raw/all-crop-acre-dt.fst'), 
  as.data.table = TRUE
)
crop_yield_dt = read.fst(
  here('data/raw/all-crop-yield-dt.fst'), 
  as.data.table = TRUE
)
# Loading county sizes 
cnty_area_dt = read.fst(
  here('data/download-script/cnty-area-dt.fst'),
  as.data.table = TRUE
)[census_year == 201,.(GEOID, area_km2)]

# Taking 90-95 average by county 
pre_gm_crop_dt = 
  join(
    crop_acre_dt, 
    crop_yield_dt,
    on = c('year','GEOID')
  )[year %in% 1990:1995,-'year'] |>
  gby(GEOID) |>
  fmean() |>
  join(
    cnty_area_dt, 
    on = 'GEOID',
    how = 'left'
  )
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
# Calculating GM avg and max percentiles 
crop_instr_dt[,':='(
  percentile_gm_yield_avg = (percentile_corn_yield + percentile_soy_yield + percentile_cotton_yield)/3,
  percentile_gm_yield_max = max(percentile_corn_yield, percentile_soy_yield, percentile_cotton_yield)), 
  by = 1:nrow(crop_instr_dt)
]
crop_instr_dt[,':='(
  percentile_gm_yield_avg = frank(percentile_gm_yield_avg)/.N, 
  percentile_gm_yield_max = frank(percentile_gm_yield_max)/.N 
)]
# Cleaning up column names 
setnames(
  crop_instr_dt, 
  old = colnames(crop_instr_dt),
  new = str_remove(colnames(crop_instr_dt),'value_')
)
# Doing it again for eastern US 
trt_dt = read_fst(
  here('data/clean/trt-dt.fst'), 
  as.data.table = TRUE
)[,.(GEOID, e100m)]
# Turning into long table and taking percentiles 
crop_instr_dt_e100m = 
    melt(
      merge(pre_gm_crop_dt, trt_dt, by = 'GEOID')[e100m == TRUE,-'e100m'], 
      id.vars = 'GEOID'
    )[, percentile := frank(value)/.N,
      by = variable
    ] |>
    dcast(
      GEOID ~ variable,
      value.var = c('value','percentile')
    )
# Calculating GM avg and max percentiles 
crop_instr_dt_e100m[,':='(
  percentile_gm_yield_avg = (percentile_corn_yield + percentile_soy_yield + percentile_cotton_yield)/3,
  percentile_gm_yield_max = max(percentile_corn_yield, percentile_soy_yield, percentile_cotton_yield)), 
  by = 1:nrow(crop_instr_dt_e100m)
]
crop_instr_dt_e100m[,':='(
  percentile_gm_yield_avg = frank(percentile_gm_yield_avg)/.N, 
  percentile_gm_yield_max = frank(percentile_gm_yield_max)/.N 
)]
# Cleaning up column names 
setnames(
  crop_instr_dt_e100m, 
  old = colnames(crop_instr_dt_e100m),
  new = str_remove(colnames(crop_instr_dt_e100m),'value_')
)
# Append e100m to variable name 
setnames(
  crop_instr_dt_e100m, 
  old = str_subset(colnames(crop_instr_dt_e100m),'percentile'),
  new = paste0(str_subset(colnames(crop_instr_dt_e100m),'percentile'),"_e100m")
)
# Saving the results 
crop_instr_dt =
  merge(
    crop_instr_dt, 
    crop_instr_dt_e100m |> get_vars('GEOID|e100m', regex = TRUE), 
    by = 'GEOID', 
    all.x = TRUE
  )
write.fst(
  x = crop_instr_dt, 
  path = here('data/clean/crop-acre-percentile-90-95.fst')
)



# Testing out the results -----------------------------------------------------


# ggplot(crop_instr_dt, aes(x = percentile_gm_yield_max)) + geom_density()

# ggplot(pre_gm_crop_dt, aes(x = soy_acres_pct_cnty)) + 
# geom_histogram(bins = 50)
# ggplot(pre_gm_crop_dt, aes(x = corn_yield_per_acre)) + 
# geom_histogram(bins = 50)
# ggplot(pre_gm_crop_dt, aes(x = cotton_yield_per_acre)) + 
# geom_histogram(bins = 50)

# ggplot(pre_gm_crop_dt, aes(x = soy_acres_pct_cnty, y = soy_yield_per_acre)) + 
# geom_point() + 
# scale_x_log10() + 
# scale_y_log10()
# ggplot(pre_gm_crop_dt, aes(x = corn_acres_pct_cnty, y = corn_yield_per_acre)) + 
# geom_point() + 
# scale_x_log10() + 
# scale_y_log10()
# ggplot(pre_gm_crop_dt, aes(x = cotton_acres_pct_cnty, y = cotton_yield_per_acre)) + 
# geom_point() + 
# scale_x_log10() + 
# scale_y_log10()





# # Is this correlated with GAEZ data 
# trt_dt = read.fst( 
#   path = here("data/clean/trt-dt.fst"),
#   as.data.table = TRUE
# )
# tmp =
#   merge(
#     crop_instr_dt, 
#     trt_dt, 
#     by = 'GEOID',
#     all =TRUE
#   )
# ggplot(tmp, aes(y = percentile_gm_acres_pct_cnty, x =  all_yield_diff_percentile_gmo)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# ggplot(tmp, aes(y = percentile_gm_acres_pct_cnty, x =  percentile_gm_acres)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()


# cor(tmp$percentile_gm_acres_pct_cnty, tmp$percentile_gm_acres)

# ggplot(tmp, aes(y = percentile_soy_acres_pct_cnty, x =  all_yield_diff_percentile_soy)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp$percentile_soy_acres_pct_cnty, tmp$all_yield_diff_percentile_soy)

# ggplot(tmp, aes(y = percentile_corn_acres_pct_cnty, x =  all_yield_diff_percentile_mze)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp$percentile_corn_acres_pct_cnty, tmp$all_yield_diff_percentile_mze)

# ggplot(tmp, aes(y = percentile_cotton_acres_pct_cnty, x =  all_yield_diff_percentile_cot)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp$percentile_cotton_acres_pct_cnty, tmp$all_yield_diff_percentile_cot)


# ggplot(tmp, aes(y = percentile_soy_yield_per_acre, x =  all_yield_diff_percentile_soy)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()

# cor(tmp$percentile_soy_acres, tmp$all_yield_diff_percentile_soy)
# cor(tmp$percentile_soy_acres_pct_cnty, tmp$all_yield_diff_percentile_soy)
# cor(tmp$percentile_soy_yield, tmp$all_yield_diff_percentile_soy)
# cor(tmp$percentile_soy_acres_pct_cnty, tmp$percentile_soy_yield)

# cor(tmp$percentile_corn_acres, tmp$all_yield_diff_percentile_mze)
# cor(tmp$percentile_corn_acres_pct_cnty, tmp$all_yield_diff_percentile_mze)
# cor(tmp$percentile_corn_yield, tmp$all_yield_diff_percentile_mze)
# cor(tmp$percentile_corn_acres_pct_cnty, tmp$percentile_corn_yield)

# cor(tmp$percentile_cotton_acres, tmp$all_yield_diff_percentile_cot)
# cor(tmp$percentile_cotton_acres_pct_cnty, tmp$all_yield_diff_percentile_cot)
# cor(tmp$percentile_cotton_yield, tmp$all_yield_diff_percentile_cot)
# cor(tmp$percentile_cotton_acres_pct_cnty, tmp$percentile_cotton_yield)

# cor(tmp$percentile_gm_acres, tmp$all_yield_diff_percentile_gmo)
# cor(tmp$percentile_gm_acres_pct_cnty, tmp$all_yield_diff_percentile_gmo)
# cor(tmp$percentile_gm_yield_avg, tmp$all_yield_diff_percentile_gmo)
# cor(tmp$percentile_gm_yield_max, tmp$all_yield_diff_percentile_gmo)
# cor(tmp$percentile_gm_yield_max, tmp$all_yield_diff_percentile_gmo_max)
# cor(tmp$percentile_gm_yield_max, tmp$percentile_gm_acres_pct_cnty)

# ggplot(tmp, aes(y = percentile_soy_yield, x =  percentile_soy_acres_pct_cnty)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# ggplot(tmp, aes(y = percentile_corn_yield, x =  percentile_corn_acres_pct_cnty)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# ggplot(tmp, aes(y = percentile_cotton_yield, x =  percentile_cotton_acres_pct_cnty)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()

# ggplot(crop_instr_dt, aes(y = cotton_yield_per_acre, x =  cotton_acres_pct_cnty)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()

# ggplot(tmp, aes(y = percentile_corn_acres_pct_cnty, x =  all_yield_diff_percentile_mze)) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     theme_minimal()
# cor(tmp$percentile_corn_acres_pct_cnty, tmp$all_yield_diff_percentile_mze)

# ggplot(tmp, aes(y = percentile_cotton_acres_pct_cnty, x =  all_yield_diff_percentile_cot)) + 
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