
# Analyzes the diff-in-diff results for the county level outcomes 
# Packages
library(pacman)
p_load(
  fastverse, fixest, fst, qs, patchwork, magrittr, here, dplyr, 
  purrr, readr, collapse
)
fastverse_extend(topics = c('ST', 'DT', 'VI'))

# Load data -------------------------------------------------------------------

# Load the processed double-difference dataset
comb_cnty_health_dt = read_fst(
  path = here('data/clean/comb-cnty-health-dt.fst'),
  as.data.table = TRUE
)[,':='(
  alachlor_km2 = alachlor/area_km2,
  cyanazine_km2 = cyanazine/area_km2,
  fluazifop_km2 = fluazifop/area_km2,
  metolachlor_km2 = metolachlor/area_km2,
  metribuzin_km2 = metribuzin/area_km2,
  nicosulfuron_km2 = nicosulfuron/area_km2,
  herbicide_km2 = herbicide/area_km2,
  insecticide_km2  = insecticide/area_km2
)]
# Restrict years
comb_cnty_health_dt %<>% .[between(year, 1990, 2013)]

# Function to estimate Event Study and DiD ------------------------------------
est_twfe_cnty = function(outcomes_in, trt_in, est_dt, rural_in = TRUE, name){
  # Filtering to rural if specified 
  if(rural_in == TRUE){
    est_dt = est_dt[rural == TRUE]
    path = paste0('data/results/county-level/rural/',name)
  }else{
    path = paste0('data/results/county-level/all/',name)
  }
  # Creating directory to put results in
  if(!dir.exists(here(path))) dir.create(here(path), recursive = TRUE)
  # Estimating event study for all outcomes (including pesticides)
  event_mods = feols(
    data = est_dt,
    weight = ~tot_inf_births,
    fml = paste0(
      "c(", paste(outcomes_in, collapse = ","), ") ~ sw(",
        paste(paste0("i(year,",trt_in,", ref = 1995)"), collapse = ","),
      ") | year + GEOID"
    ) |> as.formula()
  )
  qsave(event_mods, here(path,'event-mods.qs'))
  rm(event_mods)
  gc()
  # Estimating DiD for all outcomes (including pesticides) 
  did_mods = feols(
    data = est_dt,
    weight = ~tot_inf_births,
    fml = paste0(
      "c(", paste(outcomes_in, collapse = ","),") ~ sw(",
      paste(paste0("i(year>1995,",trt_in,", ref = FALSE)"), collapse = ","),
      ") | year + GEOID"
    ) |> as.formula()
  )
  qsave(did_mods, here(path,'did-mods.qs'))
  rm(did_mods)
  gc()
  # Removing pesticide outcomes 
  tsls_outcomes = str_subset(outcomes_in, 'km2', negate = TRUE)
  if(length(tsls_outcomes) > 0){
    # Estimating TSLS: map because fixest cant do multiple estimation in IV's
    tsls_mods = 
      trt_in |>
      map(\(x){
        feols(
          data = est_dt,
          weight = ~tot_inf_births, 
          fml = paste0(
            "c(", paste(tsls_outcomes, collapse = ","), 
            ") ~ 1 | year + GEOID | glyph_km2 ~ ",
            paste0("i(year,",x,", ref = 1995)")
          ) |> as.formula()
        )
      })
    qsave(tsls_mods, here(path,'tsls-mods.qs'))
    rm(tsls_mods)
    gc()
  }
  return('yay')
}

# All of the outcomes (including first stage) we might care about 
# Main health outcomes to estimate across all instruments
health_outcomes = c(
  'median_birth_wt',
  'mean_birth_wt',
  'pct_low_bw',
  'median_gest',
  'sex_ratio',
  'inf_mort',
  'inf_mort_int',
  'inf_mort_ext'
)  
# Other health: just estimate with main instruments
alt_health_outcomes = c(
  'median_birth_wt_male','median_birth_wt_female',
  'median_birth_wt_q1','median_birth_wt_q2','median_birth_wt_q3','median_birth_wt_q4',
  'pct_preterm','pct_c_section',
  'inf_mort_male','inf_mort_female', 
  'inf_mort_q1','inf_mort_q2','inf_mort_q3','inf_mort_q4',
  'inf_mort_endocrine','inf_mort_respiratory','inf_mort_sids','inf_mort_low_weight'
)  
# Pesticides
pesticides = c(
  'glyph_km2',
  'alachlor_km2', 
  'cyanazine_km2', 
  'fluazifop_km2', 
  'metolachlor_km2', 
  'metribuzin_km2', 
  'nicosulfuron_km2', 
  'herbicide_km2', 
  'insecticide_km2'
)
alt_outcomes = c(  
  # Farm varibles
  'soy_acres', 
  'corn_acres', 
  'tot_acres', 
  'other_acres',
  'soy_yield', 
  'corn_yield', 
  'cotton_yield', 
  'other_yield',
  # Other economic vars
  'farm_income', 'nonfarm_income', 
  'farm_empl', 'tot_empl', 'unemployment_rate', 
  'pct_farm_empl', 'inc_per_cap_farm', 'inc_per_cap_nonfarm'
)
trt = c(
  #"e100m_yield_diff_percentile_bck",
  #"e100m_yield_diff_percentile_cab",
  #"e100m_yield_diff_percentile_olv",
  #"e100m_yield_diff_percentile_srg",
  #"e100m_yield_diff_percentile_wpo" 
  #'e100m_yield_diff_soy_50_10',
  #'e100m_yield_diff_soy_60_0',
  #'e100m_yield_diff_soy_40_0',
  #'e100m_yield_diff_gmo_50_10',
  #'e100m_yield_diff_gmo_40_0',
  #'e100m_yield_diff_gmo_60_0',
  #'all_yield_diff_gmo_50_0',
  "all_yield_diff_percentile_gmo",
  "all_yield_diff_percentile_gmo_max",
  "all_yield_diff_percentile_soy",
  "all_yield_diff_percentile_mze",
  "all_yield_diff_percentile_cot",
  "e100m_yield_diff_percentile_gmo",
  "e100m_yield_diff_percentile_gmo_max",
  "e100m_yield_diff_percentile_soy",
  "e100m_yield_diff_percentile_mze",
  "e100m_yield_diff_percentile_cot",
  'e100m_yield_diff_gmo_50_0',
  'e100m_yield_diff_soy_50_0',
  'all_yield_diff_gmo_50_0',
  'all_yield_diff_soy_50_0',
  "all_yield_diff_percentile_bck",
  "all_yield_diff_percentile_cab",
  "all_yield_diff_percentile_olv",
  "all_yield_diff_percentile_srg",
  "all_yield_diff_percentile_wpo"
)
trt_main = 'all_yield_diff_percentile_gmo'

# Running different combinations 
est_twfe_cnty(
  health_outcomes, 
  trt, 
  comb_cnty_health_dt,
  rural_in = TRUE, 
  name = 'cnty-main'
)
est_twfe_cnty(
  c(alt_health_outcomes, alt_outcomes, pesticides),  
  trt_main, 
  comb_cnty_health_dt,
  rural_in = TRUE, 
  name = 'cnty-alt'
)
est_twfe_cnty(
  health_outcomes, 
  trt, 
  comb_cnty_health_dt,
  rural_in = FALSE, 
  name = 'cnty-main'
)
est_twfe_cnty(
  c(alt_health_outcomes, alt_outcomes, pesticides),  
  trt_main, 
  comb_cnty_health_dt,
  rural_in = FALSE, 
  name = 'cnty-alt'
)




# Putting extra fertilizer code here for now ----------------------------------
# trt_dt = read.fst(here('data/clean/trt-dt.fst'), as.data.table = TRUE)
# comb_cnty_health_dt = 
#   read.fst(here('data/clean/comb-cnty-health-dt.fst'), as.data.table = TRUE)[
#     year %in% c(1992:2012),
#     .(tot_inf_births = fsum(tot_inf_births)),
#     keyby = .(GEOID, rural, area_km2)
#   ]
# est_dt = 
#   merge(
#     annual_fert_dt, 
#     trt_dt, 
#     by = 'GEOID'
#   ) |>
#   merge(
#     comb_cnty_health_dt, 
#     by = 'GEOID'
#   )
# est_dt[,state_fips := str_sub(GEOID,1,2)]
# # First a simple plot to show what is going on 
# ggplot(
#   melt(
#     est_dt, 
#     id.vars = c('GEOID','year','all_yield_diff_gmo_50_0','area_km2'), 
#     measure.vars = colnames(annual_fert_dt)[-(1:2)]
#   )[,
#     .(avg = fmean(value)), 
#     by = .(variable, year, all_yield_diff_gmo_50_0)
#   ], 
#   aes(x = year, y = avg, color = all_yield_diff_gmo_50_0)
# ) + 
# geom_vline(xintercept = 1995, linetype = 'dashed') +
# geom_line() +
# #geom_point() +
# scale_color_viridis_d(
#   name = 'Suitability', 
#   option = 'magma', 
#   labels = c('Low','High'), 
#   begin = 0.2, end = 0.8
# ) +
# scale_y_continuous(
#   name = 'Nutrient weight (kg e-6)', 
#   labels = scales::label_number(scale = 1e-6, big.mark = ',', )
# ) +
# scale_x_continuous(name = 'Year') + 
# theme_minimal() + 
# facet_wrap(
#   ~variable, 
#   scales = 'free'
# )



# fert_mod_logs = feols(
#   data = est_dt[year >= 1980], 
#   split = ~rural, 
#   weight = ~tot_inf_births,
#   fml = c(log(n_commercial/area_km2), log(p_commercial/area_km2), log(n_total/area_km2), log(p_total/area_km2), log(n_manure/area_km2), log(p_manure/area_km2)) ~ i(year, all_yield_diff_percentile_gmo, ref = '1995') | year + GEOID, 
#   cluster = ~state_fips + year
# )
# fert_mod_levels = feols(
#   data = est_dt[year >= 1980], 
#   split = ~rural, 
#   weight = ~tot_inf_births,
#   fml = c(n_commercial/area_km2, p_commercial/area_km2, n_total/area_km2, p_total/area_km2, n_manure/area_km2, p_manure/area_km2) ~ i(year, all_yield_diff_percentile_gmo, ref = '1995') | year + GEOID, 
#   cluster = ~state_fips + year
# )

# # Getting results into a table
# fert_result_dt = data.table(coeftable(fert_mod_logs))[,.(
#   id, 
#   rural = sample, 
#   lhs,
#   nutrient = fifelse(str_detect(lhs, 'n_'), 'Nitrogen', 'Phosphorous'),
#   type = fcase(
#     str_detect(lhs, 'commercial'), 'Commercial', 
#     str_detect(lhs, 'manure'), 'Manure', 
#     str_detect(lhs, 'total'), 'Total'
#   ) |> factor(levels = c('Commercial','Manure','Total')), 
#   year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.numeric(), 
#   estimate = Estimate, 
#   ci_l = Estimate + qnorm(0.025)*`Std. Error`, 
#   ci_h = Estimate + qnorm(0.975)*`Std. Error`
# )][,lhs_name := paste(nutrient, type)]
# # Add the reference bin 
# fert_result_dt =
#   rbind(
#     fert_result_dt[year != 1995],
#     fert_result_dt[,.(
#       id, rural, lhs, nutrient, type, lhs_name,
#       year = 1995, 
#       estimate = 0, ci_l = 0, ci_h = 0
#     )] |> unique()
#   )

# ggplot(fert_result_dt[rural == TRUE], aes(x = year, y = estimate, color = nutrient, fill = nutrient)) + 
#   geom_hline(yintercept = 0) + 
#   geom_vline(xintercept = 1995, linetype = 'dashed') + 
#   geom_line() + 
# #  geom_point() + 
#   geom_ribbon(aes(ymin = ci_l, ymax = ci_h), alpha = 0.3, color = NA) + 
#   theme_minimal() + 
#   scale_y_continuous(
#     name = 'Partial Elasticity', 
#     label = scales::label_percent()
#   ) +
#   scale_x_continuous(
#     name = '', breaks = seq(1980,2020, by = 10)
#   ) +
#   scale_color_brewer(
#     name = 'Nutrient',
#     palette = 'Dark2',
#     aesthetics = c('color','fill')
#   ) +
#   facet_wrap(~nutrient + type, scales = 'free') + 
#   theme(legend.position = 'bottom')


# # For rural places
# fert_result_dt_levels = data.table(coeftable(fert_mod_levels))[,.(
#   id, 
#   rural = sample, 
#   lhs,
#   nutrient = fifelse(str_detect(lhs, 'n_'), 'Nitrogen', 'Phosphorous'),
#   type = fcase(
#     str_detect(lhs, 'commercial'), 'Commercial', 
#     str_detect(lhs, 'manure'), 'Manure', 
#     str_detect(lhs, 'total'), 'Total'
#   ) |> factor(levels = c('Commercial','Manure','Total')), 
#   year = str_extract(coefficient, '(?<=year::)\\d{4}') |> as.numeric(), 
#   estimate = Estimate, 
#   ci_l = Estimate + qnorm(0.025)*`Std. Error`, 
#   ci_h = Estimate + qnorm(0.975)*`Std. Error`
# )][,lhs_name := paste(nutrient, type)]
# # Add the reference bin 
# fert_result_dt_levels =
#   rbind(
#     fert_result_dt_levels[year != 1995],
#     fert_result_dt_levels[,.(
#       id, rural, lhs, nutrient, type, lhs_name,
#       year = 1995, 
#       estimate = 0, ci_l = 0, ci_h = 0
#     )] |> unique()
#   )

# ggplot(
#   fert_result_dt_levels[rural == TRUE], 
#   aes(x = year, y = estimate, color = nutrient, fill = nutrient)
# ) + 
#   geom_hline(yintercept = 0) + 
#   geom_vline(xintercept = 1995, linetype = 'dashed') + 
#   geom_line() + 
#   #geom_point() + 
#   geom_ribbon(aes(ymin = ci_l, ymax = ci_h), alpha = 0.3, color = NA) + 
#   theme_minimal() + 
#   scale_y_continuous(
#     name = 'Estimate (kg/km2)'
#     #label = scales::label_number(scale = 1e-6)
#   ) +
#   scale_x_continuous(
#     name = '', breaks = seq(1950,2020, by = 10)
#   ) +
#   scale_color_brewer(
#     name = 'Nutrient',
#     palette = 'Dark2',
#     aesthetics = c('color','fill')
#   ) +
#   facet_wrap(~nutrient + type, scales = 'free') + 
#   theme(legend.position = 'bottom')
