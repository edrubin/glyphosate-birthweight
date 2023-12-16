
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
  'farm_empl', 'tot_empl', 'unemployment_rate'
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




