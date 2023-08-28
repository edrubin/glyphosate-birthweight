
# Analyzes the diff-in-diff results for the county level outcomes 
# Packages
library(pacman)
p_load(
  fastverse, fixest, fst, qs, patchwork, magrittr, here, dplyr
)
fastverse_extend(topics = c('ST', 'DT', 'VI'))

# Load data --------------------------------------------------------------------

# Load the processed double-difference dataset
comb_cnty_health_dt = read_fst(
  path = here('data-clean/comb-cnty-health-dt.fst'),
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
comb_cnty_health_dt %<>% .[between(year, 1992, 2004)]

# Function to estimate Event Study and DiD --------------------------------------------
est_twfe_cnty = function(outcomes, trt, dist_wt, rural_in = TRUE){

  # First creating the formulas to estimate
  fml_event = paste0(
    "c(", 
    paste(outcomes, collapse = ","),
    ") ~ sw(",
    paste(paste0("i(year,",trt,", ref = 1995)"), collapse = ","),
    ") | year + GEOID"
  )
  fml_did = paste0(
    "c(", 
    paste(outcomes, collapse = ","),
    ") ~ sw(",
    paste(paste0("i(year>1995,",trt,", ref = FALSE)"), collapse = ","),
    ") | year + GEOID"
  )
  fml_event_water = paste0(
    "glyph_km2_awt_tot_15_up ~ sw(",
    paste(paste0("i(year,",trt,"_tot_",dist_wt,"_up, ref = 1995)"), collapse = ","),
    ") | year + GEOID"
  )
  fml_did_water = paste0(
    "glyph_km2_awt_tot_15_up ~ sw(",
    paste(paste0("i(year > 1995,",trt,"_tot_",dist_wt,"_up, ref = FALSE)"), collapse = ","),
    ") | year + GEOID"
  )
  
  # Now estimating the models 
  event_county = 
    feols(
      data = comb_cnty_health_dt[rural == rural_in], 
      fml = fml_event |> as.formula(),
      cluster = ~ year + GEOID,
      mem.clean = TRUE
    )
  qsave(
    event_county, 
    here(paste0("data-clean/results/20220615/event_county_",dist_wt,".qs")),
    preset = 'fast'
  )
  event_county_wbirth = 
    feols(
      data = comb_cnty_health_dt[rural == rural_in], 
      fml = fml_event |> as.formula(),
      cluster = ~ year + GEOID,
      mem.clean = TRUE,
      weights = ~tot_inf_births
    )
  qsave(
    event_county_wbirth, 
    here(paste0("data-clean/results/20220615/event_county_wbirth_",dist_wt,".qs")),
    preset = 'fast'
  )
  event_county_water = 
    feols(
      data = comb_cnty_health_dt[rural == rural_in], 
      fml = fml_event_water |> as.formula(),
      cluster = ~ year + GEOID,
      mem.clean = TRUE
    )
  qsave(
    event_county_water, 
    here(paste0("data-clean/results/20220615/event_county_water_",dist_wt,".qs")),
    preset = 'fast'
  )
  event_county_water_wbirth = 
    feols(
      data = comb_cnty_health_dt[rural == rural_in], 
      fml = fml_event_water |> as.formula(),
      cluster = ~ year + GEOID,
      mem.clean = TRUE,
      weights = ~tot_inf_births
    )
  qsave(
    event_county_water_wbirth, 
    here(paste0("data-clean/results/20220615/event_county_water_wbirth_",dist_wt,".qs")),
    preset = 'fast'
  )
  did_county = 
    feols(
      data = comb_cnty_health_dt[rural == rural_in], 
      fml = fml_did |> as.formula(),
      cluster = ~ year + GEOID,
      mem.clean = TRUE
    )
  qsave(
    did_county, 
    here(paste0("data-clean/results/20220615/did_county_",dist_wt,".qs")),
    preset = 'fast'
  )
  did_county_wbirth = 
    feols(
      data = comb_cnty_health_dt[rural == rural_in], 
      fml = fml_did |> as.formula(),
      cluster = ~ year + GEOID,
      mem.clean = TRUE,
      weights = ~tot_inf_births
    )
  qsave(
    did_county_wbirth, 
    here(paste0("data-clean/results/20220615/did_county_wbirth_",dist_wt,".qs")),
    preset = 'fast'
  )
  did_county_water = 
    feols(
      data = comb_cnty_health_dt[rural == rural_in], 
      fml = fml_did_water |> as.formula(),
      cluster = ~ year + GEOID,
      mem.clean = TRUE
    )
  qsave(
    did_county_water, 
    here(paste0("data-clean/results/20220615/did_county_water_",dist_wt,".qs")),
    preset = 'fast'
  )
  did_county_water_wbirth = 
    feols(
      data = comb_cnty_health_dt[rural == rural_in], 
      fml = fml_did_water |> as.formula(),
      cluster = ~ year + GEOID,
      mem.clean = TRUE,
      weights = ~tot_inf_births
    )
  qsave(
    did_county_water_wbirth, 
    here(paste0("data-clean/results/20220615/did_county_water_wbirth_",dist_wt,".qs")),
    preset = 'fast'
  )

}  

# Run function -----------------------------------------------------------------
# Run the function with desired instruments
outcomes = c(
  'glyph_km2',
  'alachlor_km2', 
  'cyanazine_km2', 
  'fluazifop_km2', 
  'metolachlor_km2', 
  'metribuzin_km2', 
  'nicosulfuron_km2', 
  'herbicide_km2', 
  'insecticide_km2', 
  'soy_acres', 
  'corn_acres', 
  'tot_acres', 
  'other_acres',
  'soy_yield', 
  'corn_yield', 
  'cotton_yield', 
  'other_yield'
  # pct_hisp, pct_farm_empl, unemployment_rate,  
  # hs_deg_pct, college_some_pct, college_deg_pct
)
trt = c(
  'e100m_yield_diff_soy_50_0',
  'e100m_yield_diff_soy_50_10',
  'e100m_yield_diff_soy_40_0',
  'e100m_yield_diff_soy_60_0',
  'e100m_yield_diff_gmo_50_0',
  'e100m_yield_diff_gmo_50_10',
  'e100m_yield_diff_gmo_40_0',
  'e100m_yield_diff_gmo_60_0',
  'all_yield_diff_soy_50_0',
  'all_yield_diff_gmo_50_0'
)


# Eastern Soy
est_twfe_cnty(outcomes, trt, dist_wt = "m")
est_twfe_cnty(outcomes, trt, dist_wt = "h")
est_twfe_cnty(outcomes, trt, dist_wt = "l")
est_twfe_cnty(outcomes, trt, dist_wt = "05")
est_twfe_cnty(outcomes, trt, dist_wt = "10")
est_twfe_cnty(outcomes, trt, dist_wt = "15")

  
  
iplot(event_county[[1]])

did_county = 
  feols(
    data = comb_cnty_health_dt[rural == TRUE], 
    fml = 
      c(
        glyph_km2 
        ,alachlor, cyanazine, fluazifop, metolachlor, metribuzin, nicosulfuron, 
        herbicide, insecticide, 
        soy_acres, corn_acres, tot_acres, other_acres,
        soy_yield, corn_yield, cotton_yield, other_yield
        #pct_hisp, pct_farm_empl, unemployment_rate,  
        # hs_deg_pct, college_some_pct, college_deg_pct
      )
    ~ i(year > 1995, e100m_yield_diff_gmo_50_0, ref = FALSE)
    | year + GEOID,
    cluster = ~ year + GEOID,
    mem.clean = TRUE
  )

did_county



# Weighted by births
event_county_wtbirth = 
  feols(
    data = comb_cnty_health_dt[rural == TRUE], 
    fml = c(
      glyph_km2, 
      inf_mort, inf_mort_int, sex_ratio, inf_mort_ext,
      alachlor, cyanazine, fluazifop, metolachlor, metribuzin, nicosulfuron, 
      herbicide, insecticide, 
      soy_acres, corn_acres, tot_acres, other_acres,
      soy_yield, corn_yield, cotton_yield, other_yield
      #pct_hisp, pct_farm_empl, unemployment_rate,  
      #hs_deg_pct, college_some_pct, college_deg_pct
    )
    ~ i(year, e100m_yield_diff_gmo_50_0, ref = 1995)
    | year + GEOID,
    cluster = ~ year + GEOID,
    weights = ~tot_inf_births,
    mem.clean = TRUE
  )

iplot(event_county_wtbirth[[1]])

# Using continuous yield diff
cont_county_wtbirth = 
  feols(
    data = comb_cnty_health_dt[rural == TRUE], 
    fml = c(
      glyph_km2, 
      inf_mort, inf_mort_int, sex_ratio, inf_mort_ext,
      alachlor, cyanazine, fluazifop, metolachlor, metribuzin, nicosulfuron, 
      herbicide, insecticide, 
      soy_acres, corn_acres, tot_acres, other_acres,
      soy_yield, corn_yield, cotton_yield, other_yield
      #pct_hisp, pct_farm_empl, unemployment_rate,  
      #hs_deg_pct, college_some_pct, college_deg_pct
    )
    ~ i(year, yield_diff_gmo, ref = 1995)
    | year + GEOID,
    cluster = ~ year + GEOID,
    weights = ~tot_inf_births,
    mem.clean = TRUE
  )

iplot(cont_county_wtbirth[[1]])


# Upstream
event_county_up_wtbirth = 
  feols(
    data = comb_cnty_health_dt[rural == TRUE], 
    fml = c(
      glyph_km2_awt_tot_m_up, 
      inf_mort, inf_mort_int, sex_ratio, inf_mort_ext
      #pct_hisp, pct_farm_empl, unemployment_rate,  
      #hs_deg_pct, college_some_pct, college_deg_pct
    )
    ~ i(year, e100m_yield_diff_gmo_50_0_tot_m_up, ref = 1995)
    | year + GEOID,
    cluster = ~ year + GEOID,
    weights = ~tot_inf_births,
    mem.clean = TRUE
  )

iplot(event_county_up_wtbirth[[1]])



qsave(
  x = event_county,
  file = here('data-clean/results/20220609/event_county.qs'),
  preset = 'fast'
)
qsave(
  x = event_county_wtbirth,
  file = here('data-clean/results/20220609/event_county_wtbirth.qs'),
  preset = 'fast'
)
qsave(
  x = cont_county_wtbirth,
  file = here('data-clean/results/20220609/event_cont_county_wtbirth.qs'),
  preset = 'fast'
)
qsave(
  x = event_county_up_wtbirth,
  file = here('data-clean/results/20220609/event_county_up_wtbirth.qs'),
  preset = 'fast'
)

event_split_robust = 
  feols(
    data = comb_cnty_health_dt[rural == TRUE], 
    fml = glyph_km2 ~ sw(
      i(year, e100m_yield_diff_gmo_25_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_30_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_35_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_40_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_45_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_50_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_55_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_60_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_65_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_70_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_75_0, ref = 1995)
    )
    | year + GEOID,
    cluster = ~ year + GEOID,
    mem.clean = TRUE
  )

p_load(viridis)

iplot(
  event_split_robust,
  col = magma(n = 11, begin = 0.2, end = 0.8),
  pt.pch = 1
)  

event_crop_robust = 
  feols(
    data = comb_cnty_health_dt[rural == TRUE], 
    fml = glyph_km2 ~ sw(
      i(year, e100m_yield_diff_mze_50_0, ref = 1995),
      i(year, e100m_yield_diff_cot_50_0, ref = 1995),
      i(year, e100m_yield_diff_soy_50_0, ref = 1995),
      i(year, e100m_yield_diff_gmo_50_0, ref = 1995)
    )
    | year + GEOID,
    cluster = ~ year + GEOID,
    mem.clean = TRUE
  )

iplot(event_crop_robust)

qsave(
  x = event_split_robust,
  file = here('data-clean/results/20220609/event_split_robust.qs'),
  preset = 'fast'
)
qsave(
  x = event_crop_robust,
  file = here('data-clean/results/20220609/event_crop_robust.qs'),
  preset = 'fast'
)

