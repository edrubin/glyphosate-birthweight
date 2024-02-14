#-------------------------------------------------------
# Adding the county level health data to combined county 
# Runtime: 3 mins 
#-------------------------------------------------------

# Loading required packages
library(pacman)
p_load(
  magrittr, data.table, janitor, tidyr, dplyr, 
  readr, here, stringr, glue, fst, purrr, readxl
)

# A function to calculate infant mortality
calc_percent = function(numerator, denominator, scale = 1000){
  # Setting to 0 if numerator is NA but denominator is not
  ifelse(
    is.na(numerator) & !is.na(denominator),
    0,
    numerator*scale/denominator
  )
}

# Reading the other county level data
comb_cnty_dt =
  read.fst(  
    here("data/clean/comb-cnty-dt.fst"),
    as.data.table = TRUE
  )

# Reading in the natality data
natality_dt = map_dfr(
  1990:2013,
  \(yr){
    read.fst(
      path = here(paste0("data/health-restricted/period-clean/natality-",yr,".fst")),
      as.data.table = TRUE
    )[, `:=`(
        any_anomaly = 1 * (do.call(pmin, .SD) == 1),
        any_anomaly_nona = 1 * (do.call(function(...) pmin(..., na.rm = TRUE), .SD) == 1)
      ), 
      .SDcols = febrile:baby_other_cong
    ][,.(# Summarizing the results 
      median_birth_wt = median(dbwt, na.rm = T),
      median_birth_wt_male = median(ifelse(sex %in% c("1","M"), dbwt, as.numeric(NA)), na.rm = T),
      median_birth_wt_female = median(ifelse(!(sex %in% c("1","M")), dbwt, as.numeric(NA)), na.rm = T),
      median_birth_wt_q1 = median(ifelse(month %in% paste0("0",1:3), dbwt, as.numeric(NA)), na.rm = T),
      median_birth_wt_q2 = median(ifelse(month %in% paste0("0",4:6), dbwt, as.numeric(NA)), na.rm = T),
      median_birth_wt_q3 = median(ifelse(month %in% paste0("0",7:9), dbwt, as.numeric(NA)), na.rm = T),
      median_birth_wt_q4 = median(ifelse(month %in% paste0(10:12), dbwt, as.numeric(NA)), na.rm = T),
      median_birth_wt_nq1 = median(ifelse(!(month %in% paste0("0",1:3)), dbwt, as.numeric(NA)), na.rm = T),
      mean_birth_wt = mean(dbwt, na.rm = T),
      mean_birth_wt_male = mean(ifelse(sex %in% c("1","M"), dbwt, as.numeric(NA)), na.rm = T),
      mean_birth_wt_female = mean(ifelse(!(sex %in% c("1","M")), dbwt, as.numeric(NA)), na.rm = T),
      mean_birth_wt_q1 = mean(ifelse(month %in% paste0("0",1:3), dbwt, as.numeric(NA)), na.rm = T),
      mean_birth_wt_q2 = mean(ifelse(month %in% paste0("0",4:6), dbwt, as.numeric(NA)), na.rm = T),
      mean_birth_wt_q3 = mean(ifelse(month %in% paste0("0",7:9), dbwt, as.numeric(NA)), na.rm = T),
      mean_birth_wt_q4 = mean(ifelse(month %in% paste0(10:12), dbwt, as.numeric(NA)), na.rm = T),
      mean_birth_wt_nq1 = mean(ifelse(!(month %in% paste0("0",1:3)), dbwt, as.numeric(NA)), na.rm = T),
      low_birth_wt = sum(dbwt < 2500, na.rm = T),
      v_low_birth_wt = sum(dbwt < 1500, na.rm = T),
      low_birth_wt_male = sum(sex %in% c("1","M") & dbwt < 2500, na.rm = T),
      low_birth_wt_female = sum(!(sex %in% c("1","M")) & dbwt < 2500, na.rm = T),
      low_birth_wt_q1 = sum(month %in% paste0("0",1:3) & dbwt < 2500, na.rm = T),
      low_birth_wt_q2 = sum(month %in% paste0("0",4:6) & dbwt < 2500, na.rm = T),
      low_birth_wt_q3 = sum(month %in% paste0("0",7:9) & dbwt < 2500, na.rm = T),
      low_birth_wt_q4 = sum(month %in% paste0(10:12) & dbwt < 2500, na.rm = T),
      tot_male_births = sum(sex %in% c("1","M"), na.rm = T),
      tot_male_births_q1 = sum(month %in% paste0("0",1:3) & sex %in% c("1","M"), na.rm = T),
      tot_male_births_q2 = sum(month %in% paste0("0",4:6) & sex %in% c("1","M"), na.rm = T),
      tot_male_births_q3 = sum(month %in% paste0("0",7:9) & sex %in% c("1","M"), na.rm = T),
      tot_male_births_q4 = sum(month %in% paste0(  10:12) & sex %in% c("1","M"), na.rm = T),
      tot_inf_births = .N,
      tot_inf_births_q1 = sum(month %in% paste0("0",1:3), na.rm = T),
      tot_inf_births_q2 = sum(month %in% paste0("0",4:6), na.rm = T),
      tot_inf_births_q3 = sum(month %in% paste0("0",7:9), na.rm = T),
      tot_inf_births_q4 = sum(month %in% paste0(10:12), na.rm = T),
      # Some new indicators
      tot_c_section = sum(c_section, na.rm = TRUE),
      tot_vent_under_30m = sum(vent_under_30m == '1', na.rm = TRUE),
      tot_vent_over_30m = sum(vent_over_30m == '1', na.rm = TRUE),
      median_gest = median(
        fifelse(gestation == '99', as.numeric(NA), as.numeric(gestation)), 
        na.rm = TRUE
      ),
      mean_gest = mean(
        fifelse(gestation == '99', as.numeric(NA), as.numeric(gestation)), 
        na.rm = TRUE
      ),
      preterm = sum(
        as.integer(gestation) <= 37, 
        na.rm = TRUE
      ),
      any_anomaly = sum(any_anomaly, na.rm = TRUE),
      pct_tobacco = mean(tobacco, na.rm = TRUE),
      pct_alcohol = mean(alcohol, na.rm = TRUE)
    ),
    by = .(GEOID, year)
    ]
})


# Pre period mortality
mortality_dt = map_dfr(
  1990:2013,
  \(yr){
    read.fst(
      path = here(paste0("data/health-restricted/period-clean/mortality-",yr,".fst")),
      as.data.table = TRUE
    )[,':='(
      age = as.character(age),
      sex = as.character(sex),
      hisp = as.character(hisp)
    )]
  })[,.(# Summarizing the results 
      tot_male_deaths = sum(sex %in% c("M","1"), na.rm = T),
      tot_male_deaths_q1 = sum(month_birth %in% paste0("0",1:3) & sex %in% c("1","M"), na.rm = T),
      tot_male_deaths_q2 = sum(month_birth %in% paste0("0",4:6) & sex %in% c("1","M"), na.rm = T),
      tot_male_deaths_q3 = sum(month_birth %in% paste0("0",7:9) & sex %in% c("1","M"), na.rm = T),
      tot_male_deaths_q4 = sum(month_birth %in% paste0(  10:12) & sex %in% c("1","M"), na.rm = T),
      tot_male_int_deaths = sum(internal & sex %in% c("M","1"), na.rm = T),
      tot_male_int_deaths_q1 = sum(internal & month_birth %in% paste0("0",1:3) & sex %in% c("1","M"), na.rm = T),
      tot_male_int_deaths_q2 = sum(internal & month_birth %in% paste0("0",4:6) & sex %in% c("1","M"), na.rm = T),
      tot_male_int_deaths_q3 = sum(internal & month_birth %in% paste0("0",7:9) & sex %in% c("1","M"), na.rm = T),
      tot_male_int_deaths_q4 = sum(internal & month_birth %in% paste0(  10:12) & sex %in% c("1","M"), na.rm = T),
      tot_int_deaths = sum(internal, na.rm = T),
      tot_int_deaths_q1 = sum(internal & month_birth %in% paste0("0",1:3), na.rm = T),
      tot_int_deaths_q2 = sum(internal & month_birth %in% paste0("0",4:6), na.rm = T),
      tot_int_deaths_q3 = sum(internal & month_birth %in% paste0("0",7:9), na.rm = T),
      tot_int_deaths_q4 = sum(internal & month_birth %in% paste0(  10:12), na.rm = T),
      tot_inf_deaths = .N,
      tot_inf_deaths_q1 = sum(month_birth %in% paste0("0",1:3), na.rm = T),
      tot_inf_deaths_q2 = sum(month_birth %in% paste0("0",4:6), na.rm = T),
      tot_inf_deaths_q3 = sum(month_birth %in% paste0("0",7:9), na.rm = T),
      tot_inf_deaths_q4 = sum(month_birth %in% paste0(  10:12), na.rm = T),
      tot_infection_deaths = sum(category_code == 1, na.rm = TRUE),
      tot_neoplasm_deaths = sum(category_code == 2, na.rm = TRUE),
      tot_blood_deaths = sum(category_code == 3, na.rm = TRUE),
      tot_endocrine_deaths = sum(category_code == 4, na.rm = TRUE),
      tot_nervous_deaths = sum(category_code == 5, na.rm = TRUE),
      tot_ear_deaths = sum(category_code == 6, na.rm = TRUE),
      tot_circulatory_deaths = sum(category_code == 7, na.rm = TRUE),
      tot_respiratory_deaths = sum(category_code == 8, na.rm = TRUE),
      tot_digestive_deaths = sum(category_code == 9, na.rm = TRUE),
      tot_genitourinary_deaths = sum(category_code == 10, na.rm = TRUE),
      tot_mother_deaths = sum(category_code == 11, na.rm = TRUE),
      tot_low_weight_deaths = sum(category_code == 12, na.rm = TRUE),
      tot_sids_deaths = sum(category_code == 13, na.rm = TRUE),
      tot_congenital_deaths = sum(category_code == 14, na.rm = TRUE)
    ),
    by = .(GEOID, year = year_birth)
  ] 

# Merging mort and nat together
health_dt = merge(
  natality_dt,
  mortality_dt,
  by = c("GEOID","year"),
  all = TRUE
)

# Creating health metrics 
health_dt[,':='(
  inf_mort = calc_percent(tot_inf_deaths, tot_inf_births),
  inf_mort_male = calc_percent(tot_male_deaths, tot_male_births), 
  inf_mort_female = calc_percent(tot_inf_deaths-tot_male_deaths, tot_inf_births-tot_male_births),
  inf_mort_q1 = calc_percent(tot_inf_deaths_q1, tot_inf_births_q1),
  inf_mort_q2 = calc_percent(tot_inf_deaths_q2, tot_inf_births_q2),
  inf_mort_q3 = calc_percent(tot_inf_deaths_q3, tot_inf_births_q3),
  inf_mort_q4 = calc_percent(tot_inf_deaths_q4, tot_inf_births_q4),
  inf_mort_int =  calc_percent(tot_int_deaths, tot_inf_births),
  inf_mort_int_male =  calc_percent(tot_male_int_deaths, tot_male_births),
  inf_mort_int_female =  calc_percent(tot_int_deaths - tot_male_int_deaths, tot_inf_births - tot_male_births),
  inf_mort_int_q1 = calc_percent(tot_int_deaths_q1, tot_inf_births_q1),
  inf_mort_int_q2 = calc_percent(tot_int_deaths_q2, tot_inf_births_q2),
  inf_mort_int_q3 = calc_percent(tot_int_deaths_q3, tot_inf_births_q3),
  inf_mort_int_q4 = calc_percent(tot_int_deaths_q4, tot_inf_births_q4),
  inf_mort_ext = calc_percent(tot_inf_deaths - tot_int_deaths, tot_inf_births),
  inf_mort_ext_q1 = calc_percent(tot_inf_deaths_q1 - tot_int_deaths_q1, tot_inf_births_q1),
  inf_mort_ext_q2 = calc_percent(tot_inf_deaths_q2 - tot_int_deaths_q2, tot_inf_births_q2),
  inf_mort_ext_q3 = calc_percent(tot_inf_deaths_q3 - tot_int_deaths_q3, tot_inf_births_q3),
  inf_mort_ext_q4 = calc_percent(tot_inf_deaths_q4 - tot_int_deaths_q4, tot_inf_births_q4),
  sex_ratio = calc_percent(tot_male_births, tot_inf_births - tot_male_births, scale = 100),
  sex_ratio_q1 = calc_percent(tot_male_births_q1, tot_inf_births_q1 - tot_male_births_q1, scale = 100),
  sex_ratio_q2 = calc_percent(tot_male_births_q2, tot_inf_births_q2 - tot_male_births_q2, scale = 100),
  sex_ratio_q3 = calc_percent(tot_male_births_q3, tot_inf_births_q3 - tot_male_births_q3, scale = 100),
  sex_ratio_q4 = calc_percent(tot_male_births_q4, tot_inf_births_q4 - tot_male_births_q4, scale = 100),
  pct_low_bw = calc_percent(low_birth_wt, tot_inf_births, scale = 100),
  pct_low_bw_q1 = calc_percent(low_birth_wt_q1, tot_inf_births_q1, scale = 100),
  pct_low_bw_q2 = calc_percent(low_birth_wt_q2, tot_inf_births_q2, scale = 100),
  pct_low_bw_q3 = calc_percent(low_birth_wt_q3, tot_inf_births_q3, scale = 100),
  pct_low_bw_q4 = calc_percent(low_birth_wt_q4, tot_inf_births_q4, scale = 100),
  inf_mort_infection = calc_percent(tot_infection_deaths,tot_inf_births),
  inf_mort_neoplasm = calc_percent(tot_neoplasm_deaths,tot_inf_births),
  inf_mort_blood = calc_percent(tot_blood_deaths,tot_inf_births),
  inf_mort_endocrine = calc_percent(tot_endocrine_deaths,tot_inf_births),
  inf_mort_nervous = calc_percent(tot_nervous_deaths,tot_inf_births),
  inf_mort_ear = calc_percent(tot_ear_deaths,tot_inf_births),
  inf_mort_circulatory = calc_percent(tot_circulatory_deaths,tot_inf_births),
  inf_mort_respiratory = calc_percent(tot_respiratory_deaths,tot_inf_births),
  inf_mort_digestive = calc_percent(tot_digestive_deaths,tot_inf_births),
  inf_mort_genitourinary = calc_percent(tot_genitourinary_deaths,tot_inf_births),
  inf_mort_mother = calc_percent(tot_mother_deaths,tot_inf_births),
  inf_mort_low_weight = calc_percent(tot_low_weight_deaths,tot_inf_births),
  inf_mort_sids = calc_percent(tot_sids_deaths,tot_inf_births),
  inf_mort_congenital = calc_percent(tot_congenital_deaths,tot_inf_births),
  pct_c_section = calc_percent(tot_c_section, tot_inf_births, scale =100),
  pct_preterm = calc_percent(preterm, tot_inf_births, scale = 100),
  pct_vent_over_30m = calc_percent(tot_vent_over_30m, tot_inf_births, scale = 100),
  pct_vent_under_30m = calc_percent(tot_vent_under_30m, tot_inf_births, scale = 100),
  tot_vent = tot_vent_under_30m + tot_vent_over_30m
)]
health_dt[, pct_vent := calc_percent(tot_vent, tot_inf_births, scale = 100)]

  
# Combining health and county level data
comb_health_dt = 
  merge(
    comb_cnty_dt,
    health_dt, 
    by = c("GEOID","year"),
    all = TRUE
  ) 

# Saving the results 
write.fst(
  comb_health_dt,
  here("data/clean/comb-cnty-health-dt.fst")
)
write.fst(
  health_dt,
  here("data/clean/health-dt.fst")
)
