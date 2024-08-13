# Script to monetize the costs of infant health effects 
library(pacman)
p_load(
  here, data.table, fst, collapse
)

# Simple method first: Waitzman 2023--$64,815 per preterm birth (2016 USD)-----
  # Birth and GLY data
  comb_cnty_health_dt = 
    read_fst(
      path = here('data/clean/comb-cnty-health-dt.fst'),
      as.data.table = TRUE
    )[between(year, 1990, 2013) & rural == TRUE & !is.na(tot_inf_births),.(
      GEOID, 
      year, 
      glyph_km2, 
      tot_inf_births
    )]
  # Policy
  policy_2016 = c(
    64815*0.0046*comb_cnty_health_dt[year == 2012,.(fsum(tot_inf_births))]$V1/1e6,
    64815*0.0159*comb_cnty_health_dt[year == 2012,.(fsum(tot_inf_births))]$V1/1e6,
    64815*0.0272*comb_cnty_health_dt[year == 2012,.(fsum(tot_inf_births))]$V1/1e6
  )
  # GLY
  gly_2016 = c(
    64815*0.0023*comb_cnty_health_dt[year == 2012,.(fsum(tot_inf_births))]$V1/1e6,
    64815*0.0224*comb_cnty_health_dt[year == 2012,.(fsum(tot_inf_births))]$V1/1e6,
    64815*0.0426*comb_cnty_health_dt[year == 2012,.(fsum(tot_inf_births))]$V1/1e6
  )
  # Converting into other years: 
  cat(paste0(
    '2023 Dollars\n  Policy Effect $', round(policy_2016[2]*304.702/240.007,0),
    ' million (',round(policy_2016[1]*304.702/240.007,0),', ',round(policy_2016[3]*304.702/240.007,0),')' ,
    '\n  GLY Effect $', round(gly_2016[2]*304.702/240.007,0),
    ' million (',round(gly_2016[1]*304.702/240.007,0),', ',round(gly_2016[3]*304.702/240.007,0),')',
    '\n2016 Dollars\n  Policy Effect $', round(policy_2016[2]*240.007/240.007,0),
    ' million (',round(policy_2016[1]*240.007/240.007,0),', ',round(policy_2016[3]*240.007/240.007,0),')' ,
    '\n  GLY Effect $', round(gly_2016[2]*240.007/240.007,0),
    ' million (',round(gly_2016[1]*240.007/240.007,0),', ',round(gly_2016[3]*240.007/240.007,0),')',
    '\n2012 Dollars\n  Policy Effect $', round(policy_2016[2]*229.594/240.007,0),
    ' million (',round(policy_2016[1]*229.594/240.007,0),', ',round(policy_2016[3]*229.594/240.007,0),')' ,
    '\n  GLY Effect $', round(gly_2016[2]*229.594/240.007,0),
    ' million (',round(gly_2016[1]*229.594/240.007,0),', ',round(gly_2016[3]*229.594/240.007,0),')'  
  ))
  
  

64815*304.702/240.007