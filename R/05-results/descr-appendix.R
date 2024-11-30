# Some appendix figures 
library(pacman)
p_load(
  here, data.table, fst, ggplot2, readr, 
  stringr, collapse, purrr
)

# First correlation between GAEZ and acreage ---------------------------------
  # Loading GAEZ data
  gaez_dt = read.fst(
    path = here('data/clean/trt-dt.fst'),
    as.data.table = TRUE
  )
  # Loading pre-period acreage and yield
  crop_instr_dt = 
    read.fst(
      path = here('data/clean/crop-acre-percentile-90-95.fst'),
      as.data.table = TRUE
    ) |>
    merge(
      gaez_dt,
      by = 'GEOID', 
      all.x = TRUE 
    )   
  # Making crop names consistent btw GAEZ and acre/yield 
  setnames(
    crop_instr_dt, 
    colnames(crop_instr_dt) |>
      str_replace('(?<=all_yield_diff_percentile_)cot', 'cotton') |>
      str_replace('(?<=all_yield_diff_percentile_)mze', 'corn') |>
      str_replace('(?<=all_yield_diff_percentile_)gmo', 'gm') |>
      str_replace('gm_yield_avg','gm_yield') |>
      str_replace('gm_yield_max','gm_max_yield')
  )


# Making the plots 
gaez_crop_acreage_correlation = function(crop_in, crop_instr_dt, pink = '#e64173'){
  if(crop_in != 'gm_max'){
    acres_p =   
      ggplot(
        data = crop_instr_dt, 
        aes(
          x = .data[[paste0('all_yield_diff_percentile_',crop_in)]],
          y = .data[[paste0('percentile_', crop_in, '_acres_pct_cnty')]]
        )
      ) + 
      geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
      geom_point(alpha = 0.05) + 
      geom_smooth(method = 'loess', se = FALSE, color = pink) + 
      theme_minimal(base_size = 16) +
      scale_x_continuous(
        name = 'GAEZ Attainable Yield Percentile', 
        label = scales::label_percent()  
      ) + 
      scale_y_continuous(
        name = 'Pre-period Acreage Percentile', 
        label = scales::label_percent()  
      )
    ggsave(
      acres_p, 
      filename = here(
        'figures/descriptive/gaez-acreage',
        paste0('gaez-acreage-',crop_in,'.jpeg')
      ),
      width = 6, height = 4
    )
    # Correlation btw yield and acreage
    acres_yield_p =   
      ggplot(
        data = crop_instr_dt, 
        aes(
          x = .data[[paste0('percentile_', crop_in, '_yield')]],
          y = .data[[paste0('percentile_', crop_in, '_acres_pct_cnty')]]
        )
      ) + 
      geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
      geom_point(alpha = 0.05) + 
      geom_smooth(method = 'loess', se = FALSE, color = pink) + 
      theme_minimal(base_size = 16) +
      scale_x_continuous(
        name = 'Pre-period Yield Percentile', 
        label = scales::label_percent()  
      ) + 
      scale_y_continuous(
        name = 'Pre-period Acreage Percentile', 
        label = scales::label_percent()  
      )
    ggsave(
      acres_yield_p, 
      filename = here(
        'figures/descriptive/gaez-acreage',
        paste0('yield-acreage-',crop_in,'.jpeg')
      ),
      width = 6, height = 4
    )
  }
  yield_p =   
    ggplot(
      data = crop_instr_dt, 
      aes(
        x = .data[[paste0('all_yield_diff_percentile_',crop_in)]],
        y = .data[[paste0('percentile_', crop_in, '_yield')]]
      )
    ) + 
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
    geom_point(alpha = 0.05) + 
    geom_smooth(method = 'loess', se = FALSE, color = pink) + 
    theme_minimal(base_size = 16) +
    scale_x_continuous(
      name = 'GAEZ Attainable Yield Percentile', 
      label = scales::label_percent()  
    ) + 
    scale_y_continuous(
      name = 'Pre-period Yield Percentile', 
      label = scales::label_percent()  
    )
  ggsave(
    yield_p, 
    filename = here(
      'figures/descriptive/gaez-acreage',
      paste0('gaez-yield-',crop_in,'.jpeg')
    ),
    width = 6, height = 4
  )
  #return(acres_p)
}

# Running it
map(
  c('cotton','corn','soy','gm','gm_max'),
  gaez_crop_acreage_correlation,
  crop_instr_dt = crop_instr_dt
)

# Regressions of GAEZ on acreage 
p_load(fixest)

list(
  feols(
    data = crop_instr_dt, 
    fml = percentile_gm_acres_pct_cnty ~ all_yield_diff_percentile_gm_max
  ),
  feols(
    data = crop_instr_dt, 
    fml = percentile_corn_acres_pct_cnty ~ all_yield_diff_percentile_corn
  ),
  feols(
    data = crop_instr_dt, 
    fml = percentile_soy_acres_pct_cnty ~ all_yield_diff_percentile_soy
  ),
  feols(
    data = crop_instr_dt, 
    fml = percentile_cotton_acres_pct_cnty ~ all_yield_diff_percentile_cotton
  ),
  feols(
    data = crop_instr_dt, 
    fml = percentile_gm_yield ~ all_yield_diff_percentile_gm_max
  ),
  feols(
    data = crop_instr_dt, 
    fml = percentile_corn_yield ~ all_yield_diff_percentile_corn
  ),
  feols(
    data = crop_instr_dt, 
    fml = percentile_soy_yield ~ all_yield_diff_percentile_soy
  ),
  feols(
    data = crop_instr_dt, 
    fml = percentile_cotton_yield ~ all_yield_diff_percentile_cotton
  )
) |> etable(
    tex = TRUE,
    style.tex = style.tex(
      depvar.title = 'Dep Var:',
      model.format = "", 
      line.top = 'simple',
      line.bottom = 'simple',
      var.title = '\\midrule'
    ),
    dict = c(
      percentile_gm_acres_pct_cnty = 'GM Acreage Percentile',
      percentile_soy_acres_pct_cnty = 'Soy Acreage Percentile',
      percentile_corn_acres_pct_cnty = 'Corn Acreage Percentile',
      percentile_cotton_acres_pct_cnty = 'Cotton Acreage Percentile',
      percentile_gm_yield = 'GM Yield Percentile',
      percentile_soy_yield = 'Soy Yield Percentile',
      percentile_corn_yield = 'Corn Yield Percentile',
      percentile_cotton_yield = 'Cotton Yield Percentile',
      all_yield_diff_percentile_gm_max = 'GM GAEZ Yield Percentile',
      all_yield_diff_percentile_soy = 'Soy GAEZ Yield Percentile',
      all_yield_diff_percentile_corn = 'Corn GAEZ Yield Percentile',
      all_yield_diff_percentile_cotton = 'Cotton GAEZ Yield Percentile'
    ),
    se.below = TRUE,
    digits = 3,
    signif.code = NA,
    se.row = FALSE,
    #fitstat = ~n_millions,
    digits.stats = 2,
    tpt = TRUE,
    notes = "We first calculate the county-level 1990 to 1995 average planted acreage and yield for each of corn, soy, cotton, and the aggregate of all three for GM. We divide the acreage values by the total size of the county. We then convert the acreage share and yield values into a percentile relative to all counties in the continental US. The GAEZ yield percentiles are calculated as described in \ref{sec:data}.",
    label = 'tab:gaez-acre-corr',
    title = '\\textbf{Correlation between GAEZ suitability measures and pre-period acerage for GM crops.}'
  ) |> write(here('tables/gaez-acre-corr.tex'))


# Now the balance table 
library(pacman)
p_load(
  data.table, fixest, fst, here, ggplot2, modelsummary,
  stringr,dplyr, janitor, purrr, magrittr,
  vtable
)

# Reading the data ---------------------------------------------------
comb_dt = 
  read.fst(
    here("data/clean/comb-cnty-health-dt.fst"),
    as.data.table = TRUE
  )[,
    trt := fcase(
      rural == TRUE & all_yield_diff_gmo_50_0 == TRUE, "High GM Yield",
      rural == TRUE & all_yield_diff_gmo_50_0 == FALSE, "Low GM Yield",
      rural == FALSE, "Urban"
    )
  ][,
    num_counties := uniqueN(GEOID),by = trt
  ][,
    pct_male := tot_male_births/tot_inf_births
  ]

vars = c(
  "num_cnty",
  "median_birth_wt", 
  "pct_low_bw", 
  "pct_male", 
  "inf_mort", 
  "tot_inf_births",
  "glyph_km2",
  "tot_km2",
  "tot_pop",
  "pct_hisp",
  "unemployment_rate",
  "hs_some_pct",
  "hs_deg_pct",
  "college_some_pct",
  "college_deg_pct", 
  "inc_per_cap")

sum_dt = comb_dt[
  year %in% 1992:1995 & !is.na(trt), .(
    median_birth_wt = mean(median_birth_wt, na.rm = TRUE),
    pct_low_bw = mean(pct_low_bw, na.rm = TRUE),
    pct_male = 100*mean(pct_male, na.rm = TRUE),
    inf_mort = mean(inf_mort, na.rm = TRUE),
    tot_inf_births = mean(tot_inf_births, na.rm = TRUE),
    glyph_km2  = mean(glyph_km2, na.rm = TRUE),
    tot_km2 = 0.00404686*mean(tot_acres, na.rm = TRUE),
    tot_pop = mean(tot_pop, na.rm = TRUE)/1000,
    pct_hisp = 100*mean(pct_hisp,na.rm = TRUE), 
    unemployment_rate = 100*mean(unemployment_rate,na.rm = TRUE),
    hs_some_pct = mean(hs_some_pct, na.rm = TRUE),
    hs_deg_pct = mean(hs_deg_pct, na.rm = TRUE),
    college_some_pct = mean(college_some_pct, na.rm = TRUE),
    college_deg_pct = mean(college_deg_pct, na.rm = TRUE),
    inc_per_cap = mean(inc_per_cap_farm + inc_per_cap_nonfarm, na.rm = TRUE) 
  ),
  by = .(GEOID,trt)
][,num_cnty := uniqueN(GEOID), by = trt]


sumtable(
  data = sum_dt,
  group = 'trt',
  vars = vars,
  summ = c('mean(x)','sd(x)'),
  labels = c(
    "Number of Counties",
    "Birth Weight ($g$)", 
    "Pct Low Birth Weight", 
    "Percent Male", 
    "Infant Mortality", 
    "Total Births",
    "Glyphosate ($kg/km^2$)",
    "Total Crop Area ($km^2$)",
    "Total Pop (1000's)",
    "Percent Hispanic",
    "Unemployment Rate",
    "Pct Some HS Degree",
    "Pct HS Degree",
    "Pct Some College",
    "Pct College Degree", 
    "Income per Capita"),
  title = "Summary Statistics for high- and low-attainable yield counties between 1992 and 1995",
  digits = 3,
  out = "latex",
  note = "Means and standard deviations are calculated on county level averages between 1992 and 1995, which is the period prior to the release of GM crops."
)

# What percentage of glyphosate is used east of 100th meridian
comb_dt[year %in% 1992:2004,
  sum(ifelse(e100m == TRUE,glyphosate,0),na.rm = TRUE)
  /sum(glyphosate,na.rm = TRUE)
]

