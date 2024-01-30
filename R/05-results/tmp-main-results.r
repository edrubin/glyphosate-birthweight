# Temporary holding for results script 
# Note: won't run in this repository because we haven't moved data over yet
# FROM: here('glyphosate/presentations/04-rubin-ucsb/zz-data-work.r')
#   AND here('glyphosate/R/05-results/nature-results.R')


# Annual summaries -----------------------------------------------------------------------
  # Load packages 
  pacman::p_load(fastverse, here)
  # Load data
  co_dt = here('data-clean', 'comb-cnty-dt.fst') %>% read_fst(as.data.table = TRUE)
  # Summarize by year
  yr_dt = co_dt[, .(
    gly = fsum(glyphosate),
    acres_corn = fsum(corn_acres),
    acres_soy = fsum(soy_acres),
    acres_cotton = fsum(cotton_acres),
    acres_other = fsum(other_acres),
    acres_total = fsum(corn_acres + soy_acres + cotton_acres + other_acres),
    yield_corn = fsum(corn_yield),
    yield_soy = fsum(soy_yield),
    yield_cotton = fsum(cotton_yield),
    yield_other = fsum(other_yield)
  ), keyby = year]
  # Save
  write_fst(
    x = yr_gmo_dt,
    path = here('presentations', '04-rubin-ucsb', 'data', 'yr.fst'),
    compress = 100
  )
  # Summarize by year and suitability
  yr_gmo_dt = co_dt[all_yield_diff_percentile_gmo_max <= 0.25 | all_yield_diff_percentile_gmo_max >= 0.75, .(
  # yr_gmo_dt = co_dt[, .(
    gly = fsum(glyphosate),
    acres_corn = fsum(corn_acres),
    acres_soy = fsum(soy_acres),
    acres_cotton = fsum(cotton_acres),
    acres_other = fsum(other_acres),
    acres_total = fsum(corn_acres + soy_acres + cotton_acres + other_acres),
    yield_corn = fsum(corn_yield),
    yield_soy = fsum(soy_yield),
    yield_cotton = fsum(cotton_yield),
    yield_other = fsum(other_yield),
    area_km2 = fsum(area_km2)
  ), keyby = .(year, gmo_suitable = round(all_yield_diff_percentile_gmo_max))]
  # Save
  write_fst(
    x = yr_gmo_dt,
    path = here('presentations', '04-rubin-ucsb', 'data', 'yr-gmo.fst'),
    compress = 100
  )


# First-stage event studies --------------------------------------------------------------
  # Load packages 
  pacman::p_load(fastverse, fixest, here)
  # Load data
  co_dt = here('data-clean', 'comb-cnty-dt.fst') %>% read_fst(as.data.table = TRUE)
  # Set 'rural' to 1995 value
  tmp = co_dt[year == 1995, .(GEOID, rural)]
  co_dt[, rural := NULL]
  co_dt %<>% merge(tmp, by = 'GEOID')
  # A few county-level first-stage regressions
  fs_all_gmo = feols(
    glyph_km2 ~ i(year, all_yield_diff_percentile_gmo, ref = 1995) | year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_all_gmo_max = feols(
    glyph_km2 ~ i(year, all_yield_diff_percentile_gmo_max, ref = 1995) | year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_all_gmo_50_0_controls = feols(
    glyph_km2 ~ i(year, all_yield_diff_gmo_50_0, ref = 1995) +
    unemployment_rate |
    year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_all_gmo_controls = feols(
    glyph_km2 ~ i(year, all_yield_diff_percentile_gmo, ref = 1995) +
    unemployment_rate |
    year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_all_gmo_max_controls = feols(
    glyph_km2 ~ i(year, all_yield_diff_percentile_gmo_max, ref = 1995) +
    unemployment_rate |
    year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_all_gmo_50_0 = feols(
    glyph_km2 ~ i(year, all_yield_diff_gmo_50_0, ref = 1995) | year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_e100m_gmo = feols(
    glyph_km2 ~ i(year, e100m_yield_diff_percentile_gmo, ref = 1995) | year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_e100m_gmo_max = feols(
    glyph_km2 ~ i(year, e100m_yield_diff_percentile_gmo_max, ref = 1995) | year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_e100m_gmo_50_0 = feols(
    glyph_km2 ~ i(year, e100m_yield_diff_gmo_50_0, ref = 1995) | year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_e100m_gmo_50_0_controls = feols(
    glyph_km2 ~ i(year, e100m_yield_diff_gmo_50_0, ref = 1995) +
    unemployment_rate |
    year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_e100m_gmo_controls = feols(
    glyph_km2 ~ i(year, e100m_yield_diff_percentile_gmo, ref = 1995) +
    unemployment_rate |
    year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  fs_e100m_gmo_max_controls = feols(
    glyph_km2 ~ i(year, e100m_yield_diff_percentile_gmo_max, ref = 1995) +
    unemployment_rate |
    year + GEOID,
    cluster = ~ year + state_fips,
    data = co_dt[year >= 1992 & rural == TRUE]
  )
  # Combine event-study estimates
  fs_st = list(
    'fs_all_gmo' = fs_all_gmo %>% iplot() %$% prms ,
    'fs_all_gmo_max' = fs_all_gmo_max %>% iplot() %$% prms ,
    'fs_all_gmo_50_0' = fs_all_gmo_50_0 %>% iplot() %$% prms ,
    'fs_e100m_gmo' = fs_e100m_gmo %>% iplot() %$% prms ,
    'fs_e100m_gmo_max' = fs_e100m_gmo_max %>% iplot() %$% prms ,
    'fs_e100m_gmo_50_0' = fs_e100m_gmo_50_0 %>% iplot() %$% prms ,
    'fs_all_gmo_controls' = fs_all_gmo_controls %>% iplot() %$% prms ,
    'fs_all_gmo_max_controls' = fs_all_gmo_max_controls %>% iplot() %$% prms ,
    'fs_all_gmo_50_0_controls' = fs_all_gmo_50_0_controls %>% iplot() %$% prms ,
    'fs_e100m_gmo_controls' = fs_e100m_gmo_controls %>% iplot() %$% prms ,
    'fs_e100m_gmo_max_controls' = fs_e100m_gmo_max_controls %>% iplot() %$% prms ,
    'fs_e100m_gmo_50_0_controls' = fs_e100m_gmo_50_0_controls %>% iplot() %$% prms ,
    NULL
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  # Save the event-study estimates
  write_fst(
    x = fs_st,
    path = here('presentations', '04-rubin-ucsb', 'data', 'results', 'fs-event-study-st.fst')
  )
  # Repeat with clustering at county (rather than state)...
  # Combine event-study estimates
  fs_co = list(
    'fs_all_gmo' = fs_all_gmo %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_all_gmo_max' = fs_all_gmo_max %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_all_gmo_50_0' = fs_all_gmo_50_0 %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_e100m_gmo' = fs_e100m_gmo %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_e100m_gmo_max' = fs_e100m_gmo_max %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_e100m_gmo_50_0' = fs_e100m_gmo_50_0 %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_all_gmo_controls' = fs_all_gmo_controls %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_all_gmo_max_controls' = fs_all_gmo_max_controls %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_all_gmo_50_0_controls' = fs_all_gmo_50_0_controls %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_e100m_gmo_controls' = fs_e100m_gmo_controls %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_e100m_gmo_max_controls' = fs_e100m_gmo_max_controls %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    'fs_e100m_gmo_50_0_controls' = fs_e100m_gmo_50_0_controls %>% summary(cluster = ~ year + GEOID) %>% iplot() %$% prms ,
    NULL
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  # Save the event-study estimates
  write_fst(
    x = fs_co,
    path = here('presentations', '04-rubin-ucsb', 'data', 'results', 'fs-event-study-co.fst')
  )



# Reduced-form results -------------------------------------------------------------------
  # Load packages 
  pacman::p_load(fastverse, fixest, qs, here)
  fastverse_extend(topics = 'ST')
  # Load data
  rf1 = here(
    'data-clean', 'results', '20221220',
    'est_rf_outcome-dbwt-dbwtpctlruralpre-gestation_fe-03_controls-03_spatial-rural_het-_iv-e100myielddiffpercentilegmo.qs'
  ) %>% qread()
  rf2 = here(
    'data-clean', 'results', '20221220',
    'est_rf_outcome-dbwt-gestation_fe-03_controls-03_spatial-rural_het-_iv-allyielddiffpercentilegmo.qs'
  ) %>% qread()
  rf3 = here(
    'data-clean', 'results', '20230109',
    'est_rf_outcome-dbwtpctlruralpre_fe-03_controls-03_spatial-rural_het-_iv-allyielddiffpercentilegmo_cl-yearfips.qs'
  ) %>% qread()
  rf3b = here(
    'data-clean', 'results', '20230109',
    'est_rf_outcome-dbwtpctlruralpre_fe-03_controls-03_spatial-rural_het-_iv-allyielddiffpercentilegmo_cl-yearstatefips.qs'
  ) %>% qread()
  # Grab results as tables
  rf1_pctl = rf1 %>% names() %>% str_detect('dbwt_pctl') %>% which()
  rf1_dbwt = rf1 %>% names() %>% str_detect('dbwt') %>% which() %>% setdiff(rf1_pctl)
  rf1_gest = rf1 %>% names() %>% str_detect('gestation') %>% which()
  rf2_pctl = rf2 %>% names() %>% str_detect('dbwt_pctl') %>% which()
  rf2_dbwt = rf2 %>% names() %>% str_detect('dbwt') %>% which() %>% setdiff(rf2_pctl)
  rf2_gest = rf2 %>% names() %>% str_detect('gestation') %>% which()
  # Build tables of results
  est_rf1_pctl = lapply(
    X = rf1_pctl,
    FUN = function(i) rf1[[i]] %>% iplot() %$% prms %>% as.data.table()
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  est_rf1_dbwt = lapply(
    X = rf1_dbwt,
    FUN = function(i) rf1[[i]] %>% iplot() %$% prms %>% as.data.table()
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  est_rf1_gest = lapply(
    X = rf1_gest,
    FUN = function(i) rf1[[i]] %>% iplot() %$% prms %>% as.data.table()
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  est_rf2_pctl = lapply(
    X = rf2_pctl,
    FUN = function(i) rf2[[i]] %>% iplot() %$% prms %>% as.data.table()
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  est_rf2_dbwt = lapply(
    X = rf2_dbwt,
    FUN = function(i) rf2[[i]] %>% iplot() %$% prms %>% as.data.table()
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  est_rf2_gest = lapply(
    X = rf2_gest,
    FUN = function(i) rf2[[i]] %>% iplot() %$% prms %>% as.data.table()
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  est_rf3_pctl = lapply(
    X = 1:length(rf3),
    FUN = function(i) rf3[[i]] %>% iplot() %$% prms %>% as.data.table()
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  est_rf3b_pctl = lapply(
    X = 1:length(rf3b),
    FUN = function(i) rf3b[[i]] %>% iplot() %$% prms %>% as.data.table()
  ) %>% rbindlist(use.names = TRUE, fill = TRUE, idcol = 'spec')
  # Combine
  rf1_dt = list(
    'dbwt_pctl' = est_rf1_pctl,
    'dbwt' = est_rf1_dbwt,
    'gest' = est_rf1_gest,
    NULL
  ) %>% rbindlist(idcol = 'outcome')
  rf2_dt = list(
    'dbwt_pctl' = est_rf2_pctl,
    'dbwt' = est_rf2_dbwt,
    'gest' = est_rf2_gest,
    NULL
  ) %>% rbindlist(idcol = 'outcome')
  rf3_dt = est_rf3_pctl
  rf3_dt[, outcome := 'dbwt_pctl']
  rf3b_dt = est_rf3b_pctl
  rf3b_dt[, outcome := 'dbwt_pctl']
  # Save
  write_fst(
    x = rf1_dt,
    path = here(
      'presentations', '04-rubin-ucsb', 'data', 'results',
      'rf-e100myielddiffpercentilegmo.fst'
    )
  )
  write_fst(
    x = rf2_dt,
    path = here(
      'presentations', '04-rubin-ucsb', 'data', 'results',
      'rf-allyielddiffpercentilegmo.fst'
    )
  )
  write_fst(
    x = rf3_dt,
    path = here(
      'presentations', '04-rubin-ucsb', 'data', 'results',
      'rf-dbwtpctl-allyielddiffpercentilegmo.fst'
    )
  )
  write_fst(
    x = rf3b_dt,
    path = here(
      'presentations', '04-rubin-ucsb', 'data', 'results',
      'rf-dbwtpctl-allyielddiffpercentilegmo-clstate.fst'
    )
  )


# 2SLS results ---------------------------------------------------------------------------
  # Load packages 
  pacman::p_load(fastverse, fixest, qs, here)
  fastverse_extend(topics = 'ST')
  # Load data
  est1 = here(
    'data-clean', 'results', '20221220',
    'est_2sls_outcome-dbwt-gestation_fe-03_controls-03_spatial-rural_het-_iv-allyielddiffpercentilegmo.qs'
  ) %>% qread()
  est2 = here(
    'data-clean', 'results', '20221220',
    'est_2sls_outcome-dbwt-dbwtpctlruralpre-gestation_fe-03_controls-03_spatial-rural_het-_iv-e100myielddiffpercentilegmo.qs'
  ) %>% qread()
  est3 = here(
    'data-clean', 'results', '20230109',
    'est_2sls_outcome-dbwtpctlruralpre_fe-03_controls-03_spatial-rural_het-_iv-allyielddiffpercentilegmo_cl-yearfips.qs'
  ) %>% qread()
  # Find indices of outcomes
  i1_dbwt = est1 %>% names() %>% str_detect('dbwt') %>% which()
  i1_gest = est1 %>% names() %>% str_detect('gestation') %>% which()
  i2_pctl = est2 %>% names() %>% str_detect('dbwt_pctl') %>% which()
  i2_dbwt = est2 %>% names() %>% str_detect('dbwt') %>% which() %>% setdiff(i2_pctl)
  i2_gest = est2 %>% names() %>% str_detect('gestation') %>% which()
  # Build results tables
  # Birth weight:
  list(est1[i1_dbwt], est2[i2_dbwt[4]]) %>% etable()
  # Birth weight percentile
  list(est3, est2[i2_pctl[4]]) %>% etable()
  # Gestation length:
  list(est1[i1_gest], est2[i2_gest[4]]) %>% etable()
# Heterogeneity ---------------------------------------------------------------
  # Load data
  est = here(
    'data-clean', 'results', '20221220',
    'est_2sls_outcome-dbwt-gestation_fe-03_controls-0_spatial-rural_het-predq5_iv-allyielddiffpercentilegmo.qs'
  ) %>% qread()
  # Find indices of outcomes
  dbwt = est %>% names() %>% str_detect('dbwt') %>% which()
  gest = est %>% names() %>% str_detect('gestation') %>% which()
  # Birth weight:
  est[dbwt[seq(2, 12, 2)]] %>% etable()
  # Gestation length:
  etable(
    est[gest[seq(2, 12, 2)]],
    extralines = list(
      '_Implied effect at mean (d)' = c(-1.23,-2.32,-1.70,-1.37,-1.16,-0.77),
      '_2012 gestation mean (d)' = c(271.6,270.0,272.2,272.6,272.4,272.5)
    ),
    tex = TRUE
  )


# Figures for the NATURE draft
library(pacman)
p_load(
  here, data.table, fst, ggplot2, qs
)

# Define pink color
pink       = '#e64173'
red_pink   = '#e64173'
turquoise  = '#20B2AA'
orange     = '#FFA500'
red        = '#fb6107'
blue       = '#648FFF'
dark_blue  = '#3b3b9a'
green      = '#8bb174'
grey_light = 'grey70'
grey_mid   = 'grey50'
grey_dark  = 'grey20'
purple     = '#6A5ACD'
slate      = '#314f4f'
# Setting height and width 
width_in = 6
height_in = 3

# First loading data ----------------------------------------------------------
  # Load county-level first-stage estimates
  fs_dt = 
    read_fst(
      here('presentations/04-rubin-ucsb/data/results/fs-event-study-co.fst'),
      as.data.table = TRUE
    )[x >= 1992] # Limit to 1992
  # Loading reduced form results 
  rf2_dt = here(
    'presentations/04-rubin-ucsb/data/results',
    'rf-allyielddiffpercentilegmo.fst'
  ) |> read_fst(as.data.table = TRUE)

# Plotting first stage --------------------------------------------------------
  fs_event_p = 
    ggplot(
      data = fs_dt[spec == 'fs_all_gmo_controls' & x <= 2013],
      aes(x = x, y = y, ymin = ci_low, ymax = ci_high)
    ) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    geom_hline(yintercept = 0) +
    # Current
    geom_ribbon(fill = pink, alpha = 0.5) +
    geom_point(color = pink, size = 2.5) +
    geom_path(size = 0.3, color = pink) +
    #scale_y_continuous(latex2exp::TeX('$\\pi_{t}$')) +
    scale_y_continuous(latex2exp::TeX('$GLY/KM^2$')) +
    scale_x_continuous('Year', breaks = seq(1990, 2015, 5), minor_breaks = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x = element_blank(),
    )
  fs_event_p
  ggsave(
    fs_event_p, 
    filename = here('figures/nature-draft/fs-event-main.pdf'),
    device = cairo_pdf, 
    width = width_in, height = height_in
  )

# Plotting reduced form -------------------------------------------------------
  rf_event_p = 
    ggplot(
      data = rf2_dt[spec == 3 & outcome == 'dbwt'],
      aes(x = x, y = y, ymin = ci_low, ymax = ci_high)
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    # Current plot
    geom_ribbon(fill = pink, alpha = 0.5) +
    geom_point(color = pink, size = 2.5) +
    geom_path(size = 0.3, color = pink) +
    # General figure stuff
    scale_y_continuous('Birth weight (g)', breaks = seq(0, -120, -20)) +
    scale_x_continuous('Year', breaks = seq(1990, 2015, 5), minor_breaks = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      # panel.grid.minor.x = element_blank(),
      # panel.grid.minor.y = element_blank(),
      # panel.grid.major.y = element_blank(),
      axis.title.x = element_blank(),
    )
  rf_event_p
  ggsave(
    rf_event_p, 
    filename = here('figures/nature-draft/rf-event-main.pdf'),
    device = cairo_pdf, 
    width = width_in, height = height_in
  )


# Now upstream results --------------------------------------------------------  
  # Function to clean the model output
  clean_results = function(sample_in, rhs_in, model){
    model[sample = sample_in, rhs = rhs_in] |>
      broom::tidy() |>
      mutate(
        year = str_extract(term, "(?<=year::)\\d{4}(?=:)") |>
          as.numeric(),
        trt = str_extract(term, '(all|e100m)_yield_diff_.*_\\d*_\\d{1,2}'),
        geo = str_extract(trt, 'all|e100m'),
        crop = str_extract(trt, '(?<=yield_diff_)\\w{3}(?=_)'),
        split = str_extract(trt, '(?<=yield_diff_\\w{3}_)\\d{2}(?=_)'),
        buffer = str_extract(trt, '\\d{1,2}$'),
        controls = rhs_in == 3,
        water = rhs_in %in% 2:3,
        month = sample_in-1,
        local = str_detect(term, "high_kls", negate = TRUE),
        distance = str_extract(term, '(?<=d).{1,4}$') |>
          str_replace('n','-') |>
          as.numeric(),
        distance = ifelse(is.na(distance), 0, distance),
        distance = ifelse(distance == -Inf, -100, distance + 50),
        ci_l = estimate + std.error*qnorm(0.025),
        ci_h = estimate + std.error*qnorm(0.975)
      ) |>
      select(
        geo, crop, split, buffer, controls, water, month, 
        year, distance, local, 
        estimate, ci_l, ci_h
      ) |> 
      filter(!is.na(crop)) |>
      data.table()

  }
  # Function to get all results from model 
  all_results = function(model_in){
    pmap_dfr(
      tibble(
        sample_in = rep(1:13, each = 3),
        rhs_in = rep(1:3, 13)
      ),
      clean_results,
      model = model_in
    ) 
  }
  # Reading the reduced form results
  est_rf_water_e100m_yield_diff_gmo_50_0 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_e100m_yield_diff_gmo_50_0.qs"
  ))
  est_rf_water_e100m_yield_diff_gmo_40_0 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_e100m_yield_diff_gmo_40_0.qs"
  ))
  est_rf_water_e100m_yield_diff_gmo_60_0 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_e100m_yield_diff_gmo_60_0.qs"
  ))
  est_rf_water_e100m_yield_diff_gmo_50_10 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_e100m_yield_diff_gmo_50_10.qs"
  ))
  est_rf_water_e100m_yield_diff_soy_50_0 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_e100m_yield_diff_soy_50_0.qs"
  ))
  est_rf_water_e100m_yield_diff_soy_40_0 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_e100m_yield_diff_soy_40_0.qs"
  ))
  est_rf_water_e100m_yield_diff_soy_60_0 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_e100m_yield_diff_soy_60_0.qs"
  ))
  est_rf_water_e100m_yield_diff_soy_50_10 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_e100m_yield_diff_soy_50_10.qs"
  ))
  est_rf_water_all_yield_diff_gmo_50_0 = qread(here(
    "data-clean/results/20220831/est_rf_water_rural_all_yield_diff_gmo_50_0.qs"
  ))

  # Getting results for all months/rhs
  p_load(purrr, dplyr, stringr)
  all_results_dt = 
    map_dfr(
      list(
        est_rf_water_e100m_yield_diff_gmo_40_0,
        est_rf_water_e100m_yield_diff_gmo_50_0,
        est_rf_water_e100m_yield_diff_gmo_60_0,
        est_rf_water_e100m_yield_diff_gmo_50_10,
        est_rf_water_e100m_yield_diff_soy_50_0,
        est_rf_water_e100m_yield_diff_soy_40_0,
        est_rf_water_e100m_yield_diff_soy_60_0,
        est_rf_water_e100m_yield_diff_soy_50_10,
        est_rf_water_all_yield_diff_gmo_50_0
      ),
      all_results
    )
  # Adding baseline estimtes 
  all_results_dt = 
    rbind(
      all_results_dt[year != 1995],
      unique(all_results_dt[,.(
        geo, crop, split, buffer, controls, water, month, 
        distance, local, 
        year = 1995,
        estimate = 0, 
        ci_l= 0, ci_h = 0
      )]),
      use.names = TRUE
    )

  rf_event_study_gmo_distance_p = 
    all_results_dt[
      geo == 'all' &
      local == FALSE & 
      distance > 50 &
        crop == 'gmo' & 
        split == 50 & 
        buffer == 0 &
        month == 0 &
        controls == TRUE & 
        water == TRUE
    ] |>
    ggplot(aes(
      x = year, y = estimate , ymin = ci_l, ymax = ci_h)
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 1995.5, col = 'black', size = 1, alpha = 1, linetype = 'dashed') +
    # Current plot
    geom_ribbon(fill = pink, alpha = 0.5) +
    geom_point(color = pink, size = 2.5) +
    geom_line(size = 0.3, color = pink) +
    # General figure stuff
    scale_y_continuous('Birth weight (g)') +
    scale_x_continuous('Year', breaks = seq(1990, 2015, 5), minor_breaks = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      # panel.grid.minor.x = element_blank(),
      # panel.grid.minor.y = element_blank(),
      # panel.grid.major.y = element_blank(),
      axis.title.x = element_blank(),
    ) + 
    #scale_color_viridis_c(name = 'Distance Bin')+
    facet_wrap(~paste0('Distance Bin: ',distance), scales = 'free') + 
    labs(
  #    title = 'Effect of upstream CSC on birth weight',
      x = "Year",
      y = 'Effect on Birth Weight (g)'
    ) +
    #scale_x_continuous(breaks = c(1992,1995,2000,2004))+
    theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))
  rf_event_study_gmo_distance_p
  ggsave(
    plot = rf_event_study_gmo_distance_p,
    filename = here('figures/nature-draft/rf-event-distance-gmo.pdf'),
    width = 10, height = 5,
    bg = 'white',
    device = cairo_pdf
  )


# EXTENDED DATA FIGURES -------------------------------------------------------
  # Load data: Prediction analyses
  het_dt = here(
    'presentations/04-rubin-ucsb/data', 
    'prediction-summaries/pctl-rural.fst'
  ) |> read_fst(as.data.table = TRUE)
  het_dt[,nonmarried_pct := 1-married_pct]

  # Demographics of predictions 
  bw_pred_demog_p = 
    melt(
      het_dt[!is.na(dbwt_pred_rnd_pctl_rural_pre)],
      id.vars = c('dbwt_pred_rnd_pctl_rural_pre'),
      measure.vars = c('female_pct','m_pct_black','nonmarried_pct')
    ) |>
    ggplot(
      aes(x = dbwt_pred_rnd_pctl_rural_pre, y = value, color = variable)
    ) +
    geom_hline(yintercept = 0, size = 0.25) +
    annotate(
      geom = 'segment',
      x = seq(0, 1, 0.2),
      xend = seq(0, 1, 0.2),
      y = rep(-0.02, 6),
      yend = rep(0.02, 6),
      size = 0.25
    ) +
    geom_line(size = 1) +
    scale_color_manual(
      labels = c('Female', 'Black','Unmarried'),
      values = c(orange, pink, purple),
      name = ''
    ) + 
    # Plot stuff
    scale_x_continuous('Percentile of predicted birth-weight', labels = scales::label_percent(), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous('Percent', labels = scales::label_percent()) +
    theme_minimal(base_family = 'Fira Sans Extra Condensed', base_size = 18) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank()
    ) 
    bw_pred_demog_p
    ggsave(
      bw_pred_demog_p, 
      filename = here('figures/nature-draft/bw-pred-demog.pdf'),
      device = cairo_pdf, 
      width = width_in*1.5, height = height_in*1.25
    )

  # Prediction accuracy for RF model 
  bw_pred_accuracy_p = 
    ggplot(
      data = het_dt[!is.na(dbwt_pred_rnd_pctl_rural_pre)],
      aes(x = dbwt_pred_rnd_pctl_rural_pre)
    ) +
    geom_line(aes(y = dbwt_mean), color = slate, size = 1.25) +
    geom_line(aes(y = dbwt_pred_mean), color = pink, size = 1.5) +
    # Plot stuff
    scale_x_continuous('Percentile of predicted birth-weight', labels = scales::label_percent()) +
    scale_y_continuous('Birth weight (g)', labels = scales::label_comma()) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
    bw_pred_accuracy_p
    ggsave(
      bw_pred_accuracy_p, 
      filename = here('figures/nature-draft/bw-pred-accuracy.pdf'),
      device = cairo_pdf, 
      width = width_in, height = height_in
    )
