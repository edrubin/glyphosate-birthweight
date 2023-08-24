
# This script builds the data required to fit model predicting glyph 
# concentrations in water samples 
library(pacman)
p_load(
  here, fst, data.table, janitor, lubridate, stringr, collapse,
  sf, skimr
)

# Loading the USGS water sampling data 
site_dt = fread(here(
  "data/pesticides/usgs-monitoring/Table1. NWQN.SiteInformation.txt"
)) |> clean_names()
sample_dt = fread(here(
  "data/pesticides/usgs-monitoring/Table2. Glyphosate.AMPA.SampleConc.txt"
)) |> clean_names()
# Loading the watershed level pesticide data
watershed_pesticide_dt =  
  read.fst(
    path = here("data-clean/watershed/watershed-pesticide-dt.fst"),
    as.data.table = TRUE
  )[,.( 
    # Lots of columns in this table, so limiting to the ones we're interested in
    hybas_id, year, e100m, hybas_area_km2,
    glyphosate_awt, 
    glyph_km2_awt, 
    pred_glyphosate_awt, 
    pred_glyph_km2_awt, 
    #yield_diff_cot, 
    #yield_diff_mze, 
    #yield_diff_soy,
    all_yield_diff_percentile_cot,
    all_yield_diff_percentile_gmo,
    all_yield_diff_percentile_gmo_max,
    all_yield_diff_percentile_mze,                                                      
    all_yield_diff_percentile_soy,
    corn_acres_awt,
    cotton_acres_awt,                                      
    other_acres_awt,                                       
    soy_acres_awt,                                         
    gm_acres_awt, 
    percentile_corn_acres,
    percentile_cotton_acres,                                 
    percentile_other_acres,                                  
    percentile_soy_acres,                                    
    percentile_gm_acres,
    pct_irrigated_corn = fcoalesce(pct_irrigated_corn, 0), 
    pct_irrigated_soy = fcoalesce(pct_irrigated_soy, 0), 
    pct_irrigated_cotton = fcoalesce(pct_irrigated_cotton, 0), 
    pct_irrigated_gm = fcoalesce(pct_irrigated_gm, 0), 
    ppt_off_season, 
    ppt_growing_season,
    kffact, 
    slopelen = fcoalesce(slopelen, 0),
    kls
  )] |> 
  setkey(hybas_id, year)
  # Loading the national glyphosate data 
  #watershed_shift_share_dt = 
  #  read.fst(
  #    here('data/glyph-nat-watershed-dt.fst'),
  #    as.data.table = TRUE
  #  )[,.(hybas_id, year, glyphosate_nat_100km)]
    
# Loading Upstream watersheds
upstream_dt = 
  read.fst(
    path = here("data-clean/watershed/upstream-dt-hydrobasin.fst"), 
    as.data.table = TRUE
  )[ # Filtering to just upstream. Picking out relevant columns
    dist_km > 0 & local == FALSE,.(
    hybas_id, hybas_id2, dist_km, dist_km_bin
  )] |> 
  setkey(hybas_id2)
# Loading HydroBASINS data 
hydrobasin_sf = 
  read_sf(here("data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
  st_transform(crs = 2163) |>
  clean_names()

# -------------------------------------------------------------------------
# Choosing the distance bins to aggregate over 
dist_breaks = c(0, 50, 100, 150, 200, 250, 300, Inf)
# Creating distance bin variable 
upstream_dt[, dist_km_bin := cut(
  dist_km, 
  breaks = dist_breaks, 
  right = FALSE,
  labels = paste0('d',dist_breaks[1:length(dist_breaks)-1]) |> str_replace('-','n')
)]
# Merging upstream watersheds and grouping 
all_pesticide_upstream_dt =   
  merge(
    upstream_dt, 
    watershed_pesticide_dt,
    by.x = "hybas_id2",
    by.y = "hybas_id",
    allow.cartesian = TRUE
  ) |>
  fgroup_by(hybas_id, year, dist_km_bin)
# Aggregating: Summing and taking weighted mean  
pesticide_upstream_dt = 
  add_vars(
    all_pesticide_upstream_dt |>
      fselect(
        glyphosate_awt,pred_glyphosate_awt,
        gm_acres_awt, corn_acres_awt, soy_acres_awt, cotton_acres_awt,
        ppt_off_season, ppt_growing_season
      )|>
      fsum(),
    all_pesticide_upstream_dt |>
      fselect(
        glyph_km2_awt, pred_glyph_km2_awt, 
        #yield_diff_cot, yield_diff_mze, yield_diff_soy,
        pct_irrigated_corn, pct_irrigated_soy, pct_irrigated_cotton, pct_irrigated_gm,
        all_yield_diff_percentile_cot,
        all_yield_diff_percentile_gmo,
        all_yield_diff_percentile_gmo_max,
        all_yield_diff_percentile_mze, 
        all_yield_diff_percentile_soy,
        percentile_corn_acres,
        percentile_cotton_acres,
        percentile_other_acres,
        percentile_soy_acres,
        percentile_gm_acres,
        kffact, slopelen, hybas_area_km2
      ) |>
      fmean(w = hybas_area_km2) |> fselect(-(1:4))
  ) |>
  melt(
    id.vars = c('hybas_id','year','dist_km_bin')
  ) |>
  dcast(
    formula = hybas_id + year ~ variable + dist_km_bin,
    value.var = 'value',
    fill = 0
  )
# Adding national glyphosate
#pesticide_upstream_dt = 
#  merge(
#    pesticide_upstream_dt[,-'glyphosate_nat_100km'], 
#    watershed_shift_share_dt, 
#    by = c('hybas_id','year')
#  )
# Lagging variables 
#pesticide_upstream_dt = 
#  cbind(
#    pesticide_upstream_dt |> 
#      get_vars(vars = 'L1', regex = TRUE, invert = TRUE),
#    pesticide_upstream_dt |>
#      fgroup_by(hybas_id) |>
#      flag(t = year, stubs = TRUE) |>
#      get_vars(vars = 'pct_irrigated|glyph|ppt', regex = TRUE) |>
#      fungroup()
#  )
# Saving result to be used later
write.fst(
  pesticide_upstream_dt, 
  path = here('data-clean/ml-water/pesticide-upstream-dt.fst')
)

# -------------------------------------------------------------------------
# Merging sample to watershed data
site_sf = st_as_sf(site_dt, coords = c("longitude","latitude")) |>
  st_set_crs("NAD83") |>
  st_transform(crs = 2163)
# Creating a monitor-hydroshed crosswalk
site_watershed_dt = 
  st_join(site_sf,hydrobasin_sf) |>
  as.data.table() %>% 
  .[,.(site_no, hybas_id, region)]
# Merging sample data to watershed data
sample_watershed_dt = 
  merge(
    sample_dt[!is.na(site_no), .(
      site_no,
      sample_date = dmy(sample_start_date),
      quarter = quarter(dmy(sample_start_date)),
      month = month(dmy(sample_start_date), label = TRUE),
      year = ifelse(
        quarter(dmy(sample_start_date)) == 1, 
        year(dmy(sample_start_date)) - 1,
        year(dmy(sample_start_date))
      ),
      gly_result = ifelse(gly_rem == '<',0, gly_result), 
      ampa_result = ifelse(ampa_rem == '<',0, ampa_result)
    )],
    site_watershed_dt, 
    by = "site_no"
  ) |>
  merge(
    pesticide_upstream_dt,
    by = c("hybas_id","year")
  ) 
# -------------------------------------------------------------------------
# Saving the result 
write.fst(
  sample_watershed_dt, 
  path = here('data-clean/ml-water/sample-watershed-dt.fst')
)

skimr::skim(sample_watershed_dt)
