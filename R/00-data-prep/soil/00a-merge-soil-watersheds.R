
# Notes -----------------------------------------------------------------------
#   Goal:   Merge ID raster with HUC-08 watershed shapefile.
#   Time:   Approximately 30 minutes.


# Setup -----------------------------------------------------------------------
  # Load packages
  library(pacman)
  p_load(fastverse, exactextractr, magrittr, here)
  fastverse_extend(topics = c('SP'))


# Load data -------------------------------------------------------------------
  # Load the soil-data raster of cell IDs
  id_r = here(
    'data', 'spatial', 'soil-quality', 'gNATSGO_mukey_grid', 'gNATSGO-mukey.tif'
  ) %>% terra::rast()
  # Load the HUC-8-level watershed shapefile
  water_sf = here(
    'data', 'spatial', 'hydrobasins', 'hybas_lake_na_lev08_v1c.shp'
  ) %>% st_read(
    query = 'SELECT HYBAS_ID FROM "hybas_lake_na_lev08_v1c"'
  )


# Merge -----------------------------------------------------------------------
  # Transform watershed CRS to match cell raster
  water_sf %<>% st_transform(crs = st_crs(id_r))
  # Match cells to water 
# NOTE: There will be empty 'matches' as the watersheds hit CA and MX 
  matched_list = exact_extract(
    id_r,
    water_sf,
    fun = function(values, coverage_area) collapse::funique(values), 
    max_cells_in_memory = ncell(id_r),
    progress = FALSE
  )


# Build crosswalk and save ----------------------------------------------------
  # Build crosswalk
  xwalk_dt = lapply(
    X = seq_along(matched_list),
    FUN = function(i) {
      data.table(
        HYBAS_ID = water_sf[i,]$'HYBAS_ID',
        mukey = matched_list[[i]]
      )
    }
  ) %>% rbindlist(use.names = TRUE, fill = TRUE)
  # Save the crosswalk
  write_fst(
    x = xwalk_dt,
    path = here('data-clean', 'xwalk-soil-watershed.fst'),
    compress = 100
  )
  # Clean up
  rm(matched_list, xwalk_dt)
  invisible(gc())


# Load the crosswalk ----------------------------------------------------------
  # Load the crosswalk
  xwalk_dt = read_fst(
    path = here('data-clean', 'xwalk-soil-watershed.fst'),
    as.data.table = TRUE
  )
  # Drop missing mukeys (generally from watersheds outside of CONUS)
  xwalk_dt %<>% .[!is.na(mukey)]


# Load soil data --------------------------------------------------------------
  # Load 'chorizon' file
  ch_dt = here(
    'data', 'spatial', 'soil-quality', 'gNATSGO_Tabular_CSV', 'chorizon.csv'
  ) %>% vroom::vroom(col_select = c('cokey', 'kwfact', 'kffact'))
  # Load 'component' file
  comp_dt = here(
    'data', 'spatial', 'soil-quality', 'gNATSGO_Tabular_CSV', 'component.csv'
  ) %>% vroom::vroom(
    col_select = c('mukey', 'cokey', 'comppct_r')
  )
  # Convert both to data tables and key
  setDT(ch_dt)
  setDT(comp_dt)


# Merge the datasets ----------------------------------------------------------
  # Take minimum of the horizon factors by component (least transmissible)
  setkey(ch_dt, cokey)
  ch_dt %<>% .[, .(
    kwfact = fmin(kwfact),
    kffact = fmin(kffact)
  ), cokey]
  # Fill in missing component percents (happens only for single-row obs.)
  comp_dt[is.na(comppct_r), comppct_r := 100]
  # Merge component and horizon datasets
  setkey(comp_dt, cokey)
  setkey(ch_dt, cokey)
  soil_dt = merge(
    x = comp_dt,
    y = ch_dt,
    by = 'cokey',
    allow.cartesian = TRUE
  )
  # Aggregate to map keys, weighting factors by components' percentages
  soil_dt %<>% .[, .(
    kwfact = fmean(kwfact, w = comppct_r),
    kffact = fmean(kffact, w = comppct_r)
  ), by = mukey]
  # Merge soil data to the xwalk
  setkey(xwalk_dt, mukey)
  setkey(soil_dt, mukey)
  hy_dt = merge(
    x = xwalk_dt,
    y = soil_dt,
    by = 'mukey',
    allow.cartesian = TRUE
  )
  # Aggreate to HUC-8
  hy_dt %<>% .[, .(
    kwfact = fmean(kwfact),
    kffact = fmean(kffact)
  ), by = HYBAS_ID]


# Save results ----------------------------------------------------------------
  # Save the table
  fwrite(
    x = hy_dt,
    file = here('data', 'spatial', 'soil-quality', 'watershed-soil-factors.csv')
  )
  # Save as shapefile
  setDT(water_sf)
  water_sf %<>% merge(hy_dt, by = 'HYBAS_ID')
  water_sf %<>% st_as_sf()
  st_write(
    water_sf,
    here('data', 'spatial', 'soil-quality', 'huc8-soil-shp', 'huc8-soil-shp.shp')
  )



