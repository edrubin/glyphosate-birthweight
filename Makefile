# -----------------------------------------------------------------------------
# Options and variables
R_OPTS=--vanilla
crop-names := acre yield irrigated
crop-dt := $(crop-names:%=data/crops/all-crop-%-dt.fst)
crop-cnty := $(crop-names:%=data/crops/all_crop_%_county.fst)
# For watershed data
mods := lasso-glyph rf-glyph lasso-ampa rf-ampa
water-mods := $(mods:%=data-clean/ml-water/final-fit-%.qs)
prism-times := qtr season year
prism-fp :=  $(prism-times:%=data-clean/watershed/ppt-%-wshd-dt.fst)
pop-wt-yrs := 1990 2000 2010
pop-wts := $(pop-wt-yrs:%=data-clean/watershed/weights/hydrobasin-pop-weights%.fst)
pop-grids := $(pop-wt-yrs:%=data/spatial/us-pop-grid/uspop%.tif)

# -----------------------------------------------------------------------------
# Setting up higher level targets
all: data-download data-raw data-clean water
# Downloading data from web or API
data-download: \
 data/pesticides/est_pest_use.csv \
 data/pesticides/est_pest_use.fst \
 data/crops/census_crop_acre_county.fst \
 data/pop-area-empl/cnty-area-dt.fst \
 data/pop-area-empl/cnty-pop-dt.fst \
 data/pop-area-empl/farm-empl-dt.fst \
 $(crop-cnty) 
# Raw data we won't touch very often
data-raw: \
 data/pop-area-empl/edu-dt.fst \
 data/bls-labforce/labor-dt.fst \
 $(crop-dt)
# Intermediate files that may get updated often
data-clean: \
 data-clean/health-dt.fst \
 data-clean/comb-cnty-dt.fst \
 data-clean/fs-dt.fst \
 data/crops/crop-acre-percentile-90-95.fst \
 data-clean/trt-dt.fst
# Watershed modeling 
water: \
 data-clean/watershed/county-exposure-dt.fst \
 data-clean/watershed/county-exposure-pred-dt.fst
# Predicting birthweights
# predict-bw: 

# -----------------------------------------------------------------------------
# Targets for predict-bw


# -----------------------------------------------------------------------------
# Targets for data-clean

# Micro natality clean 
data-clean/natality-micro.fst: \
 R/04-analysis/00-build-natality-twfe.R \
 $(wildcard data/health-restricted/period-clean/natality-*.fst)
	Rscript $<
	@echo "Built natality micro data"

# County level health data 
data-clean/health-dt.fst: \
 R/03-combine/01-comb-county-health.R \
 data-clean/comb-cnty-dt.fst data/glyph-nat-dt.fst
	Rscript $<
	@echo "Made health-dt"

# Combining all county data into one table
data-clean/comb-cnty-dt.fst: \
 R/03-combine/00-combine-cnty.R \
 data-clean/trt-dt.fst \
 data-clean/fs-dt.fst
	Rscript $<
	@echo "Made comb-cnty-dt"
# Other dependencies: 
# data-clean/watershed/county-exposure-dt.fst 
# data/pop-area-empl/cnty-area-dt.fst 
# data/pop-area-empl/cnty-pop-dt.fst 
# data/pop-area-empl/farm-empl-dt.fst 
# data/pop-area-empl/ruralurbancodes2003.xls
# data/pesticides/est_pest_use.fst 
# data-clean/fs-dt.fst 
# $(crop-dt)
# data/pop-area-empl/edu-dt.fst 
# data/bls-labforce/labor-dt.fst 

# First stage predictions
data-clean/fs-dt.fst: \
 R/01-define-treatment/01-first-stage.R \
 data/crops/crop-acre-percentile-90-95.fst \
 data/glyph-nat-dt.fst
	Rscript $<
	@echo "Made first stage predictions"
# Other dependencies: 
# data/pop-area-empl/cnty-area-dt.fst 
# data/pesticides/est_pest_use.fst

# Shift share instruments 
data/glyph-nat-dt.fst \
 data/glyph-nat-watershed-dt.fst &: \
 R/01-define-treatment/00c-glyph-national.R
	Rscript $<
	@echo "Made shift share instruments"
# Other dependencies: 
# data-clean/watershed/weights/hydrobasin-area-weights.fst 
# data-clean/watershed/upstream-dt-hydrobasin.fst 
# data/pesticides/est_pest_use.fst

# Pre period acreage percentiles
data/crops/crop-acre-percentile-90-95.fst: \
 R/01-define-treatment/00b-crop-acre-percentiles.R
	Rscript $<
	@echo "Made crop acreage percentiles"
# Other dependencies: 
# data/pop-area-empl/cnty-area-dt.fst 
# data/crops/all-crop-acre-dt.fst

# Treatment definitions
data-clean/trt-dt.fst: R/01-define-treatment/00-define-treatment.R
	Rscript $<
	@echo "Made treatment definitions"
# Other dependencies data/gaez-suitability-index/y_diff_dt.fst

# -----------------------------------------------------------------------------
# Targets for data-raw

# GAEZ suitability data
data/gaez-suitability-index/hydrobasin-y-diff-dt.fst \
 data/gaez-suitability-index/y_diff_dt.fst &: \
 R/00-data-prep/farm/04-gaez-yield.R 
	Rscript $<
	@echo "Made suitability data"
# Other dependencies: 
# $(wildcard data/gaez-suitability-index/attainable-yield/yl*r_*.tif)

# Education data
data/pop-area-empl/edu-dt.fst: R/00-data-prep/controls/03-education.R 
	Rscript $<
	@echo "Made education data"
# Other dependencies data/pop-area-empl/Education.xls

# BLS labor data
data/bls-labforce/labor-dt.fst: R/00-data-prep/controls/02-bls-labor.R
	Rscript $<
	@echo "Made bls labor data"

# Crop data
$(crop-dt) &: R/00-data-prep/farm/03-crop-county-cleaning.R 
	Rscript $<
	@echo "Made crop data"
# Other dependencies: $(crop-cnty)

# -----------------------------------------------------------------------------
# Targets for data-download

# Pesticide data
data/pesticides/est_pest_use.csv \
 data/pesticides/est_pest_use.fst &: \
 R/00-data-prep/farm/01-usgs-chem-data.R
	Rscript $<
	@echo "Downloaded pesticide data"

# USDA NASS data
$(crop-cnty) \
 data/crops/census_crop_acre_county.fst &: \
 R/00-data-prep/farm/02-usda-nass-data.R
	Rscript $<
	@echo "Downloaded USDA NASS"

# County area 
data/pop-area-empl/cnty-area-dt.fst \
 data/pop-area-empl/cnty-pop-dt.fst &: \
 R/00-data-prep/controls/00-cnty-pop-area.R
	Rscript $<
	@echo "Calculated county area and pop"

# BEA employment
data/pop-area-empl/farm-empl-dt.fst: R/00-data-prep/controls/01-bea-empl.R
	Rscript $<
	@echo "Downloaded BEA data"


# -----------------------------------------------------------------------------
# Targets for water

# Aggregating to county
data-clean/watershed/county-exposure-dt.fst \
 data-clean/watershed/county-exposure-pred-dt.fst &: \
 R/02-water-ml/04-watershed-to-county.R \
 data-clean/ml-water/predictions-watershed.fst \
 data-clean/watershed/pesticide-upstream-dt.fst \
 data-clean/watershed/watershed-pesticide-dt.fst \
 data-clean/watershed/weights/hydrobasin-pop-weights2010.fst
	Rscript $<
	@echo "Aggregated to county level" 

# Making predictions for all watersheds/years
data-clean/ml-water/predictions-watershed.fst: \
 R/02-water-ml/03-predictions.R data-clean/ml-water/sample-watershed-dt.fst \
 data-clean/ml-water/pesticide-upstream-dt.fst \
 $(water-mods)
	Rscript $<
	@echo "Made watershed predictions" 

# Training models and updating perfromance plots
$(water-mods) &: \
 R/02-water-ml/02a-train-models.R \
 data-clean/ml-water/sample-watershed-dt.fst
	Rscript $<
	Rscript R/02-water-ml/02b-model-performance.R
	@echo "Trained water ML models"

# Cleaning data for water ML
data-clean/ml-water/sample-watershed-dt.fst \
 data-clean/ml-water/pesticide-upstream-dt.fst &: \
 R/02-water-ml/01-data-prep.R \
 data-clean/watershed/upstream-dt-hydrobasin.fst \
 data-clean/watershed/watershed-pesticide-dt.fst
	Rscript $<
	@echo "Prepped water ml data"
# Other dependencies: 
# data/pesticides/usgs-monitoring/Table1. NWQN.SiteInformation.txt 
# data/pesticides/usgs-monitoring/Table2. Glyphosate.AMPA.SampleConc.txt 
# data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp

# Aggregating upstream and downstream
data-clean/watershed/pesticide-upstream-dt.fst \
 data-clean/watershed/watershed-pesticide-dt.fst &: \
 R/02-water-ml/00-watershed-upstream-aggregation.R \
 data-clean/watershed/upstream-dt-hydrobasin.fst \
 data-clean/watershed/usle-dt.fst \
 data/crops/all-crop-irrigated-dt.fst \
 data-clean/trt-dt.fst data-clean/fs-dt.fst \
 data-clean/watershed/weights/hydrobasin-area-weights.fst
	Rscript $<
	@echo "Aggregated upstream/downstream"
# Other dependencies: data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp

# Finding watersheds upstream and downstream
data-clean/watershed/upstream-dt-hydrobasin.fst: \
 R/00-data-prep/watershed/04-watershed-find-upstream.R \
 data-clean/watershed/weights/hydrobasin-area-weights.fst \
 data-clean/watershed/hybas-dist-dt.fst
	Rscript $<
	@echo "Found watersheds upstream/downstream"
# Other dependencies: data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp

# Calculating distanct to next downstream
data-clean/watershed/hybas-dist-dt.fst: \
 R/00-data-prep/watershed/03-next-down-dists.R 
	Rscript $<
	@echo "Calculated dist to next downstream"
# Other dependencies: data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp 

# Area weights
data-clean/watershed/weights/hydrobasin-area-weights.fst: \
 R/00-data-prep/watershed/02-area-weights-hydroshed.R
	Rscript $<
	@echo "Calculated area weights"

# Population weights 
$(pop-wts) &: \
 R/00-data-prep/watershed/01-pop-weights-hydroshed.R \
 $(pop-grids)
	Rscript $<
	@echo "Calculated population weights"

# Soil data 
data-clean/watershed/usle-dt.fst: \
 R/00-data-prep/soil/01-combine-soil-precip.R \
 data/spatial/soil-quality/watershed-soil-factors.csv \
 data-clean/watershed/ppt-season-wshd-dt.fst
	Rscript $<
	@echo "Cleaned soil data"

# Raw soil data 
data/spatial/soil-quality/watershed-soil-factors.csv: \
 R/00-data-prep/soil/00a-merge-soil-watersheds.R \
 data/spatial/soil-quality/gNATSGO_mukey_grid/gNATSGO-mukey.tif \
 data/spatial/soil-quality/gNATSGO_Tabular_CSV/chorizon.csv \
 data/spatial/soil-quality/gNATSGO_Tabular_CSV/component.csv
	Rscript $<
	@echo "Merged soil data"

# Aggregating precipitation data 
$(prism-fp) &: R/00-data-prep/controls/05-prism-precip-aggregate.R
	Rscript $<
	@echo "Ran PRISM aggregate"
# Other dependencies: 
# data/prism/* 
# data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp


# -----------------------------------------------------------------------------
# Helpers
.PHONY: all data-clean data-raw data-download water FORCE
FORCE: