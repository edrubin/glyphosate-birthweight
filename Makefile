# -----------------------------------------------------------------------------
# Options 
R_OPTS=--vanilla
# Directories 
dman-dir = data/download-manual/
dscr-dir = data/download-script/
raw-dir = data/raw/
clean-dir = data/clean/
result-dir = data/results/
water-dir = data/watershed/
health-dir = data/health-restricted/
fig-desc-dir = figures/descriptive/
# Variables
crop-names := acre yield irrigated
crop-dt := $(crop-names:%=$(raw-dir)all-crop-%-dt.fst)
# For watershed data
mods := lasso-glyph rf-glyph lasso-ampa rf-ampa
water-mods := $(mods:%=$(result-dir)ml-water/final-fit-%.qs)
prism-times := qtr season year
prism-fp :=  $(prism-times:%=$(water-dir)ppt-%-wshd-dt.fst)
pop-wt-yrs := 1990 2000 2010
pop-wts := $(pop-wt-yrs:%=$(water-dir)weights/hydrobasin-pop-weights%.fst)
pop-grids := $(pop-wt-yrs:%=$(water-dir)us-pop-grid/uspop%.tif)

# -----------------------------------------------------------------------------
# Setting up higher level targets
all:  data-raw data-clean # water data-download
# Downloading data from web or API
data-download: \
 $(dscr-dir)usgs-pesticides-raw.fst \
 $(dscr-dir)cnty-area-dt.fst \
 $(dscr-dir)cnty-pop-dt.fst \
 $(dscr-dir)farm-empl-dt.fst \
 download-crops
# Raw data we won't touch very often
data-raw: \
 $(raw-dir)est_pest_use.csv \
 $(raw-dir)est_pest_use.fst \
 $(raw-dir)edu-dt.fst \
 $(raw-dir)labor-dt.fst \
 $(raw-dir)census_crop_acre_county.fst \
 $(raw-dir)y_diff_dt.fst \
 $(crop-dt)
# Intermediate files that may get updated often
data-clean: \
 $(clean-dir)health-dt.fst \
 $(clean-dir)comb-cnty-dt.fst \
 $(clean-dir)fs-dt.fst \
 $(clean-dir)crop-acre-percentile-90-95.fst \
 $(clean-dir)trt-dt.fst \
 $(clean-dir)glyph-nat-dt.fst
# Watershed modeling 
water: \
 $(water-dir)county-exposure-dt.fst \
 $(water-dir)county-exposure-pred-dt.fst
# Predicting birthweights
# predict-bw:

# Descriptive figs 
desc-figs: \
	$(fig-descr-dir)glyph-km2-diff-9512.jpeg \
	$(wildcard $(fig-descr-dir)ts-*.jpeg) \
	$(wildcard $(fig-descr-dir)yield-diff-percentile/*.jpeg)
# County Level Analysis
cnty-results: 

# -----------------------------------------------------------------------------
# Targets for county level analysis


# -----------------------------------------------------------------------------
# Targets for descriptive figures
$(fig-descr-dir)glyph-km2-diff-9512.jpeg \
 $(wildcard $(fig-descr-dir)yield-diff-percentile/*.jpeg): \
 R/05-results/descr-maps.R \
 $(clean-dir)comb-cnty-dt.fst
	Rscript $< 
	@echo "Made descriptive maps"

$(wildcard $(fig-descr-dir)ts-*.jpeg): \
 R/05-results/descr-time-series.R \
 $(clean-dir)comb-cnty-dt.fst
	Rscript $<
	@echo "Made descriptive time series"


# -----------------------------------------------------------------------------
# Targets for predict-bw


# Micro natality clean 
$(clean-dir)natality-micro.fst: \
 R/04-analysis/00-build-natality-twfe.R \
 $(wildcard $(health-dir)period-clean/natality-*.fst)
	Rscript $<
	@echo "Built natality micro data"

# -----------------------------------------------------------------------------
# Targets for data-clean

# County level health data 
$(clean-dir)health-dt.fst: \
 R/03-combine/01-comb-county-health.R \
 $(clean-dir)comb-cnty-dt.fst \
 $(wildcard $(health-dir)period-clean/*.fst)
	Rscript $<
	@echo "Made health-dt"

# Combining all county data into one table
$(clean-dir)comb-cnty-dt.fst: \
 R/03-combine/00-combine-cnty.R \
 $(clean-dir)trt-dt.fst \
 $(clean-dir)fs-dt.fst \
 $(raw-dir)est_pest_use.fst \
 $(raw-dir)edu-dt.fst \
 $(raw-dir)labor-dt.fst \
 $(crop-dt)
	Rscript $<
	@echo "Made comb-cnty-dt"
# Other dependencies: 
# $(water-dir)county-exposure-dt.fst 
# $(dscr-dir)cnty-area-dt.fst 
# $(dscr-dir)cnty-pop-dt.fst 
# $(dscr-dir)farm-empl-dt.fst 
# $(dman-dir)ruralurbancodes2003.xls

# First stage predictions
$(clean-dir)fs-dt.fst: \
 R/01-define-treatment/01-first-stage.R \
 $(clean-dir)crop-acre-percentile-90-95.fst \
 $(clean-dir)glyph-nat-dt.fst \
 $(raw-dir)est_pest_use.fst
	Rscript $<
	@echo "Made first stage predictions"
# Other dependencies: 
# $(dscr-dir)cnty-area-dt.fst 

# Shift share instruments 
$(clean-dir)glyph-nat-dt.fst \
 $(water-dir)glyph-nat-watershed-dt.fst &: \
 R/01-define-treatment/00c-glyph-national.R \
 $(raw-dir)est_pest_use.fst
	Rscript $<
	@echo "Made shift share instruments"
# Other dependencies: 
# $(water-dir)weights/hydrobasin-area-weights.fst 
# $(water-dir)upstream-dt-hydrobasin.fst 

# Pre period acreage percentiles
$(clean-dir)crop-acre-percentile-90-95.fst: \
 R/01-define-treatment/00b-crop-acre-percentiles.R \
 $(raw-dir)all-crop-acre-dt.fst
	Rscript $<
	@echo "Made crop acreage percentiles"
# Other dependencies: 
# $(dscr-dir)cnty-area-dt.fst 

# Treatment definitions
$(clean-dir)trt-dt.fst: \
 R/01-define-treatment/00-define-treatment.R \
 $(raw-dir)y_diff_dt.fst
	Rscript $<
	@echo "Made treatment definitions"

# -----------------------------------------------------------------------------
# Targets for data-raw
# Pesticide data
$(raw-dir)est_pest_use.csv \
 $(raw-dir)est_pest_use.fst &: \
 R/00-data-prep/farm/01-usgs-chem-data.R 
	Rscript $<
	@echo "Cleaned pesticide data"
# Other dependencies:
# $(dscr-dir)usgs-pesticides-raw.fst
# $(dman-dir)est_pest_use_raw_2013_2017.txt

# GAEZ suitability data
$(raw-dir)y_diff_dt.fst &: R/00-data-prep/farm/04-gaez-yield.R 
	Rscript $<
	@echo "Made suitability data"
# Other dependencies: 
# $(wildcard $(dman-dir)/attainaible-yield/yl*r_*.tif)

# Education data
$(raw-dir)edu-dt.fst: R/00-data-prep/controls/03-education.R 
	Rscript $<
	@echo "Made education data"
# Other dependencies $(dman-dir)Education.xls

# BLS labor data
$(raw-dir)labor-dt.fst: R/00-data-prep/controls/02-bls-labor.R
	Rscript $<
	@echo "Made bls labor data"
# Other dependencies $(wildcard $(dman-dir)bls-labforce-raw/*.xlsx)

# Crop data
$(crop-dt) &: R/00-data-prep/farm/03-crop-county-cleaning.R
	Rscript $<
	@echo "Made crop data"
# Other dependencies: $(wildcard $(dscr-dir)nass-*/*/*.fst) # Recursive?

# -----------------------------------------------------------------------------
# Targets for data-download

# Dowloading pesticide data from earlier years 
$(dscr-dir)usgs-pesticides-raw.fst: R/00-data-prep/farm/01a-usgs-chem-download.R
	Rscript $<
	@echo "Downloaded pesticide data"

# USDA NASS data
download-crops: R/00-data-prep/farm/02-usda-nass-data.R
	Rscript $<
	@echo "Downloaded USDA NASS"

# County population from census and county area
$(dscr-dir)cnty-area-dt.fst \
 $(dscr-dir)cnty-pop-dt.fst &: \
 R/00-data-prep/controls/00-cnty-pop-area.R
	Rscript $<
	@echo "Calculated county area and pop"

# BEA employment
$(dscr-dir)farm-empl-dt.fst: R/00-data-prep/controls/01-bea-empl.R
	Rscript $<
	@echo "Downloaded BEA data"


# -----------------------------------------------------------------------------
# Targets for water

# Aggregating to county
$(water-dir)county-exposure-dt.fst \
 $(water-dir)county-exposure-pred-dt.fst &: \
 R/02-water-ml/04-watershed-to-county.R \
 $(result-dir)predictions-watershed.fst \
 $(water-dir)pesticide-upstream-dt.fst \
 $(water-dir)watershed-pesticide-dt.fst \
 $(water-dir)weights/hydrobasin-pop-weights2010.fst
	Rscript $<
	@echo "Aggregated to county level" 

# Making predictions for all watersheds/years
$(result-dir)predictions-watershed.fst: \
 R/02-water-ml/03-predictions.R $(result-dir)sample-watershed-dt.fst \
 $(result-dir)pesticide-upstream-dt.fst \
 $(water-mods)
	Rscript $<
	@echo "Made watershed predictions" 

# Training models and updating perfromance plots
$(water-mods) &: \
 R/02-water-ml/02a-train-models.R \
 $(result-dir)sample-watershed-dt.fst
	Rscript $<
	Rscript R/02-water-ml/02b-model-performance.R
	@echo "Trained water ML models"

# Cleaning data for water ML
$(result-dir)sample-watershed-dt.fst \
 $(result-dir)pesticide-upstream-dt.fst &: \
 R/02-water-ml/01-data-prep.R \
 $(water-dir)upstream-dt-hydrobasin.fst \
 $(water-dir)watershed-pesticide-dt.fst
	Rscript $<
	@echo "Prepped water ml data"
# Other dependencies: 
# $(water-dir)usgs-monitoring/Table1. NWQN.SiteInformation.txt 
# $(water-dir)usgs-monitoring/Table2. Glyphosate.AMPA.SampleConc.txt 
# $(water-dir)hydrobasins/hybas_lake_na_lev08_v1c.shp

# Aggregating upstream and downstream
$(water-dir)pesticide-upstream-dt.fst \
 $(water-dir)watershed-pesticide-dt.fst &: \
 R/02-water-ml/00-watershed-upstream-aggregation.R \
 $(water-dir)upstream-dt-hydrobasin.fst \
 $(water-dir)usle-dt.fst \
 $(raw-dir)all-crop-irrigated-dt.fst \
 $(clean-dir)trt-dt.fst $(clean-dir)fs-dt.fst \
 $(water-dir)weights/hydrobasin-area-weights.fst
	Rscript $<
	@echo "Aggregated upstream/downstream"
# Other dependencies: $(water-dir)hydrobasins/hybas_lake_na_lev08_v1c.shp

# Finding watersheds upstream and downstream
$(water-dir)upstream-dt-hydrobasin.fst: \
 R/00-data-prep/watershed/04-watershed-find-upstream.R \
 $(water-dir)weights/hydrobasin-area-weights.fst \
 $(water-dir)hybas-dist-dt.fst
	Rscript $<
	@echo "Found watersheds upstream/downstream"
# Other dependencies: $(water-dir)hydrobasins/hybas_lake_na_lev08_v1c.shp

# Calculating distanct to next downstream
$(water-dir)hybas-dist-dt.fst: \
 R/00-data-prep/watershed/03-next-down-dists.R 
	Rscript $<
	@echo "Calculated dist to next downstream"
# Other dependencies: $(water-dir)hydrobasins/hybas_lake_na_lev08_v1c.shp 

# Area weights
$(water-dir)weights/hydrobasin-area-weights.fst: \
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
$(water-dir)usle-dt.fst: \
 R/00-data-prep/soil/01-combine-soil-precip.R \
 $(water-dir)/soil-quality/watershed-soil-factors.csv \
 $(water-dir)ppt-season-wshd-dt.fst
	Rscript $<
	@echo "Cleaned soil data"

# Raw soil data 
$(water-dir)/soil-quality/watershed-soil-factors.csv: \
 R/00-data-prep/soil/00a-merge-soil-watersheds.R \
 $(water-dir)/soil-quality/gNATSGO_mukey_grid/gNATSGO-mukey.tif \
 $(water-dir)/soil-quality/gNATSGO_Tabular_CSV/chorizon.csv \
 $(water-dir)/soil-quality/gNATSGO_Tabular_CSV/component.csv
	Rscript $<
	@echo "Merged soil data"

# Aggregating precipitation data 
$(prism-fp) &: R/00-data-prep/controls/05-prism-precip-aggregate.R
	Rscript $<
	@echo "Ran PRISM aggregate"
# Other dependencies: 
# $(water-dir)prism/* 
# $(water-dir)hydrobasins/hybas_lake_na_lev08_v1c.shp

# Calculating attainable yield by watershed
$(water-dir)hydrobasin-y-diff-dt.fst: R/00-data-prep/farm/04b-gaez-yield-watershed.R 
	Rscript $<
	@echo "Made suitability data"
# Other dependencies: 
# $(wildcard $(dman-dir)/attainaible-yield/yl*r_*.tif)

# -----------------------------------------------------------------------------
# Helpers
.PHONY: all data-clean data-raw data-download water FORCE
FORCE: