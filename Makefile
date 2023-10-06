# -----------------------------------------------------------------------------
# Options and variables
R_OPTS=--vanilla
#prism-times := qtr season year
#prism-fp :=  $(prism-times:%=data-clean/watershed/ppt-%-wshd-dt.fst)
crop-names := acre yield irrigated
crop-dt := $(crop-names:%=data/crops/all-crop-%-dt.fst)
crop-cnty := $(crop-names:%=data/crops/all_crop_%_county.fst)

# -----------------------------------------------------------------------------
# Setting up higher level targets
all: data-download data-raw data-clean
# Downloading data from web or API
data-download: $(crop-cnty) data/pesticides/est_pest_use.csv data/pesticides/est_pest_use.fst data/crops/census_crop_acre_county.fst data/pop-area-empl/cnty-area-dt.fst data/pop-area-empl/cnty-pop-dt.fst data/pop-area-empl/farm-empl-dt.fst
# Raw data we won't touch very often
data-raw: data/pop-area-empl/edu-dt.fst data/bls-labforce/labor-dt.fst $(crop-dt)
# Just the intermediate files that we update more frequently
data-clean: data-clean/health-dt.fst data-clean/comb-cnty-dt.fst data-clean/fs-dt.fst data/crops/crop-acre-percentile-90-95.fst data-clean/trt-dt.fst
# Watershed modeling 
#water: $(prism-fp) 

# -----------------------------------------------------------------------------
# Targets for data-clean

# County level health data 
data-clean/health-dt.fst: R/03-combine/01-comb-county-health.R data-clean/comb-cnty-dt.fst data/glyph-nat-dt.fst
	Rscript $<
	@echo "Made health-dt"

# Combining all county data into one table
data-clean/comb-cnty-dt.fst: R/03-combine/00-combine-cnty.R data-clean/trt-dt.fst data-clean/fs-dt.fst
	Rscript $<
	@echo "Made comb-cnty-dt"
# Other dependencies: data-clean/watershed/county-exposure-dt.fst data/pop-area-empl/cnty-area-dt.fst data/pop-area-empl/cnty-pop-dt.fst data/pop-area-empl/farm-empl-dt.fst data/pop-area-empl/ruralurbancodes2003.xls
# data/pesticides/est_pest_use.fst data-clean/fs-dt.fst $(crop-dt) data/pop-area-empl/edu-dt.fst data/bls-labforce/labor-dt.fst 

# First stage predictions
data-clean/fs-dt.fst: R/01-define-treatment/01-first-stage.R data/crops/crop-acre-percentile-90-95.fst data/glyph-nat-dt.fst
	Rscript $<
	@echo "Made first stage predictions"
# Other dependencies: data/pop-area-empl/cnty-area-dt.fst data/pesticides/est_pest_use.fst

# Shift share instruments 
data/glyph-nat-dt.fst data/glyph-nat-watershed-dt.fst &: R/01-define-treatment/00c-glyph-national.R
	Rscript $<
	@echo "Made shift share instruments"
# Other dependencies: data-clean/watershed/weights/hydrobasin-area-weights.fst data-clean/watershed/upstream-dt-hydrobasin.fst data/pesticides/est_pest_use.fst

# Pre period acreage percentiles
data/crops/crop-acre-percentile-90-95.fst: R/01-define-treatment/00b-crop-acre-percentiles.R
	Rscript $<
	@echo "Made crop acreage percentiles"
# Other dependencies: data/pop-area-empl/cnty-area-dt.fst data/crops/all-crop-acre-dt.fst

# Treatment definitions
data-clean/trt-dt.fst: R/01-define-treatment/00-define-treatment.R
	Rscript $<
	@echo "Made treatment definitions"
# Other dependencies data/gaez-suitability-index/y_diff_dt.fst

# -----------------------------------------------------------------------------
# Targets for data-raw

# GAEZ suitability data
data/gaez-suitability-index/hydrobasin-y-diff-dt.fst data/gaez-suitability-index/y_diff_dt.fst &: R/00-data-prep/farm/04-gaez-yield.R 
	Rscript $<
	@echo "Made suitability data"
# Other dependencies: $(wildcard data/gaez-suitability-index/attainable-yield/yl*r_*.tif)

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
data/pesticides/est_pest_use.csv data/pesticides/est_pest_use.fst &: R/00-data-prep/farm/01-usgs-chem-data.R
	Rscript $<
	@echo "Downloaded pesticide data"

# USDA NASS data
$(crop-cnty) data/crops/census_crop_acre_county.fst &: R/00-data-prep/farm/02-usda-nass-data.R
	Rscript $<
	@echo "Downloaded USDA NASS"

# County area 
data/pop-area-empl/cnty-area-dt.fst data/pop-area-empl/cnty-pop-dt.fst &: R/00-data-prep/controls/00-cnty-pop-area.R
	Rscript $<
	@echo "Calculated county area and pop"

# BEA employment
data/pop-area-empl/farm-empl-dt.fst: R/00-data-prep/controls/01-bea-empl.R
	Rscript $<
	@echo "Downloaded BEA data"


# -----------------------------------------------------------------------------
# Targets for water

# NEED TO WRITE THESE TARGETS =============================
#data-clean/watershed/county-exposure-dt.fst: 
#data-clean/watershed/weights/hydrobasin-area-weights.fst 
#data-clean/watershed/upstream-dt-hydrobasin.fst
# Check for others...
# =========================================================
#$(prism-fp): data/prism R/00-data-prep/controls/05-prism-precip-aggregate.R
#	Rscript R/00-data-prep/controls/05-prism-precip-aggregate.R
#	@echo "Ran PRISM aggregate"

# Helpers
.PHONY: all data-clean data-raw data-download
FORCE: