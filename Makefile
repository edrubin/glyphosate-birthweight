R_OPTS=--vanilla
# Defining variables
prism-times := qtr season year
prism-fp :=  $(prism-times:%=data-clean/watershed/ppt-%-wshd-dt.fst)
crop-names := acre yield irrigated
crop-dt := $(crop-names:%=data/crops/all-crop-%-dt.fst)
crop-cnty := $(crop-names:%=data/crops/all_crop_%_county.fst)
# Setting up specific targets
all: DATA-CLEAN DATA-RAW FORCE
# Raw data we won't touch very often
DATA-RAW: data/pop-area-empl/edu-dt.fst data/bls-labforce/labor-dt.fst $(crop-dt) data/pesticides/est_pest_use.csv data/pesticides/est_pest_use.fst
#$(prism-fp)
# Just the intermediate files that we update more frequently
DATA-CLEAN: data-clean/health-dt.fst

# County level health data 
data-clean/health-dt.fst: R/03-combine/01-comb-county-health.R data-clean/comb-cnty-dt.fst
	Rscript R/03-combine/01-comb-county-health.R
	echo "Made health-dt"

# Helpers
.PHONY: all DATA-RAW DATA-CLEAN FORCE
FORCE: