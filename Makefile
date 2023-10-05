# Step 5: Figures and tables 

# Step 4: Analysis

# Step 3: Combine data

# Step 2: Train Water ML model 

# Step 1: Define treatment 

# Step 0: Prep the data 

# Cleaning data ---------------------------------------------------------------

# GAEZ suitability data **NEEDS TO MATCH ENTIRE FOLDER** (in dependency)


# Soil data 

# Raw soil data 

# Watershed things **NEEDS TO MATCH ENTIRE FOLDER** (in target)


# BLS labor data
data/bls-labforce/labor-dt.fst: R/00-data-prep/controls/02-bls-labor.R
	Rscript R/00-data-prep/controls/02-bls-labor.R
	echo "Ran the bls labor script"
# Education data
#data/pop-area-empl/edu-dt.fst: R/00-data-prep/controls/03-education.R data/pop-area-empl/Education.xls
#	Rscript R/00-data-prep/controls/03-education.R	
# PRISM data
#data-clean/watershed/ppt-qtr-wshd-dt.fst, data-clean/watershed/ppt-season-wshd-dt.fst, data-clean/watershed/ppt-yr-wshd-dt.fst: data/prism, R/00-data-prep/controls/05-prism-precip-aggregate.R
#	Rscript R/00-data-prep/controls/05-prism-precip-aggregate.R
# Crop data 
#data/crops/all-crop-acre-dt.fst, data/crops/all-crop-yield-dt.fst, data/crops/all-crop-irrigated-dt.fst: data/crops/all_crop_acre_county.fst, data/crops/all_crop_yield_county.fst, data/crops/all_crop_irrigated_county.fst, R/00-data-prep/farm/03-crop-county-cleaning.R
#	Rscript R/00-data-prep/farm/03-crop-county-cleaning.R


