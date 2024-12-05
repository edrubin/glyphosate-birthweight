# Replication materials for paper *Glyphosate exposure and GM seed rollout unequally reduced perinatal health*

## Requirements 

We run the analysis using `make` version `4.4.1` and `R` version `4.4.1`. We use [`renv`](https://rstudio.github.io/renv/index.html) to manage packages. To get started, install `renv` and run `renv::restore()` to download and install the package versions used in this project, which are recorded in the `renv.lock` file. 

`make data-clean` will generate all of the intermediate files we need for the analysis, which takes about 4 minutes to run. This does not run two categories of targets: downloading data and the water ML pipeline. Those both take a while to run, along with the data we downloaded manually---they are grouped together in the `data/download-manual`, `data/download-script`, and `data/watershed` directories.

`make data-raw` will create all of the files in the `data/raw` directory, `make data-clean` creates all of the files in the `data/clean` directory, `make water` creates all of the files in the `data/watershed` directory, and `make data-download` creates all of the files in the `data/download-script` directory.

For analysis: 
- `make desc-figs` will create the descriptive time series plots and maps, as well as some appendix figures   
- `make cnty-results` runs the county level analysis (event studies, DiD, TSLS for many outcomes and different treatment vars) and creates the event study figures. It also does the Ag district level analysis
- `make predict-bw` runs scripts to train birthweight prediction models and generate predictions for each birth  
- `make micro-mods` runs the birth-level analysis  
- `make micro-results` creates figures from the birth-level analysis  

### API keys

The following API keys are required, save them to .Renviron with with `usethis::edit_r_environ()`   
- [USDA QuickStats API](https://quickstats.nass.usda.gov/api/) saved in .Renviron as `NASS_KEY`  
- [Census API](https://api.census.gov/data/key_signup.html) saved in .Renviron as `CENSUS_KEY`  
- [BEA API](https://apps.bea.gov/API/signup/) saved in .Renvoron as `BEA_KEY`  

## Data not included

Most data required for our analysis is included in this repository, but some files are too large or not allowed to be shared publicly. 

### NCHS data

Instructions on how to get access to the restricted Births (Natality) and Deaths (Mortality) files are on the [NCHS website](https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm). Our primary analysis uses the natality files between 1990 and 2013. We do supplemental analysis that uses the mortality files over the same time period. Once obtained, the raw natality and mortality files go into: `data/health-restricted/raw`.

### HydroBASINS  

Download the [HydroBASINS](https://www.hydrosheds.org/products/hydrobasins) data for North America and copy the contents into `data/watersheds/hydrobasins`.


### USGS gridded soil survey  

There are two pieces of data from the USGS's gridded soil survey needed for water analysis, which the [USGS hosts on Box here](https://nrcs.app.box.com/v/soils/folder/233393842838): 

1. Download `MUKEY Grids (TIF)/FY2021_gNATSGO_mukey_grid.zip` and place the contents here: `data/watersheds/soil-quality/gNATSGO_mukey_grid`
2. Download `MUKEY Grids (TIF)/FY2021_gNATSGO_Tabular_CSV.zip` and place the contents here: `data/watersheds/soil-quality/gNATSGO_Tabular_CSV`

## Other data sources

[GAEZ data](https://gaez-services.fao.org/server/rest/services/res05/ImageServer) for Attainable Yield is in `data/download-manual/attainable-yield/`. These can also be downloaded using the following links: 
  - [Soy high](https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res05/CRUTS32/Hist/8110H/ylHr_soy.tif): res05/CRUTS32/Hist/8110H/ylHr_soy.tif  
  - [Soy low](https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res05/CRUTS32/Hist/8110L/ylLr_soy.tif): res05/CRUTS32/Hist/8110H/ylLr_soy.tif
  - [Corn high](https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res05/CRUTS32/Hist/8110H/ylHr_mze.tif): res05/CRUTS32/Hist/8110H/ylHr_mze.tif  
  - [Corn low](https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res05/CRUTS32/Hist/8110L/ylLr_mze.tif): res05/CRUTS32/Hist/8110H/ylLr_mze.tif
  - [Cotton high](https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res05/CRUTS32/Hist/8110H/ylHr_cot.tif): res05/CRUTS32/Hist/8110H/ylHr_cot.tif  
  - [Cotton low](https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res05/CRUTS32/Hist/8110L/ylLr_cot.tif): res05/CRUTS32/Hist/8110H/ylLr_cot.tif 

The USDA Agriculutral Statistic District to County FIPS crosswalk is [here](https://www.nass.usda.gov/Data_and_Statistics/County_Data_Files/Frequently_Asked_Questions/county_list.txt).
