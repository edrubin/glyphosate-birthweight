
# Requirements 

We run the analysis using `make` version `4.4.1` 

`make data-clean` will generate all of the intermediate files we need for the analysis. On my computer it just took 4 minutes to run all targets. This does not run two categories of targets: downloading data and the water ML pipeline. Those both take a while to run, and thus I recommend you just get their data outputs from dropbox for now, along with the data we downloaded manually---they are grouped together in the `data/download-manual`, `data/download-script`, and `data/watershed` directories.

I've set it up so that `make data-raw` will create all of the files in the `data/raw` directory, `make data-clean` creates all of the files in the `data/clean` directory, `make water` creates all of the files in the `data/watershed` directory, and `make data-download` creates all of the files in the `data/download-script` directory.

For analysis: 
- `make desc-figs` will create the descriptive time series plots and maps
- `make cnty-results` runs the county level analysis (event studies, DiD, TSLS for many outcomes and different treatment vars) and creates the event study figures. It also does the Ag district level analysis
- **TODO**: The main analysis is not yet in the makefile, we need to add it!


### API Keys

The following API keys are required, save them to .Renviron with with `usethis::edit_r_environ()`   
- [USDA QuickStats API](https://quickstats.nass.usda.gov/api/) saved in .Renviron as `NASS_KEY`  
- [Census API](https://api.census.gov/data/key_signup.html) saved in .Renviron as `CENSUS_KEY`  
- [BEA API](https://apps.bea.gov/API/signup/) saved in .Renvoron as `BEA_KEY`  

### Other Data 

Ag district to county crosswalk from [here](https://www.nass.usda.gov/Data_and_Statistics/County_Data_Files/Frequently_Asked_Questions/county_list.txt).
