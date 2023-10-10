# Script to downlad early years of pesticide data 
pacman::p_load(
  data.table, janitor, here, fst
)

download_pest_data = function(years = 1992:2012){
  # Now getting the pesticide data, 1992-2012 data from website
  pest_urls = paste0(
    "https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/PesticideUseEstimates/EPest.county.estimates.", 
    years, 
    ".txt"
  )
  # Reading files from each year
  pest = 
    lapply(
      pest_urls, 
      fread, 
      colClasses = c(
        "STATE_FIPS_CODE"="character",
        "COUNTY_FIPS_CODE"="character")
    ) |>
    rbindlist() |>
    clean_names()
  return(pest)
}

# Downloading and saving 
pest_old = download_pest_data()
write.fst(
  pest_old, 
  here('data/download-script/usgs-pesticides-raw.fst')
)