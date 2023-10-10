#-------------------------------------------------------
# Getting education data from the BLS: https://www.bls.gov/lau/#data
#-------------------------------------------------------
# Setup
pacman::p_load(
  data.table, stringr, magrittr, here, fst,
  janitor, purrr, dplyr, readxl
)

# Function to clean the data
clean_labor = function(file){
  # Reading raw data
  labor_raw_dt = read_xlsx(
    path = here(paste0("data/download-manual/bls-labforce-raw/",file))
  ) |> data.table()
  # Getting year from first column name 
  file_yr = str_extract(colnames(labor_raw_dt)[1],"\\d{4}")
  # Cleaning the column names
  labor_raw_dt |>
    setnames(
      map_chr(
        1:ncol(labor_raw_dt),
        \(j){pull(labor_raw_dt[1:4,..j])[!is.na(labor_raw_dt[1:4,..j])] |> paste(collapse = " ")}
      ) |> make_clean_names()
    )
  # Adding year, removing empty column and unempl rate 
  labor_raw_dt[,':='(
    year = file_yr, 
    x = NULL, 
    unemploy_ment_rate_percent = NULL
  )]
  # Filtering to rows with data
  labor_dt = labor_raw_dt[
    !is.na(county_fips_code) 
    & !(county_fips_code %in% c("County","FIPS","Code")), .(
      GEOID = paste0(state_fips_code, county_fips_code),
      year = as.integer(year),
      labor_force = as.numeric(labor_force), 
      employed = as.numeric(employed),
      unemployed = as.numeric(unemployed),
      unemployment_rate = as.numeric(unemployed)/as.numeric(labor_force)
    )
  ]
  return(labor_dt)
}

# Cleaning all files 
labor_dt = map_dfr(
  list.files(here("data/download-manual/bls-labforce-raw")),
  clean_labor
)

# Saving the results 
write.fst(
  labor_dt, 
  path = here("data/raw/labor-dt.fst")
)




