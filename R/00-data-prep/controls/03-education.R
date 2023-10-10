#-------------------------------------------------------
# Education data from USDA ERS
#-------------------------------------------------------
pacman::p_load(
  data.table, purrr, lubridate, stringr, janitor,
  dplyr, tidyr, magrittr, fst, here, readxl
)

clean_education_data = function(path_in = "data/download-manual/Education.xls"){
  # Reading in the raw data
  edu_raw_df =
    read_xls(
      path = here(path_in),
      skip = 4
    ) |> clean_names()
  # Cleaning up the column names a bit 
  names(edu_raw_df) = 
    names(edu_raw_df) |>
      str_replace("completing_four_years_of_college","with_a_bachelors_degree") |>
      str_replace("four_years_of_college", "bachelors_degree") |>
      str_replace("some_college_1_3_years", "some_college_or_associates_degree") |>
      str_replace("90", "_1990") |>
      str_replace("80", "_1980") |>
      str_replace("70", "_1970") |>
      str_remove("_19")
  # Pivoting into long form rather than wide
  edu_dt = 
    edu_raw_df |>
    pivot_longer( # Tidying the data
      cols = starts_with(c("less_than","high_school","some_college","bachelor","percent")),
      names_to = c(".value","year"),
      names_pattern = '(.*)_(\\d{4})'
    ) |> data.table()
  # Final bit of cleaning up 
  edu_dt |>
    setnames(
      old = c(
        "less_than_a_high_school_diploma"                               
        ,"high_school_diploma_only"                                      
        ,"some_college_or_associates_degree"                             
        ,"bachelors_degree_or_higher"                                    
        ,"percent_of_adults_with_less_than_a_high_school_diploma"        
        ,"percent_of_adults_with_a_high_school_diploma_only"             
        ,"percent_of_adults_completing_some_college_or_associates_degree"
        ,"percent_of_adults_with_a_bachelors_degree_or_higher"  
      ),
      new = c(
        "hs_some"                               
        ,"hs_deg"                                      
        ,"college_some"                             
        ,"college_deg"                                    
        ,"hs_some_pct"        
        ,"hs_deg_pct"             
        ,"college_some_pct"
        ,"college_deg_pct"  
      )
    )
  # Selecting cols
  edu_dt = edu_dt[,.(
    GEOID = fips_code, 
    census_year = str_sub(year, 1, 3), 
    hs_some, hs_deg, college_some, college_deg,
    hs_some_pct, hs_deg_pct, college_some_pct, college_deg_pct
  )]
  # Saving the results 
  write.fst(
    edu_dt,
    path = here("data/raw/edu-dt.fst")
  )
}

# Running the function
clean_education_data()