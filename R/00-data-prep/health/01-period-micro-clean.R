# Cleaning raw data and saving it as an fst for quick access
# Loading packages
library(pacman)
p_load(
  data.table,readr,here,magrittr,stringr,dplyr,
  tidyr,glue,purrr,tictoc,furrr, fst, skimr, 
  lubridate
)

# Loading state fips codes
state_fips = 
  fread(
    here("data/download-manual/state-fips-codes.csv"),
    colClasses = "character"
  )[,.(
    state_abb, 
    new_state_fips = str_pad(state_fips, '2','left','0')
  )]

# Getting the natality data dictionary 
source(here("R/00-data-prep/health/00-data-dictionary.R"))

#-------------------------------------------------------------#
# Creating a function that cleans data for a single year
#-------------------------------------------------------------#
clean_natality = function(file_year, dt_out = FALSE){
  
  # Filtering to only dictionary for specific year
  natality_dct_yr = natality_dct[year == file_year]
  # Setting filepath for natality data
  natality_filepath = here(paste0(
    "data/health-restricted/raw/NATL",file_year,"US.AllCnty.txt"
  ))
  # Reading Natality (denominator) File
  natality_raw_dt = read_fwf(
    file = natality_filepath,
    col_positions = fwf_positions(
      start = natality_dct_yr$start_pos, 
      end = natality_dct_yr$end_pos, 
      col_names = natality_dct_yr$var_name
    )
  ) |> as.data.table() 
  
  if(file_year %in% 2003:2013){
    
    natality_raw_dt =
      merge(
        natality_raw_dt,
        state_fips,
        by.x = "state_postal", 
        by.y = "state_abb", 
        all.x = TRUE
      ) |>
      merge(
        state_fips,
        by.x = "state_postal_occ", 
        by.y = "state_abb", 
        all.x = TRUE
      ) %>% 
      .[,':='(
          year = file_year,
          month = dob_mm,
          GEOID = paste0(
            str_pad(new_state_fips.x,2,"left","0"), 
            str_pad(county_fips,3,"left","0")
          ),
          GEOID_occ = paste0(
            str_pad(new_state_fips.y,2,"left","0"), 
            str_pad(county_fips_occ,3,"left","0")
          ),
          meduc = fcase(
            !is.na(meduc), meduc,
            umeduc %in% paste0("0",0:8), 1,
            umeduc %in% c("09",10,11), 2,
            umeduc == "12", 3,
            umeduc %in% 13:15, 4,
            umeduc == 16, 6, 
            umeduc == 17, 7, 
            umeduc == 99, 9
          ),
          mage = fcase(
            file_year >= 2004 & as.numeric(mage) <= 15, 1,
            file_year >= 2004 & as.numeric(mage) > 15, as.numeric(mage) - 13,
            file_year < 2004 & !is.na(mage), as.numeric(mage) 
          ),
        dbwt = ifelse(dbwt == 9999, as.numeric(NA), as.numeric(dbwt)),
        no_city = place_fips == 99999,
        apgar5 = ifelse(apgar5 == 99, as.numeric(NA),as.numeric(apgar5)),
        c_section = fcase(
          delmeth %in% c(3,4,7), TRUE,
          delmeth %in% c(1,2,6), FALSE
        ),
        tobacco = fcase(
          tobacco == '1', TRUE, 
          tobacco_recode == 'Y', TRUE, 
          tobacco %in% c('2','8','9'), FALSE, 
          tobacco_recode %in% c('N','U'), FALSE,
          default = FALSE
        ),
        alcohol = alcohol == '1'
      )]
  }else if(file_year %in% 1990:1991){
    
    # Cleaning variables
    natality_raw_dt[,':='(
      year = file_year,
      month = dob_mm,
      GEOID = paste0(
        str_pad(state_fips,2,"left","0"), 
        str_pad(county_fips,3,"left","0")
      ),
      GEOID_occ = paste0(
        str_pad(state_fips_occ,2,"left","0"), 
        str_pad(county_fips_occ,3,"left","0")
      ),
      mrace = fcase(
        mrace %in% paste0("0",1:3), str_sub(mrace, 2,2),
        mrace %in% paste0("0",4:8), "4",
        mrace == "09", "1"
      ),
      frace = fcase(
        frace %in% paste0("0",1:3), str_sub(frace, 2,2),
        frace %in% paste0("0",4:8), "4",
        frace == "09", "1",
        default = "9"
      ),
      meduc = fcase(
        umeduc %in% paste0("0",0:8), 1,
        umeduc %in% c("09",10,11), 2,
        umeduc == "12", 3,
        umeduc %in% 13:15, 4,
        umeduc == 16, 6, 
        umeduc == 17, 7, 
        umeduc == 99, 9
      ),
      sex = fcase(
        sex == "1", "M",
        sex == "2", "F"
      ),
      dbwt = ifelse(dbwt == 9999, as.numeric(NA), as.numeric(dbwt)),
      no_city = city_res == 999,
      apgar5 = ifelse(apgar5 == 99, as.numeric(NA),as.numeric(apgar5)),
      c_section = fcase(
        delmeth %in% c(3,4), TRUE,
        delmeth %in% c(1,2), FALSE
      ),
      tobacco = tobacco == '1',
      alcohol = alcohol == '1'
    )] 
  }else if(file_year %in% 1992:2002){
    
    # Cleaning variables
    natality_raw_dt[,':='(
      year = file_year,
      month = dob_mm,
      GEOID = paste0(
        str_pad(state_fips,2,"left","0"), 
        str_pad(county_fips,3,"left","0")
      ),
      GEOID_occ = paste0(
        str_pad(state_fips_occ,2,"left","0"), 
        str_pad(county_fips_occ,3,"left","0")
      ),
      mrace = fcase(
        mrace %in% paste0("0",1:3), str_sub(mrace, 2,2),
        mrace %in% c(paste0("0",4:7), paste0(1:7,"8")), "4"
      ),
      frace = fcase(
        frace %in% paste0("0",1:3), str_sub(frace,2,2),
        frace %in% c(paste0("0",4:7), paste0(1:7,"8")), "4",
        default = "9"
      ),
      meduc = fcase(
        umeduc %in% paste0("0",0:8), 1,
        umeduc %in% c("09",10,11), 2,
        umeduc == "12", 3,
        umeduc %in% 13:15, 4,
        umeduc == 16, 6, 
        umeduc == 17, 7, 
        umeduc == 99, 9
      ),
      sex = fcase(
        sex == "1", "M",
        sex == "2", "F"
      ),
      dbwt = ifelse(dbwt == 9999, as.numeric(NA), as.numeric(dbwt)),
      no_city = city_res == 999,
      apgar5 = ifelse(apgar5 == 99, as.numeric(NA),as.numeric(apgar5)),
      c_section = fcase(
        delmeth %in% c(3,4), TRUE,
        delmeth %in% c(1,2), FALSE
      ),
      tobacco = tobacco == '1',
      alcohol = alcohol == '1'
    )] 
  }
  
  # Fixing broken GEOIDS 
  # skimr::skim(natality_raw_dt)
  # merge(
  #   county_year_dt[year == file_year,.(GEOID, year, in_panel = 1)], 
  #   natality_raw_dt[,.(num_births = .N),.(GEOID = GEOID, year)],
  #   by = c('GEOID','year'),
  #   all = TRUE
  # )[
  #   !(str_sub(GEOID, 1,2) %in% c('02','15','66','72','78','NA')) 
  #   & (is.na(in_panel) | is.na(num_births))]
  natality_raw_dt[,':='(
    GEOID = fcase(
      GEOID == '12025', '12086', 
      GEOID == '51780', '51083',
      GEOID == '51560', '51005',
      !is.na(GEOID), GEOID
    ),
    GEOID_occ = fcase(
      GEOID_occ == '12025', '12086', 
      GEOID_occ == '51780', '51083',
      GEOID_occ == '51560', '51005',
      !is.na(GEOID_occ), GEOID_occ
    )
  )]

  # For 1990-1993 files, have to to special merge to get place_fips
  if(file_year %in% 1990:1993){
    # Loading the list of cities 
    city_dt =
      read.fst(
        path = here('data/clean/city-water-dt.fst'),
        as.data.table = TRUE
      )
    # Loading the manual crosswalk
    manual_xwalk = 
      fread(
        here('data/health-restricted/city_res_xwalk.csv')
      )[,.(
        city_state,
        city_res = str_pad(city_res, 3, 'left','0')
      )] |>
      merge(
        city_dt,
        by = 'city_state',
        all= TRUE
      ) %>% .[,.(state_fips, city_res, place_fips)]
    natality_raw_dt[,state_fips := str_sub(GEOID, 1,2)]
    # Adding place fips to data 
    natality_raw_dt = 
      merge(
        natality_raw_dt[,-'place_fips'], 
        manual_xwalk,
        by = c('state_fips','city_res'),
        all.x = TRUE
      )
  }
  
  # Only saving the columns we will use 
  natality_dt = natality_raw_dt[,.(
    GEOID, GEOID_occ, year, month, sex, dbwt, apgar5, gestation,
    mage, mrace, mhisp, meduc, mar,
    fage,  fhisp, frace, 
    birth_facility, restatus, no_city,
    live_birth_order, total_birth_order,
    c_section, 
    city_res, place_fips,
    plurality,febrile,meconium,membrane_rupture,abruptio_placentae,placenta_previa,excessive_bleeding,mother_seizure,labor_under_3h,labor_over_20h,labor_dysfunc,breech,cephalopelvic_disproportion,cord_prolapse,anesthetic_comp,fetal_distress,labor_complication,baby_anemia,baby_injury,fetal_alcohol,baby_hyaline,meconoim_aspiration,vent_under_30m,vent_over_30m,baby_seizures,baby_other_abn,anencephaly,spina_bifida,hydrocephalus,microcephalus,baby_other_cent_nerv,heart_malform,baby_other_circ,rectal_atresia,tracheo,omphalocele,baby_other_gastro,malformed_genitals,renal_agenesis,baby_other_urogenital,cleft_lip,polydactyly,club_foot,baby_hernia,baby_other_muscl,downs_syndr,baby_other_chromo,baby_other_cong,tobacco, alcohol
  )]
  # Saving the results 
  write.fst(
    natality_dt,
    path = here(paste0("data/health-restricted/period-clean/natality-",file_year,".fst"))
  )
  # Also returning the results 
  if(dt_out == TRUE){return(natality_dt)}
  rm(natality_dt, natality_raw_dt)
  print(paste(file_year, "done"))
}


#plan(multisession(workers = 4))
map(
  2003:2013,
  clean_natality
)


  # Codes from:
  # https://www-doh.state.nj.us/doh-shad/query/ICDInf130.htm
  # https://wonder.cdc.gov/wonder/sci_data/natal/linked/type_txt/cohort99/130Cause99.pdf
# Function to clean the icd-code crosswalk
get_icd_xwalk = function(file_year){
  # Loading the data
  inf_icd_crosswalk = fread(
      here('data/health-restricted/inf-icd-xwalk.csv')
    )[,inf_cod := str_pad(inf_cod, '3','left','0')]
  # For ICD-9 codes we need to do a lot of work
  if(file_year < 1999){
    # Need to be able to merge this list to icd codes 
    codes = str_split(inf_icd_crosswalk$icd_9,',') 
    tmp =   
      cbind(
        inf_cod = inf_icd_crosswalk$inf_cod,
        rbindlist(
          lapply(codes, \(x)data.table(t(x))),
          fill =TRUE
        ) 
      ) |>
      melt(id.vars= 'inf_cod') %>%
      .[!is.na(value)]
    # Handling external
    tmp[,':='(
      external = str_detect(value, 'E'),
      value = str_remove_all(value, 'E')
    )]
    # Getting start and end codes 
    tmp[,
      c('start_code','end_code') := tstrsplit(value, '-',fixed = TRUE),
    ]
    tmp[,end_code := fifelse(is.na(end_code), start_code, end_code)]
    tmp[,c('variable','value') := NULL]
    # Adding fourth zero if here is not one 
    tmp[,':='(
      start_code = 
        fifelse(
          str_detect(start_code, '\\.\\d'),
          start_code, 
          paste0(start_code, '.0')
        ) |> 
        str_remove('\\.') |>
        str_pad(4,'left','0'),
      end_code = 
        fifelse(
          str_detect(end_code, '\\.\\d'),
          end_code, 
          paste0(end_code, '.9')
        )|> 
        str_remove('\\.') |>
        str_pad(4,'left','0')
    )]
    # A few codes were missing from the data 
    tmp = 
      rbind(
        tmp, 
        data.table(
          inf_cod = c('052','158','158'),
          external = c(FALSE, TRUE, TRUE), 
          start_code = c('4200','9800', '9690'), 
          end_code = c('4229','9999', '9699')
        )
      )
    # Expanding to have row for every possible code in range
    icd_xwalk = 
      map_dfr(
        1:nrow(tmp),
        \(i){
          data.table(
            inf_cod = tmp$inf_cod[i],
            icd_9 = str_pad(tmp$start_code[i]:tmp$end_code[i], 4, 'left','0')
          )
        }
      )
    # Merging
    long_inf_icd_crosswalk = 
      merge(
        inf_icd_crosswalk[,-'icd_9'], 
        icd_xwalk,
        by = 'inf_cod'
      )[,.(icd_9, category_code, category_desc, inf_cod, inf_cod_desc = descr)] |>
      setkey(icd_9)
    return(long_inf_icd_crosswalk)
  # For ICD-10 codes it is a lot easier
  }else {
    out_dt = inf_icd_crosswalk[,.(
      inf_cod, inf_cod_desc = descr, category_code, category_desc
    )]  |> setkey(inf_cod)
    return(out_dt)
  }
}


#-------------------------------------------------------------#
# Function to clean mortality data
#-------------------------------------------------------------#
clean_mortality = function(file_year, mortality_dct, dt_out = FALSE){
  # Filtering data dictionary to year
  mort_dct = mortality_dct[year == file_year]
  # Now we can read the Fixed Width CDC data files
  mort_filepath = here::here(
    paste0("data/health-restricted/raw/MULT",file_year,".USAllCnty.txt")
    )
  # Reading Numerator File (deaths)
  mort_raw_dt = 
    read_fwf(
      file = mort_filepath,
      col_positions = fwf_positions(
        start = mort_dct$start_pos, 
        end = mort_dct$end_pos, 
        col_names = mort_dct$var_name
      ),
      col_types = 'c'
    ) |> as.data.table()  
  # Filtering to deaths age < 1
  mort_raw_dt =  mort_raw_dt[!is.na(inf_age) & !is.na(inf_cod)]
  # Turning state abbreviations into codes 
  mort_raw_dt[,':='(
    state_fips = str_sub(GEOID,1,2),
    county_fips = str_sub(GEOID,3,5),
    state_fips_occ = str_sub(GEOID_occ,1,2),
    county_fips_occ = str_sub(GEOID_occ,3,5)
  )]
  # Merging codes to abbreviations
  mort_raw_dt = 
    merge(
      mort_raw_dt,
      state_fips, 
      by.x = 'state_fips', 
      by.y = 'state_abb',
      all.x = TRUE
    ) |>
    merge(
      state_fips, 
      by.x = 'state_fips_occ', 
      by.y = 'state_abb',
      all.x = TRUE
    ) 
  # Changing abbr to codes if state fips are matched to a code
  mort_raw_dt[,':='(
    GEOID = fcase(
      is.na(new_state_fips.x), GEOID,
      !is.na(new_state_fips.x), paste0(new_state_fips.x, county_fips)
    ),
    state_fips = fcase(
      is.na(new_state_fips.x), state_fips,
      !is.na(new_state_fips.x), new_state_fips.x
    ),
    GEOID_occ = fcase(
      is.na(new_state_fips.y), GEOID_occ,
      !is.na(new_state_fips.y), paste0(new_state_fips.y, county_fips_occ)
    ),
    state_fips_occ = fcase(
      is.na(new_state_fips.y), state_fips_occ,
      !is.na(new_state_fips.y), new_state_fips.y
    )
  )]
  # Cleanup
  mort_raw_dt[,c('new_state_fips.x','new_state_fips.y') := NULL]
  # Calculating birth month from age and month of death 
  mort_raw_dt[,':='(
    date_death = ymd(paste(file_year, month_death, '15')),
    year_death = file_year,
    inf_age = as.integer(inf_age)
  )]
  # Inferring date of birth from infant age variable 
  mort_raw_dt[,':='(
    age_months = fcase(
      inf_age %in% 1:9, 0L, 
      inf_age %in% 10:11, 1L,
      inf_age %in% 12:22, inf_age-11L
    ),
    date_birth = fcase(
      inf_age %in% 1:9, date_death, 
      inf_age %in% 10:11, date_death - months(1),
      inf_age %in% 12:22, date_death - months(inf_age-11)
    )
  )]
  # Adding year and month 
  mort_raw_dt[,':='(
    year_birth = year(date_birth),
    month_birth = str_pad(month(date_birth), 2,'left','0'),
    # Removing these because they are not actually meaningful
    date_birth = NULL, date_death = NULL
  )]
  
  # Now categorizing the death codes 
  mort_raw_dt[,ucod := str_pad(ucod, 4,'right','0')]
  # Getting crosswalk for icd-9 codes
  inf_icd_crosswalk = get_icd_xwalk(file_year)
  if(file_year < 1999){
    mort_raw_dt |> setkey(ucod)
    # Adding 130 recode and categories 
    mort_dt = 
      merge(
        mort_raw_dt[,-'inf_cod'], 
        inf_icd_crosswalk, 
        by.x = 'ucod',
        by.y = 'icd_9',
        all.x = TRUE
      )
  } else{
    mort_raw_dt |> setkey(inf_cod)
    # Adding 130 recode and categories 
    mort_dt = 
      merge(
        mort_raw_dt, 
        inf_icd_crosswalk, 
        by = 'inf_cod',
        all.x = TRUE
      )
  }
  # Adding a flag for internal
  mort_dt[,':='(
    internal = category_desc != "External"
  )]
  # Checking to make sure no cod's are missing 
  print(mort_dt[is.na(category_code), .N,keyby = ucod], n = 1000)
  # Only saving the columns we will use 
  write.fst(
    mort_dt,
    path = here(paste0("data/health-restricted/period-clean/mortality-",file_year,".fst"))
  )  
  # Also returning the results 
  if(dt_out == TRUE){return(mort_dt)}
  rm(mort_raw_dt, mort_dt)
  gc()
}

# Running 
map(
  1990:2013,
  clean_mortality,
  mortality_dct = mortality_dct
)


# tmp = read.fst(
#   here("data/health-restricted/period-clean/natality-2004.fst"),
#   as.data.table = TRUE
# )
# tmp[is.na(category_code), .N,keyby = ucod]










