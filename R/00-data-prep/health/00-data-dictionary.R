
pacman::p_load(data.table, dplyr, here)

# This script creates the data dictionary for natality data
natality_dct_1992 = 
  data.table(
    year = 1992, 
    var_name = c(
      "dob_yy", "dob_mm", 
      "state_fips", "county_fips",
      "state_fips_occ", "county_fips_occ",
      "birth_facility", "mage", 
      "city_res","place_fips", "restatus",
      "mrace", "mhisp",
      "mar", 
      "umeduc",
      "fage",
      "frace","fhisp",
      "live_birth_order", "total_birth_order",
      "apgar5", "sex",
      "gestation", "dbwt",
      "delmeth",
      # New indicator variables 
      "plurality","febrile","meconium","membrane_rupture","abruptio_placentae","placenta_previa","excessive_bleeding","mother_seizure","labor_under_3h","labor_over_20h","labor_dysfunc","breech","cephalopelvic_disproportion","cord_prolapse","anesthetic_comp","fetal_distress","labor_complication","baby_anemia","baby_injury","fetal_alcohol","baby_hyaline","meconoim_aspiration","vent_under_30m","vent_over_30m","baby_seizures","baby_other_abn","anencephaly","spina_bifida","hydrocephalus","microcephalus","baby_other_cent_nerv","heart_malform","baby_other_circ","rectal_atresia","tracheo","omphalocele","baby_other_gastro","malformed_genitals","renal_agenesis","baby_other_urogenital","cleft_lip","polydactyly","club_foot","baby_hernia","baby_other_muscl","downs_syndr","baby_other_chromo","baby_other_cong",
      # Tobacco and alcohol
      'tobacco', 'alcohol'
    ),
    var_desc = c(
      "birth year", "birth month", 
      "residece state fips", "residence county fips",
      "occurence state fips", "occurence county fips",
      "birth place", "mother's age (recode 36)",
      "city of residence (mother)", 'place fips (mother residence)', "residence status",
      "mother's race", "mother hispanic origin",
      "mother's marital status", 
      "mother's education detail",
      "father's age (recode 11)",
      "father's race recode", "father hispanic origin",
      "Sum of all previous live births", "Sum of all previous pregnancies",
      "Five minute APGAR score", "sex of infant",
      "Computed gestation", "Birthweight",
      "Delivery method",
      "plurality","febrile","meconium","membrane_rupture","abruptio_placentae","placenta_previa","excessive_bleeding","mother_seizure","labor_under_3h","labor_over_20h","labor_dysfunc","breech","cephalopelvic_disproportion","cord_prolapse","anesthetic_comp","fetal_distress","labor_complication","baby_anemia","baby_injury","fetal_alcohol","baby_hyaline","meconoim_aspiration","vent_under_30m","vent_over_30m","baby_seizures","baby_other_abn","anencephaly","spina_bifida","hydrocephalus","microcephalus","baby_other_cent_nerv","heart_malform","baby_other_circ","rectal_atresia","tracheo","omphalocele","baby_other_gastro","malformed_genitals","renal_agenesis","baby_other_urogenital","cleft_lip","polydactyly","club_foot","baby_hernia","baby_other_muscl","downs_syndr","baby_other_chromo","baby_other_cong",
      'tobacco use during pregnancy', 
      'alcohol use during pregnancy'
    ),
    start_pos = c(
      176, 172,
      42, 44,
      21, 23,
      8, 72,
      37, 47, 6,
      80, 77,
      87, 
      83,
      156,
      160, 158,
      102, 105,
      205,189,
      183, 193,
      224,
      201,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,
      242, 246
    ),
    end_pos = c(
      179, 173,
      43, 46,
      22, 25,
      8, 73,
      39, 51, 6,
      81, 77,
      87,
      84,
      157,
      161, 158,
      102, 105,
      206, 189,
      184, 196,
      224,
      201,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,
      242, 246
    ),
    var_length = c(
      4, 2, 
      2, 3, 
      2, 3,
      1, 2,
      3,5, 1,
      2, 1,
      1,
      2,
      2,
      2, 1,
      2, 2,
      2, 1,
      2, 4,
      1,
      1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1

    ),  
    var_codes = list(
      data.table(code = 1992, desc = "year"),
      data.table(code = "01-12", desc = "month"),
      data.table(code = "00", desc = "state fips"),
      data.table(code = "000", desc = "county fips"),
      data.table(code = "00", desc = "state fips"),
      data.table(code = "000", desc = "county fips"),
      data.table(code = c(1:5,9), desc = c(
        "Hospital", "Freestanding Birthing Center", "Clinic / Doctor’s Office",
        "Residence", "Other", "Unknown"
      )),
      data.table(code = c("01","02-36"), desc = c("Under 15","Code +13 years")),
      data.table(
        code = c("001", "999", "ZZZ"), 
        desc = c("Cities","Balance of county", "Foreign")
      ),
      data.table(
        code = c("nnnnn", "99999", "00000"), 
        desc = c("Cities","Balance of county", "Foreign")
      ),
      data.table(
        code = 1:4, 
        desc = c("resident","intrastate nonres","interstate nonres","foreign")
      ),
      data.table(code = c(paste0("0",1:7), 18,28,38,48,58,68,78), desc = c(
        "White",
        "Black",
        "American Indian",
        "Chinese",
        "Japanese",
        "Hawaiian",
        "Filipino",
        "Asian Indian",
        "Korean",
        "Samoan",
        "Vietnamese",
        "Guamanian",
        "Other Asian or Pacific Islander",
        "Combined other Asian or Pacific Islander"
      )),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = c(1,2), desc = c("Married","Unmarried")),
      data.table(code = c("00","01-08","09",10:17,99), desc = c(
        "No formal education"
        ,"Years of elementary school"
        ,"1 year of high school"
        ,"2 years of high school"
        ,"3 years of high school"
        ,"4 years of high school"
        ,"1 year of college"
        ,"2 years of college"
        ,"3 years of college"
        ,"4 years of college"
        ,"5 or more years of college"
        ,"Not stated"
      )),
      data.table(code = c(1:11), desc = c(
        "Under 15 years","15-19 years","20-24 years","25-29 years","30-34 years",
        "35-39 years","40-44 years","45-49 years","50-54 years","55-98 years", "Unknown"
      )),
      data.table(code = c(paste0("0",1:7), 18,28,38,48,58,68,78), desc = c(
        "White",
        "Black",
        "American Indian",
        "Chinese",
        "Japanese",
        "Hawaiian",
        "Filipino",
        "Asian Indian",
        "Korean",
        "Samoan",
        "Vietnamese",
        "Guamanian",
        "Other Asian or Pacific Islander",
        "Combined other Asian or Pacific Islander"
      )),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = c("1-7","8","9"), desc = c("Sum of all live births", "Eight or more","Unknown")),
      data.table(code = c("1-7","8","9"), desc = c("Sum of all pregnancies", "Eight or more","Unknown")),
      data.table(code = c("00-10", 99), desc = c("Score", "Unknown")),
      data.table(code = c("1","2"), desc = c("Male", "Female")),
      data.table(code = c("17-47", 99), desc = c("Weeks of gestation", "Unknown")),
      data.table(code = c("0000-9998", 9999), desc = c("Birth weight in grams", "Unknown")),
      data.table(code = c(1:5), desc = c("Vaginal", "Vaginal after previous c-section","Primary C-section","Repeat C-section","Unknown")),
      data.table(code = "1-5", desc = "number of babies"),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,9), desc = c('reported','not reported','Unknown/not stated')),
      data.table(code = c(1,2,9), desc = c('reported','not reported','Unknown/not stated'))
    )
  )


natality_dct_2003 = 
  data.table(
    year = 2003, 
    var_name = c(
      "dob_yy", "dob_mm",
      "state_postal", "county_fips",
      "state_postal_occ", "county_fips_occ",
      "birth_facility", "mage", 
      "city_res", 'place_fips', "restatus",
      "mrace", "mhisp",
      "mar", 
      "meduc","umeduc",
      "fage",
      "frace","fhisp",
      "live_birth_order", "total_birth_order",
      "apgar5", "sex",
      "gestation", "dbwt",
      "delmeth",
      # New indicator variables 
      "plurality","febrile","meconium","membrane_rupture","abruptio_placentae","placenta_previa","excessive_bleeding","mother_seizure","labor_under_3h","labor_over_20h","labor_dysfunc","breech","cephalopelvic_disproportion","cord_prolapse","anesthetic_comp","fetal_distress","labor_complication","baby_anemia","baby_injury","fetal_alcohol","baby_hyaline","meconoim_aspiration","vent_under_30m","vent_over_30m","baby_seizures","baby_other_abn","anencephaly","spina_bifida","hydrocephalus","microcephalus","baby_other_cent_nerv","heart_malform","baby_other_circ","rectal_atresia","tracheo","omphalocele","baby_other_gastro","malformed_genitals","renal_agenesis","baby_other_urogenital","cleft_lip","polydactyly","club_foot","baby_hernia","baby_other_muscl","downs_syndr","baby_other_chromo","baby_other_cong",
      'tobacco','tobacco_recode',
      'alcohol'
    ),
    var_desc = c(
      "birth year", "birth month",
      "residence state postal code", "residence county fips",
      "occurence state postal code", "occurence county fips",
      "birth place", "mother's age (recode 41)",
      "city of residence (mother)", 'place fips (mother residence)', "residence status",
      "mother's race recode", "mother hispanic origin",
      "mother's marital status", 
      "mother's education revised","mother's education unrevised",
      "father's age (recode 11)",
      "father's race recode", "father hispanic origin",
      "Sum of all previous live births", "Sum of all previous pregnancies",
      "Five minute APGAR score", "sex of infant",
      "Computed gestation", "Birthweight",
      "Delivery Method",
      # New indicator variables 
      "plurality","febrile","meconium","membrane_rupture","abruptio_placentae","placenta_previa","excessive_bleeding","mother_seizure","labor_under_3h","labor_over_20h","labor_dysfunc","breech","cephalopelvic_disproportion","cord_prolapse","anesthetic_comp","fetal_distress","labor_complication","baby_anemia","baby_injury","fetal_alcohol","baby_hyaline","meconoim_aspiration","vent_under_30m","vent_over_30m","baby_seizures","baby_other_abn","anencephaly","spina_bifida","hydrocephalus","microcephalus","baby_other_cent_nerv","heart_malform","baby_other_circ","rectal_atresia","tracheo","omphalocele","baby_other_gastro","malformed_genitals","renal_agenesis","baby_other_urogenital","cleft_lip","polydactyly","club_foot","baby_hernia","baby_other_muscl","downs_syndr","baby_other_chromo","baby_other_cong",
      'tobacco use', 'tobacco use (2003 birth certificate)',
      'alcohol use'
    ),
    start_pos = c(
      15, 19, 
      109, 114,
      30,37,
      42, 89,
      117, 120, 138,
      143, 148,
      153, 
      155, 156,
      186,
      191, 195,
      212, 217,
      415, 436,
      451, 463,
      401,
      423,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,483,484,485,486,487,488,489,490,491,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,
      290, 294,
      295
    ),
    end_pos = c(
      18, 20, 
      110, 116,
      31, 39,
      42, 90,
      119, 124, 138,
      143, 148,
      153,
      155, 157,
      187,
      191, 195,
      212, 217,
      416, 436,
      452, 466,
      401,
      423,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,483,484,485,486,487,488,489,490,491,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,
      290,294,
      295
    ),
    var_length = c(
      4, 2,
      2, 3,
      2, 3, 
      1, 2,
      3, 5, 1,
      1, 1,
      1,
      1, 2,
      2,
      1, 1,
      1, 1,
      2, 1,
      2, 4,
      1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,
      1
    ),  
    var_codes = list(
      data.table(code = 2003, desc = "year"),
      data.table(code = "01-12", desc = "month"),
      data.table(code = "AK", desc = "postal code"),
      data.table(code = "000", desc = "county fips"),
      data.table(code = "AK", desc = "postal code"),
      data.table(code = "000", desc = "county fips"),
      data.table(code = c(1:5,9), desc = c(
        "Hospital", "Freestanding Birthing Center", "Clinic / Doctor’s Office",
        "Residence", "Other", "Unknown"
      )),
      data.table(code = c("01","02-41"), desc = c("Under 15","Code +13 years")),
      data.table(code = c("nnn", "999",'ZZZ'), desc = c("Cities","Balance of county", "Foreign")),
      data.table(
        code = c("nnnnn", "99999", "00000"), 
        desc = c("Cities","Balance of county", "Foreign")
      ),
      data.table(code = 1:4, desc = c("resident","intrastate nonres","interstate nonres","foreign")),
      data.table(code = c(1:4), desc = c("White","Black","American Indian","Asian or Pacific Islander")),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = c(1,2,9), desc = c("Married","Unmarried","Unknown")),
      data.table(code = c(1:9), desc = c(
        "8th grade or less"
        ,"9th through 12th grade with no diploma"
        ,"High school graduate or GED completed"
        ,"Some college credit, but not a degree"
        ,"Associate degree (AA,AS)"
        ,"Bachelor’s degree (BA, AB, BS)"
        ,"Master’s degree (MA, MS, MEng, MEd, MSW, MBA)"
        ,"Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)"
        ,"Unknown"
      )),
      data.table(code = c("00","01-08","09",10:17,99), desc = c(
        "No formal education"
        ,"8	Years of elementary school"
        ,"1 year of high school"
        ,"2 years of high school"
        ,"3 years of high school"
        ,"4 years of high school"
        ,"1 year of college"
        ,"2 years of college"
        ,"3 years of college"
        ,"4 years of college"
        ,"5 or more years of college"
        ,"Not stated"
      )),
      data.table(code = c(1:11), desc = c(
        "Under 15 years","15-19 years","20-24 years","25-29 years","30-34 years",
        "35-39 years","40-44 years","45-49 years","50-54 years","55-98 years", "Unknown"
      )),
      data.table(code = c(1:4,9), desc = c(
        "White","Black","American Indian","Asian or Pacific Islander", "Unknown"
      )),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = c("1-7","8","9"), desc = c("Sum of all live births", "Eight or more","Unknown")),
      data.table(code = c("1-7","8","9"), desc = c("Sum of all pregnancies", "Eight or more","Unknown")),
      data.table(code = c("00-10", 99), desc = c("Score", "Unknown")),
      data.table(code = c("M","F"), desc = c("Male", "Female")),
      data.table(code = c("17-47", 99), desc = c("Weeks of gestation", "Unknown")),
      data.table(code = c("0000-9998", 9999), desc = c("Birth weight in grams", "Unknown")),
      data.table(code = c(1:7), desc = c(
        "Vaginal", "Vaginal after previous c-section","Primary C-section","Repeat C-section","Unknown",
        "Vaginal (2003 only)", "C-section (2003 only)"
      )),
      data.table(code = "1-5", desc = "number of babies"),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,8,9), desc = c('reported','not reported', 'not on birth certificate', 'not classifiable')),
      data.table(code = c(1,2,9), desc = c('reported','not reported', 'unknown/not stated')),
      data.table(code = c('Y','N','U'), desc = c('reported','not reported', 'unknown/not stated')),
      data.table(code = c(1,2,9), desc = c('reported','not reported', 'unknown/not stated'))
    )
  )

natality_dct = 
  rbind(
    mutate(natality_dct_1992, year = 1990),
    mutate(natality_dct_1992, year = 1991),
    natality_dct_1992,
    mutate(natality_dct_1992, year = 1993),
    mutate(natality_dct_1992, year = 1994),
    mutate(natality_dct_1992, year = 1995),
    mutate(natality_dct_1992, year = 1996),
    mutate(natality_dct_1992, year = 1997),
    mutate(natality_dct_1992, year = 1998),
    mutate(natality_dct_1992, year = 1999),
    mutate(natality_dct_1992, year = 2000),
    mutate(natality_dct_1992, year = 2001),
    mutate(natality_dct_1992, year = 2002),
    natality_dct_2003,
    mutate(natality_dct_2003, year = 2004),
    mutate(natality_dct_2003, year = 2005),
    mutate(natality_dct_2003, year = 2006),
    mutate(natality_dct_2003, year = 2007),
    mutate(natality_dct_2003, year = 2008),
    mutate(natality_dct_2003, year = 2009),
    mutate(natality_dct_2003, year = 2010),
    mutate(natality_dct_2003, year = 2011),
    mutate(natality_dct_2003, year = 2012),
    mutate(natality_dct_2003, year = 2013)
  )

save(natality_dct, file = here("data/health-restricted/natality-dct.RData"))
load(file = here("data/health-restricted/natality-dct.RData"))



# Mortality data dictionary
# Variables needed: 
#   - Year, month of death 
#   - County of residence, county of occurence 
#   - Cause of death code
#   - Residence status (?)
#   - Age
#   - Sex
mortality_dct_1992 = 
  data.table(
    year = 1992, 
    var_name = c(
      'GEOID', 'GEOID_occ',
      'month_death',
      'age',
      'inf_age',
      'sex',
      'race',
      'hisp',
      'ucod',
      'inf_cod'
    ),
    var_desc = c(
      'county of residence', 'county of occurence',
      'month of death',
      'age',
      'infant age recode 22',
      'sex',
      'detailed race',
      'hispanic origin',
      'cause of death',
      '61 category infant cause of death'
    ),
    start_pos = c(
      124, 119,
      55,
      64,
      73,
      59,
      60,
      80,
      142,
      154
    ),
    end_pos = c(
      128, 123,
      56,
      66,
      74,
      59,
      61,
      81,
      145,
      156
    ),
    var_length = c(
      5,5,
      2,
      3,
      2,
      1,
      1,
      2,
      4,
      3
    ),  
    var_codes = list(
      data.table(code = '00000', desc = "county fips code"),
      data.table(code = '00000', desc = "county fips code"),
      data.table(code = '00', desc = "month of death"),
      data.table(code = '000', desc = "Complicated coding, see documentation"),
      data.table(code = 1:22, desc = c(
        '<1hr','1-23hrs', 
        paste(1:6, 'days'),
         '7-13 days','14-20 days','21-27 days',
         paste(1:11, 'months')
      )),
      data.table(code = 1:2, desc = c('M','F')),
      data.table(code = 1:9, desc = c(
        'white','black','native american','chinese','japanese',
        'hawaiian','filipino','other asian/pacific islander','other'
      )),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = 1234, desc = 'ICD-9 code'),
      data.table(code = '000', desc = 'See documentation for codes')
    )
  )
mortality_dct_2003 = 
  data.table(
    year = 2003, 
    var_name = c(
      'GEOID', 'GEOID_occ',
      'month_death',
      'age',
      'inf_age',
      'sex',
      'race',
      'hisp',
      'ucod',
      'inf_cod'
    ),
    var_desc = c(
      'county of residence', 'county of occurence',
      'month of death',
      'age',
      'infant age recode 22',
      'sex',
      'detailed race',
      'hispanic origin',
      'cause of death',
      '130 category infant cause of death'
    ),
    start_pos = c(
      33, 21,
      65,
      70,
      81,
      69,
      445,
      488,
      146,
      157
    ),
    end_pos = c(
      37, 25,
      66,
      73,
      82,
      69,
      446,
      488,
      149,
      159
    ),
    var_length = c(
      5,5,
      2,
      3,
      2,
      1,
      1,
      1,
      4,
      3
    ),  
    var_codes = list(
      data.table(code = '00000', desc = "county fips code"),
      data.table(code = '00000', desc = "county fips code"),
      data.table(code = '00', desc = "month of death"),
      data.table(code = '000', desc = "Complicated coding, see documentation"),
      data.table(code = 1:22, desc = c(
        '<1hr','1-23hrs', 
        paste(1:6, 'days'),
         '7-13 days','14-20 days','21-27 days',
         paste(1:11, 'months')
      )),
      data.table(code = 1:2, desc = c('M','F')),
      data.table(code = 1:9, desc = c(
        'white','black','native american','chinese','japanese',
        'hawaiian','filipino','other asian/pacific islander','other'
      )),
      data.table(code = c(0:5,9), desc = c(
        "Non-Hispanic","Mexican","Puerto Rican","Cuban","Central American"
        ,"Other and Unknown Hispanic","Origin unknown or not stated"
      )),
      data.table(code = 1234, desc = 'ICD-10 code'),
      data.table(code = '000', desc = 'See documentation for codes')
    )
  )


mortality_dct = 
  rbind(
    mutate(mortality_dct_1992, year = 1990),
    mutate(mortality_dct_1992, year = 1991),
    mortality_dct_1992,
    mutate(mortality_dct_1992, year = 1993),
    mutate(mortality_dct_1992, year = 1994),
    mutate(mortality_dct_1992, year = 1995),
    mutate(mortality_dct_1992, year = 1996),
    mutate(mortality_dct_1992, year = 1997),
    mutate(mortality_dct_1992, year = 1998),
    mutate(mortality_dct_1992, year = 1999),
    mutate(mortality_dct_1992, year = 2000),
    mutate(mortality_dct_1992, year = 2001),
    mutate(mortality_dct_1992, year = 2002),
    mortality_dct_2003,
    mutate(mortality_dct_2003, year = 2004),
    mutate(mortality_dct_2003, year = 2005),
    mutate(mortality_dct_2003, year = 2006),
    mutate(mortality_dct_2003, year = 2007),
    mutate(mortality_dct_2003, year = 2008),
    mutate(mortality_dct_2003, year = 2009),
    mutate(mortality_dct_2003, year = 2010),
    mutate(mortality_dct_2003, year = 2011),
    mutate(mortality_dct_2003, year = 2012),
    mutate(mortality_dct_2003, year = 2013)
  )

save(mortality_dct, file = here("data/health-restricted/mortality-dct.RData"))
load(file = here("data/health-restricted/mortality-dct.RData"))