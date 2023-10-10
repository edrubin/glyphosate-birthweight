#-------------------------------------------------------
# Getting pesticide data from USGS
#-------------------------------------------------------
# Loading required packages
if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  data.table, janitor, here, dplyr, stringr, fst, 
  curl,sf, tigris, purrr
)
options(tigris_use_cache =TRUE)

#-------------------------------------------------------

# Reading files from earlier years
pest = read.fst(
  here('data/download-script/usgs-pesticides-raw.fst'),
  as.data.table = TRUE
)

# 2013-2017 Data is in it's own file
pest2 = 
  fread(
    here('data/download-manual/est_pest_use_raw_2013_2017.txt'),
    colClasses = c(
      "STATE_FIPS_CODE"="character",
      "COUNTY_FIPS_CODE"="character"), 
    header = TRUE, 
    sep = "\t"
  ) |>
  clean_names()

# Combining
pest_all = rbind(pest, pest2, use.names = TRUE)

# Loading table with insecticide/herbicide/fungicide classifications
pest_class = 
  fread(here("data/download-manual/pesticide_classification.txt"),header = F, sep = "~") |>
  setnames(new = c("compound","class"))


# Adding in some chemicals of interest, classifying the rest
# County and State FIPS codes don't have leading 0's in raw data
pest_dt = 
  pest_all |>
  merge(
    pest_class,
    by = "compound",
    all.x = T
  ) |>
  setnames(
    old = c("state_fips_code","county_fips_code"),
    new = c("state_fips","county_fips")
  ) %>%
  .[,.(
    GEOID = paste0(state_fips,county_fips),
    year,
    pest_class = 
      case_when(
        compound %in% c(
          "GLYPHOSATE","ALACHLOR","CYANAZINE","FLUAZIFOP",
          "METOLACHLOR","ATRAZINE","METRIBUZIN","NICOSULFURON")
        ~ str_to_lower(compound),
        !is.na(class) ~ class,
        TRUE ~ "not classified"
      ),
    epest_000kg = epest_high_kg/1000
  )] |>
  dcast( # Casting to wide and summing together for each pest_class
    GEOID + year ~ pest_class,
    value.var = "epest_000kg",
    fun.aggregate = \(x){sum(x,na.rm=T)}
  ) |>
  clean_names() |>
  setnames(old = "geoid",new="GEOID")|> # I know...ugh...
  setkey(GEOID, year)

# Updating GEOID's that have changed 
# 12025->12086, 46102->46113
  #pest_dt[GEOID %in% c(12025, 12086), .N, keyby = year][,.N,by = N]
  #pest_dt[GEOID %in% c(46102, 46113), .N, keyby = year][,.N,by = N]
  pest_dt[,
    GEOID := fcase(
      GEOID == '12025', '12086', 
      GEOID == '46102', '46113', 
      !(GEOID %in% c(12025,46102)), GEOID
    )
  ]

# Turning it into a balanced panel
states_sf = 
  states(
    year = 2010,
    cb = TRUE  
  ) |>
  filter( # Limiting to continental US
    !(STATE %in% c("02","15","60","66","69","72","78"))
  ) |>
  clean_names()
county_sf =
  map_dfr(
    states_sf$state,
    counties,
    year = 2010,
    cb = TRUE
  ) |> 
  clean_names() |>
  mutate(
    GEOID = paste0(statefp, countyfp)
  ) 

pest_dt_balanced = 
  CJ(
    GEOID = county_sf$GEOID, 
    year = unique(pest_dt$year)
  ) |>
  merge(
    pest_dt,
    by = c('GEOID','year'),
    all.x = TRUE
  )

# Filling in NA's with zeros
for (j in names(pest_dt_balanced)[-(1:2)]) {
  set(pest_dt_balanced,which(is.na(pest_dt_balanced[[j]])),j,0)
}


# Saving the data
fwrite(
  pest_dt_balanced, 
  here("data/raw/est_pest_use.csv")
)
write.fst(
  pest_dt_balanced, 
  here("data/raw/est_pest_use.fst")
)
