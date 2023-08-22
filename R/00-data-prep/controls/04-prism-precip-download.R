# Downloads precipitation data from PRISM database
library(pacman)
p_load(
  here, fst, data.table, prism, stars
)

# Setting the folder where the data should be stored
options(prism.path = here('data/prism'))

# Downloading monthly data from 1990-2019
get_prism_monthlys(
  type = 'ppt',
  years = 1990:2019,
  mon = 1:12
)

# Checking 
ppt_example = read_stars(here(
  "data/prism/PRISM_ppt_stable_4kmM3_199001_bil/PRISM_ppt_stable_4kmM3_199001_bil.bil"
))

plot(ppt_example)



