# Downloads precipitation data from PRISM database
library(pacman)
p_load(
  here, fst, data.table, prism, stars
)

# Setting the folder where the data should be stored
options(prism.path = here('data/watershed/prism'))

# Downloading monthly data from 1990-2019
get_prism_monthlys(
  type = 'ppt',
  years = 1990:2019,
  mon = 1:12
)

