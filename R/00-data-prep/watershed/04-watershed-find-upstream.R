# Creates a table of all upstream and all downstream hydrosheds 
# Runtime: 1 min
library(pacman)
p_load(
  data.table, here, fixest, ggplot2, 
  tigris, magrittr, sf, tidyr, dplyr, HydroCode,
  janitor, fst, stringr
)

# Loading HydroBASINS data limiting to those in the continental US
hydrobasin_sf = 
  read_sf(here("data/spatial/hydrobasins/hybas_lake_na_lev08_v1c.shp")) |>
  st_transform(crs = 2163) |>
  clean_names()

# DT with distances 
hybas_dist_dt = read.fst(
  path = here("data-clean/watershed/hybas-dist-dt.fst"),
  as.data.table = TRUE
)

# Turning it into a data table to run faster
hydrobasin_dt = 
  merge(
    data.table(hydrobasin_sf)[,-'geometry'],
    hybas_dist_dt[,.(hybas_id, dist_next_down_km = dist_km)], 
    by = 'hybas_id',
    all.x = TRUE
  )

# Checking histogram of distances between watersheds and their next downstream neighbor
ggplot(data = hydrobasin_dt, aes(x = dist_next_down_km)) + 
  geom_histogram(bins = 500) + 
  geom_vline(xintercept = 250, linetype = 'dashed')

hydrobasin_dt[,
  next_down := fcase(
    dist_next_down_km <= 100, next_down, 
    default = 0
  )
]

# Loading weights to limit to watersheds in continental US
area_weight_dt = 
  read_fst(
    here("data-clean/watershed/weights/hydrobasin/hydrobasin-area-weights.fst"),
    as.data.table = TRUE
  ) |>
  setnames(old = "watershed_weight",new = "area_weight") |>
  setkey(hybas_id,geoid)

# DT of unique hybas ID's within continental US
hybas_id_dt = data.table(end_id = area_weight_dt$hybas_id |> unique())

# Starting the upstream tracing using the `next_down` column
path_dt = 
  merge(
    hybas_id_dt, 
    hydrobasin_dt[,.(hybas_id1 = hybas_id, next_down, dist_km1 = dist_next_down_km)],
    by.x = "end_id",
    by.y = "next_down",
    all.x = TRUE
  )


# Function that finds the next upstream watersheds
#old_dt = path_dt
next_upstream = function(old_dt){
  # Getting the number of iterations already complete
  i = colnames(old_dt) |> str_extract_all("\\d+") |> as.numeric() |> max(na.rm = T)
  # Merging next level upstream 
  new_dt = 
    merge(
      old_dt |> setnames(old = paste0("dist_km",i), new = 'dist_km_current'), 
      hydrobasin_dt[,.(hybas_id, next_down, dist_next_down_km)] |> 
        setnames(old = "hybas_id", new = paste0("hybas_id",i+1)),
      by.x = paste0("hybas_id",i),
      by.y = "next_down",
      all.x = TRUE
    ) 
  
  new_dt[,dist_km_upd := dist_km_current + dist_next_down_km]
  
  new_dt |>
    setnames(
      old = c('dist_km_current','dist_km_upd'),
      new = c(paste0("dist_km",i),paste0("dist_km",i+1))
    )
  
  new_dt[,dist_next_down_km:=NULL]
  
  return(new_dt)
}  

# Iterate, getting the next upstream until 
# there are no more upstream
continue = TRUE
while(continue){
  # Iterating upstream once
  path_dt = next_upstream(path_dt)
  # Check to see that there were at least some watersheds matched
  i = colnames(path_dt) |> str_extract_all("\\d+") |> as.numeric() |> max(na.rm = T)
  not_na = !is.na(path_dt[[paste0("hybas_id",i)]])
  continue = sum(not_na) > 0
}

# Melting into long format
upstream_dt = 
  melt(
    path_dt[
      ,.(end_id, 
        hybas_id1, dist_km1, 
        hybas_id2, dist_km2, 
        hybas_id3, dist_km3, 
        hybas_id4, dist_km4, 
        hybas_id5, dist_km5, 
        hybas_id6, dist_km6, 
        hybas_id7, dist_km7, 
        hybas_id8, dist_km8, 
        hybas_id9, dist_km9, 
        hybas_id10, dist_km10,
        hybas_id11, dist_km11, 
        hybas_id12, dist_km12, 
        hybas_id13, dist_km13, 
        hybas_id14, dist_km14,
        hybas_id15, dist_km15,
        hybas_id16, dist_km16, 
        hybas_id17, dist_km17, 
        hybas_id18, dist_km18, 
        hybas_id19, dist_km19, 
        hybas_id20, dist_km20,
        hybas_id21, dist_km21, 
        hybas_id22, dist_km22, 
        hybas_id23, dist_km23, 
        hybas_id24, dist_km24,
        hybas_id25, dist_km25,
        hybas_id26, dist_km26,
        hybas_id27, dist_km27,
        hybas_id28, dist_km28,
        hybas_id29, dist_km29, 
        hybas_id30, dist_km30,
        hybas_id31, dist_km31, 
        hybas_id32, dist_km32, 
        hybas_id33, dist_km33, 
        hybas_id34, dist_km34,
        hybas_id35, dist_km35,
        hybas_id36, dist_km36,
        hybas_id37, dist_km37,
        hybas_id38, dist_km38,
        hybas_id39, dist_km39, 
        hybas_id40, dist_km40,
        hybas_id41, dist_km41, 
        hybas_id42, dist_km42, 
        hybas_id43, dist_km43, 
        hybas_id44, dist_km44,
        hybas_id45, dist_km45,
        hybas_id46, dist_km46,
        hybas_id47, dist_km47,
        hybas_id48, dist_km48,
        hybas_id49, dist_km49, 
        hybas_id50, dist_km50,
        hybas_id51, dist_km51, 
        hybas_id52, dist_km52, 
        hybas_id53, dist_km53, 
        hybas_id54, dist_km54,
        hybas_id55, dist_km55,
        hybas_id56, dist_km56,
        hybas_id57, dist_km57,
        hybas_id58, dist_km58,
        hybas_id59, dist_km59, 
        hybas_id60, dist_km60,
        hybas_id61, dist_km61, 
        hybas_id62, dist_km62, 
        hybas_id63, dist_km63, 
        hybas_id64, dist_km64,
        hybas_id65, dist_km65,
        hybas_id66, dist_km66,
        hybas_id67, dist_km67,
        hybas_id68, dist_km68,
        hybas_id69, dist_km69, 
        hybas_id70, dist_km70,
        hybas_id71, dist_km71, 
        hybas_id72, dist_km72, 
        hybas_id73, dist_km73, 
        hybas_id74, dist_km74,
        hybas_id75, dist_km75,
        hybas_id76, dist_km76,
        hybas_id77, dist_km77,
        hybas_id78, dist_km78,
        hybas_id79, dist_km79, 
        hybas_id80, dist_km80,
        hybas_id81, dist_km81, 
        hybas_id82, dist_km82, 
        hybas_id83, dist_km83, 
        hybas_id84, dist_km84,
        hybas_id85, dist_km85,
        hybas_id86, dist_km86,
        hybas_id87, dist_km87,
        hybas_id88, dist_km88,
        hybas_id89, dist_km89, 
        hybas_id90, dist_km90,
        hybas_id91, dist_km91, 
        hybas_id92, dist_km92, 
        hybas_id93, dist_km93, 
        hybas_id94, dist_km94,
        hybas_id95, dist_km95,
        hybas_id96, dist_km96,
        hybas_id97, dist_km97,
        hybas_id98, dist_km98,
        hybas_id99, dist_km99, 
        hybas_id100, dist_km100,
        hybas_id101, dist_km101, 
        hybas_id102, dist_km102, 
        hybas_id103, dist_km103, 
        hybas_id104, dist_km104,
        hybas_id105, dist_km105,
        hybas_id106, dist_km106,
        hybas_id107, dist_km107,
        hybas_id108, dist_km108,
        hybas_id109, dist_km109, 
        hybas_id110, dist_km110,
        hybas_id111, dist_km111, 
        hybas_id112, dist_km112, 
        hybas_id113, dist_km113, 
        hybas_id114, dist_km114,
        hybas_id115, dist_km115,
        hybas_id116, dist_km116,
        hybas_id117, dist_km117,
        hybas_id118, dist_km118,
        hybas_id119, dist_km119, 
        hybas_id120, dist_km120,
        hybas_id121, dist_km121, 
        hybas_id122, dist_km122, 
        hybas_id123, dist_km123, 
        hybas_id124, dist_km124,
        hybas_id125, dist_km125,
        hybas_id126, dist_km126,
        hybas_id127, dist_km127
      )
    ], 
    id.vars = 'end_id',
    measure = patterns("^hybas_id", "^dist_km"), 
    value.name = c("hybas_id", "dist_km")
  )[!is.na(hybas_id) & !is.na(dist_km),.(
    hybas_id = end_id,
    hybas_id2 = hybas_id,
    dist = as.numeric(levels(variable))[variable],
    dist_km,
    upstream = FALSE,
    downstream = TRUE
  )] |> unique()

#ggplot() + 
#  geom_sf(
#    data = inner_join(
#      hydrobasin_sf,
#      upstream_dt[
#        hybas_id == 7080766011,
#        .(dist_km = median(dist_km,na.rm = TRUE)),
#        keyby = .(hybas_id, hybas_id2)
#      ],
#      by = c('hybas_id'='hybas_id2')
#    ), 
#    aes(fill = dist_km)
#  ) + 
#  geom_sf(data = filter(hydrobasin_sf, hybas_id == 7080766011), fill = 'red')

# Now to get the upstream by reversing logic, and local 
upstream_dt = 
  rbind(
    upstream_dt[,.(# First for downstream (this has already done) 
      hybas_id, 
      hybas_id2, 
      dist, dist_km,
      upstream, 
      downstream
    )],
    upstream_dt[,.(# Now adding upstream (reversing id order, making dist negative)
      hybas_id = hybas_id2, 
      hybas_id2 = hybas_id, 
      dist, dist_km = -dist_km,
      upstream = TRUE, 
      downstream = FALSE
    )], # Adding local in as well
    data.table(
      hybas_id = hybas_id_dt$end_id,
      hybas_id2 = hybas_id_dt$end_id,
      dist = 0, dist_km = 0,
      upstream = FALSE,
      downstream = FALSE
    )
  )

# Watersheds in the same counties 
same_geoid_dt = 
  merge(
    area_weight_dt[,.N,by=.(hybas_id, geoid)],
    area_weight_dt[,.N,by=.(hybas_id2 = hybas_id, geoid)],
    by = "geoid",
    allow.cartesian = TRUE
  ) %>% .[,':='(N.x=NULL, N.y = NULL)]

# Merging with the upstream data
upstream_dt = 
  merge(
    upstream_dt, 
    same_geoid_dt, 
    by = c("hybas_id","hybas_id2"),
    all.x = TRUE
  ) %>% .[,':='(
    local = !is.na(geoid),
    geoid = NULL
  )]

# Now for the weighting metric: 1/dist^(0.1)? Can change
dist_fn = function(x, exp = -1){
  (1 + x)^(exp)
}

# Plot of the weighting function
dist_wt_fn =
  ggplot() +
  stat_function(fun = dist_fn, args = list(exp = 0), color = "#57754d", size = 2) + 
  stat_function(fun = dist_fn, args = list(exp = -0.25), color = "#ffcc77", size = 2) +  
  stat_function(fun = dist_fn, args = list(exp = -2), color = "#ec7662", size = 2) + 
  xlim(0,15) + 
  labs(
    x = "Number of Watersheds",
    y = "Distance Weight"
  ) +
  theme_minimal(base_size = 16)
ggsave(
  dist_wt_fn, 
  filename = here("figures/watershed_plots/dist_wt_fn.jpeg"),
  width = 8,height = 6
)

# Getting dist for first up/down watershed outside of county
dist_up_dt = upstream_dt[
  upstream == TRUE & local == FALSE,
  .(first_dist_up = min(dist)), 
  by = hybas_id
]
dist_down_dt = upstream_dt[
  downstream == TRUE & local == FALSE,
  .(first_dist_down = min(dist)), 
  by = hybas_id
]

upstream_dt =
  merge(
    upstream_dt[,-"first_dist_up"], 
    dist_up_dt,
    by = "hybas_id",
    all.x = TRUE
  )
upstream_dt =
  merge(
    upstream_dt[,-"first_dist_down"], 
    dist_down_dt,
    by = "hybas_id",
    all.x = TRUE
  )

upstream_dt[,dist_non_local := fcase(
  upstream == TRUE & local == FALSE, dist - (first_dist_up - 1),
  downstream == TRUE & local == FALSE, dist - (first_dist_down - 1),
  default = 0
)]

# Adding to the data
upstream_dt[,':='(
  dist_wt_h = dist_fn(dist_non_local, exp = -5),
  dist_wt_m = dist_fn(dist_non_local, exp = -0.25),
  dist_wt_l = dist_fn(dist_non_local, exp = 0),
  dist_wt_05 = ifelse(dist_non_local > 0 & dist_non_local <= 5, 1, 0),
  dist_wt_10 = ifelse(dist_non_local > 5 & dist_non_local <= 10, 1, 0),
  dist_wt_15 = ifelse(dist_non_local > 10 & dist_non_local <= 15, 1, 0)
)]

# Breaks for distance bins 
dist_breaks = c(-Inf,-100, -50, 0, 50, 100, 150, 200, 250, 300, Inf)
# Creating distance bin variable 
upstream_dt[, dist_km_bin := cut(
  dist_km, 
  breaks = dist_breaks, 
  right = FALSE,
  labels = paste0('d',dist_breaks[1:length(dist_breaks)-1]) |> str_replace('-','n')
)]


# saving the results
write.fst(
  upstream_dt, 
  path = here("data-clean/watershed/upstream-dt-hydrobasin.fst")
)

upstream_dt =
read.fst(
  path = here("data-clean/watershed/upstream-dt-hydrobasin.fst"),
  as.data.table = TRUE
)







