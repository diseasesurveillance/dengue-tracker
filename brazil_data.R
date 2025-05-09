############################################################################################
# Read data d and map map
############################################################################################

map <- st_read("map/map.shp", quiet = TRUE)

disease <- "dengue"
format <- "csv"
ew_start <- 1
ew_end <- 52
ey_start <- 2020
ey_end <- 2023

d <- vroom(file = paste0("data/dallmuni", ew_start, ey_start, ew_end, ey_end, ".csv"))
names(d)

# tibble as data.frame
d <- as.data.frame(d)

# Specify d$idtime, d$nametime, d$idloc, map$idloc, map$nameloc, map$idloc2, map$nameloc2
dmapvbles <- c("data_iniSE", "data_iniSE", "geocode", "code_mn", "name_mn", "cod_stt", "nam_stt")
# # code_muni state
dmap <- fnSetTimeLocVariablesDataMap(d, map, dmapvbles)
d <- dmap[[1]]
map <- dmap[[2]]

state_code <- map |> select(cod_stt, abbrv_s)

state_code <- tibble(
  names = names(state_code$cod_stt),
  state_n = unlist(state_code$cod_stt),
  short = unlist(state_code$abbrv_s)
) |> unique()