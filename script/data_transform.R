library(tidyverse)
library(sf)

### draft routes ###

routes_draft <- st_read("shapefile/routes_draft.geojson")

routes_draft <- routes_draft %>% mutate(
  freqname = word(FIRST_full_name, 1, sep = "_") %>% word(1, sep = "-"),
  routenum = str_trim(word(FIRST_full_name, 2, sep = "_") %>% word(1, sep = "-"))) %>% 
  mutate(freqname = case_when(
    word(routenum, 1) == "599" ~ "15 Max",
    word(routenum, 1) == "797" ~ "30 Max",
    word(routenum, 1) == "798" ~ "30 Max",
    word(routenum, 1) == "699" ~ "60 Max",
    word(routenum, 1) == "799" ~ "60 Max",
    word(routenum, 1) == "59" ~ "10 Max",
    word(routenum, 1) == "66" ~ "15 Max",
    word(routenum, 1) == "75" ~ "30 Max",
    TRUE ~ freqname
  ),
  freq = as.numeric(substr(freqname, 1, 2))) %>%
  filter(str_sub(FIRST_full_name, 1, 4) != "LUCY") %>% 
  select(-FIRST_full_name) %>% arrange(freq, freqname, geometry) %>% 
  st_simplify(dTolerance = 10)

routes_draft %>% st_transform(2272) %>%  st_write("shapefile/routes_draft_m.geojson")

### microtransit ###
draft_microtransit <- st_read("shapefile/routes_draft_micro.geojson") %>% 
  st_tranform(2272)

### current routes ###

routes_current <- st_read("shapefile/routes_current.geojson")
routes_current_freq <- read_csv("data/current_routes.csv")

routes_current <- routes_current %>% inner_join(routes_current_freq) %>% 
  rename(routenum = Route) %>% mutate(freqname = paste(freq, "Max"))

routes_current %>% st_transform(2272) %>% st_write("shapefile/routes_current_m.geojson")
