library(tidyverse)
library(sf)
routes_pal <- c("#a3172e", "#6756a6", "#00afac", "#f8bc16")

routes_current <- st_read("shapefile/routes_current_m.geojson") %>% select(routenum, freqname, freq)
trolley <- routes_current %>% filter(routenum %in% c('10','11','13','15','34','36','101','102'))

routes_draft <- st_read("shapefile/routes_draft_m.geojson")
routes_micro <- st_read("shapefile/routes_draft_micro.geojson")

routes_draft <- routes_draft %>% bind_rows(trolley)

### create block group analysis layer

blockgroup <- st_read("shapefile/nhgis0017_shapefile_tl2020_420_blck_grp_2010/PA_blck_grp_2010_tl20.shp") %>% 
  filter(COUNTYFP10 %in% c('017', '029', '045', '091', '101')) %>% select(GEOID10, Shape_Area) %>% 
  st_transform(4326)

dvrpc_eta <- st_read("data/dvrpc_eta.geojson") %>% select(-objectid, -Shape__Area, -Shape__Length)
dvrpc_rtsp <- st_read("data/dvrpc_rtsp.geojson") %>% select(-objectid, -gid, -Shape__Length)
hta_index <- read_csv("data/htaindex2019_data_blkgrps_42.csv") %>% 
  filter(str_sub(blkgrp, 4, 6)%in% c('017', '029', '045', '091', '101')) %>% 
  mutate(blkgrp = gsub('"', '', blkgrp))

analysis <- blockgroup %>% inner_join(hta_index, by = c("GEOID10"="blkgrp"))

analysis <- blockgroup %>% left_join(dvrpc_eta %>% st_drop_geometry(), by = c("GEOID10" = "geoid10"))

analysis <- analysis %>% st_transform(2272)

### buffered bus routes according to analysis layer

routes_current_analysis <- st_join(analysis, routes_current %>% st_buffer(500)) %>% 
  arrange(GEOID10, freq) %>% group_by(GEOID10) %>% slice_head(n = 1)

ggplot(routes_current_analysis)

routes_draft_analysis <- st_join(analysis, routes_draft %>% st_buffer(500)) %>% 
  arrange(GEOID10, freq) %>% group_by(GEOID10) %>% slice_head(n = 1)

ggplot(routes_draft_analysis) + geom_sf(aes(fill = freqname), lwd=0) +
  scale_fill_manual(values = routes_pal)

### neighborhoods

nbhd <- st_read("shapefile/neighborhoods/Neighborhoods_Philadelphia.shp")
bbox_phl <- setNames(nbhd %>% st_bbox(), c("left","bottom","right","top"))
bbox_phl[1] <- bbox_phl[1] - .01
bbox_phl[2] <- bbox_phl[2] - .01
bbox_phl[3] <- bbox_phl[3] + .01
bbox_phl[4] <- bbox_phl[4] + .01

### does bus route go through Center City?



### council districts

council <- st_read("shapefile/Council_Districts_2016.geojson") %>% 
  select(DISTRICT) %>% st_transform(2272)

analyze_council_dist <- routes_draft_analysis %>% st_join(council) %>% 
  group_by(DISTRICT, freqname) %>% summarise(area = sum(Shape_Area)) %>% 
  filter(!is.na(DISTRICT))

analyze_council_dist %>% ggplot(aes(DISTRICT, area)) + 
  geom_col(aes(fill = freqname), position = "fill") +
  scale_y_continuous(labels = scales::percent)
