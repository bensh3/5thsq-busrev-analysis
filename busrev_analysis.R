library(tidyverse)
library(sf)
library(ggmap)

current_routes <- st_read("shapefile/Fall_2019_Routes.geojson")
trolley <- current_routes %>% filter(District_1 == "ELM" | Route %in% c('10','15'))
draft_routes <- st_read("shapefile/busrev_draft.geojson")
draft_microtransit <- st_read("shapefile/busrev_ondemand.geojson")
stations <- st_read("shapefile/Highspeed_Stations.geojson") %>% 
  bind_rows(st_read("shapefile/Regional_Rail_Stations.geojson"))

house192 <- st_read("shapefile/pa_house_districts.geojson") %>% 
  filter(leg_distri == 192)

draft_routes <- draft_routes %>% mutate(
  freq = word(FIRST_full_name, 1, sep = "_") %>% word(1, sep = "-"),
  routenum = word(FIRST_full_name, 2, sep = "_") %>% word(1, sep = "-")) %>% 
  mutate(freq = case_when(
    word(routenum, 1) == "599" ~ "15 Max",
    word(routenum, 1) == "797" ~ "30 Max",
    word(routenum, 1) == "798" ~ "30 Max",
    word(routenum, 1) == "699" ~ "60 Max",
    word(routenum, 1) == "799" ~ "60 Max",
    word(routenum, 1) == "59" ~ "10 Max",
    word(routenum, 1) == "66" ~ "15 Max",
    word(routenum, 1) == "75" ~ "30 Max",
    TRUE ~ freq
  )) %>%
  filter(str_sub(FIRST_full_name, 1, 4) != "LUCY")

bbox <- setNames(house192 %>% st_bbox(), c("left","bottom","right","top"))
bbox[1] <- bbox[1] - .01
bbox[2] <- bbox[2] - .01
bbox[3] <- bbox[3] + .01
bbox[4] <- bbox[4] + .01

busrev_pal <- c("#a3172e", "#6756a6", "#00afac", "#f8bc16")

ggmap(get_stamenmap(bbox, zoom = 14, maptype = "toner-lite")) +
  geom_sf(data = house192, alpha = 0, lwd = 2, inherit.aes = F) +
  geom_sf(data = trolley, color = "#51822b", lwd = 1, inherit.aes = F) +
  geom_sf(data = draft_routes, aes(color = freq), lwd = 1, inherit.aes = F) +
  scale_color_manual(values = busrev_pal) +
  geom_sf_label(data = draft_routes, aes(label = routenum), color = "black", 
                inherit.aes = F) +
  geom_sf(data = stations, inherit.aes = F) +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right")

ggsave("image/img_draft.png",
       scale = 3)
