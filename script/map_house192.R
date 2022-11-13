library(tidyverse)
library(sf)
library(ggmap)
library(ggsflabel)

current_routes <- st_read("shapefile/routes_current.geojson")
trolley <- current_routes %>% filter(District_1 == "ELM" | Route %in% c('10','15'))

draft_routes <- st_read("shapefile/routes_draft_f.geojson.geojson")
draft_microtransit <- st_read("shapefile/routes_draft_micro.geojson")

stations <- st_read("shapefile/Highspeed_Stations.geojson") %>% 
  bind_rows(st_read("shapefile/Regional_Rail_Stations.geojson"))

house192 <- st_read("shapefile/pa_house_districts.geojson") %>% 
  filter(leg_distri == 192)

bbox <- setNames(house192 %>% st_bbox(), c("left","bottom","right","top"))
bbox[1] <- bbox[1] - .01
bbox[2] <- bbox[2] - .01
bbox[3] <- bbox[3] + .01
bbox[4] <- bbox[4] + .01

routes_pal <- c("#a3172e", "#6756a6", "#00afac", "#f8bc16")

ggmap(get_stamenmap(bbox, zoom = 14, maptype = "toner-lite")) +
  geom_sf(data = house192, color = "black", alpha = 0.5, lwd = .5, inherit.aes = F) +
  geom_sf(data = trolley, color = "#51822b", size = 2, inherit.aes = F) +
  geom_sf(data = draft_routes, aes(color = freq, size = freq), inherit.aes = F) +
  scale_color_manual(values = routes_pal) +
  scale_size_manual(values = (c(2, 1.5, 1, 0.5))) +
  geom_sf_label_repel(data = st_intersection(draft_routes, house192),
                     aes(label = routenum), color = "black",
                     inherit.aes = F) +
  geom_sf(data = stations, inherit.aes = F) +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right")

# ggsave("image/img_draft.png",
#        scale = 3)
