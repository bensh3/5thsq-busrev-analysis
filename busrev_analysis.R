library(tidyverse)
library(sf)

setwd("/Users/bensh/Documents/transit/SEPTA/busrev")

draft_routes <- st_read("busrev_draft.geojson")
draft_microtransit <- st_read("busrev_ondemand.geojson")

ggplot() +
  geom_sf(data=draft_routes) +
  geom_sf(data=draft_microtransit, alpha=0.3) +
  geom_sf_text(data=draft_microtransit, aes(label=FID_1))
