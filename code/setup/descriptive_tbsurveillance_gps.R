library(tidyverse)
library(sf)
library(ggmap)
library(raster)

# ggmap::register_google(key = "AIzaSyDBz7TtWW69ZGDHmThLgSzEuVEXrEt8pEQ", write = TRUE)

# ---------------------------------------------------------------------------- #
# Read geotagged TB notifications

dat_gps <- readRDS(here::here("bttbsurveillance/data","bttbsurveillance_with_gpscoords.rds")) %>% 
  sf::st_as_sf(coords = c("lng","lat"), crs = 4326, remove = FALSE)

# ---------------------------------------------------------------------------- #
# Setup map context

centre <- c(lon = mean(dat_gps$lng), lat = mean(dat_gps$lat))
blt_lines <- get_map(location = centre, source = "stamen", maptype = "terrain", zoom = 12)

# Malawi population count raster
pop.path <- here::here("data","mwi_ppp_2020_UNadj.tif")
popMLW <- raster::raster(x = pop.path)

# Crop to Blantyre with extent from ggmap zoom
bb <- attr(blt_lines, "bb")
e <- raster::extent(c(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat))

popBLT <- raster::crop(popMLW, e)
plot(popBLT)

popBLT_spdf <- as(popBLT, "SpatialPixelsDataFrame")
popBLT_df <- as.data.frame(popBLT_spdf)

# Plot data locations 
ggmap(blt_lines, 
      base_layer = ggplot(dat_gps)) +
  geom_sf(colour = "indianred", alpha = 0.3, cex = 0.7) +
  labs(x = "", y = "", subtitle = "Geotagged TB notifications between 2015-2020") -> map_cases
map_cases

ggsave(here::here("figures","tb_notifications_gps_2015_2020.png"), 
       height = 6, width = 8)


ggmap(blt_lines, 
      base_layer = ggplot(dat_gps)) +
  geom_raster(data = popBLT_df, aes(x = x, y = y, fill = mwi_ppp_2020_UNadj), alpha = 0.5) +
  scale_fill_viridis_c(trans = "sqrt", option = "magma", direction = -1, begin = 0.2) + 
  labs(fill = "Population", colour = "Case") +
  labs(x = "", y = "", subtitle = "Blantyre area estimated population") +
  coord_quickmap() -> map_pops
map_pops
ggsave(here::here("figures","blantyre_pop_2020.png"), 
       height = 6, width = 8)

ggmap(blt_lines, 
      base_layer = ggplot(dat_gps)) +
  geom_sf(colour = "black", alpha = 0.5, cex = 0.5, pch = 20) +
  labs(x = "", y = "", subtitle = "Geotagged TB notifications between 2015-2020") +
  coord_sf() -> map_cases1
map_cases1
ggsave(here::here("figures","tb_notifications_gps_2015_2020.png"), 
       height = 6, width = 8)

ggmap(blt_lines, 
      base_layer = ggplot(dat_gps)) +
  geom_raster(data = popBLT_df, aes(x = x, y = y, fill = mwi_ppp_2020_UNadj), alpha = 0.5) +
  scale_fill_viridis_c(trans = "sqrt", option = "magma", direction = -1, begin = 0.2) + 
  labs(fill = "Population", colour = "Case") +
  geom_sf(colour = "black", alpha = 0.5, cex = 0.5, pch = 20) +
  labs(x = "", y = "", subtitle = "Geotagged TB notifications between 2015-2020") +
  coord_sf() -> map_cases2
map_cases2
ggsave(here::here("figures","tb_notifications_gps_2015_2020_wpop.png"), 
       height = 6, width = 8)
