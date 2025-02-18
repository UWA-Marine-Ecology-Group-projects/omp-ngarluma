###
# Project: Parks Ngarluma
# Data:    250m bathy
# Task:    Sea level change - current and 2 historical levels
# Author:  Claude Spencer
# Date:    September 2023
##

library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)

bathy <- readRDS("data/spatial/rasters/GA_250m_bathy-trimmed.RDS") %>%
  rast(crs = "epsg:4326") %>%
  crop(ext(116.5012, 117.6063, -20.79125, -19)) %>%
  # clamp(lower = -121, values = F) %>%
  trim()

slope <- terrain(bathy, v = "slope")
aspect <- terrain(bathy, v = "aspect")
hillshade <- shade(slope, aspect, angle = 70, direction = 0)
hilldf <- as.data.frame(hillshade, xy = T)

bathdf <- bathy %>%
  as.data.frame(xy = T, na.rm = T)

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif", crs = 4283) %>%
  dplyr::filter(!FEAT_CODE %in% "sea") %>%
  glimpse()

p1 <- ggplot() +
  geom_tile(data = bathdf, aes(x = x, y = y, fill = tile2c), alpha = 1) +
  scale_fill_gradient2(low = "#08519C", mid = "#9ECAE1", high = "#EFF3FF") +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  new_scale_fill() +
  # geom_tile(data = hilldf, aes(x = x, y = y, fill = hillshade), show.legend = F, alpha = 0.2) +
  # scale_fill_gradient(low = "black", high = "white") +
  # new_scale_fill() +
  geom_contour(data = bathdf, aes(x = x, y = y, z = tile2c),
               breaks = c(-30, -120), colour = "black", size = 0.2) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.2) +
  coord_sf(crs = 4326, xlim = c(116.56, 117.54), ylim = c(-20.79125, -19.08875)) +
  theme_minimal()
p1

p2 <- ggplot() +
  geom_tile(data = bathdf, aes(x = x, y = y, fill = tile2c), alpha = 1) +
  scale_fill_gradient2(low = "#08519C", mid = "#9ECAE1", high = "#EFF3FF") +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  new_scale_fill() +
  geom_tile(data = hilldf, aes(x = x, y = y, fill = hillshade), show.legend = F, alpha = 0.2) +
  scale_fill_gradient(low = "black", high = "white") +
  new_scale_fill() +
  geom_contour(data = bathdf, aes(x = x, y = y, z = tile2c),
               breaks = c(-30, -120), colour = "black", size = 0.2) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.2) +
  coord_sf(crs = 4326, xlim = c(116.8, 117.2), ylim = c(-20.68, -20.1)) +
  theme_minimal()
p2
