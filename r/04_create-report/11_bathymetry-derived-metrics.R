###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Pressure data from AODN, formatted in 02_spatial-layers
# Task:    Create pressure plots (not used in this report)
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(ggnewscale)
library(patchwork)

# Set cropping extent - larger than most zoomed out plot
e <- ext(116.7, 117.7,-20.919, -20)

# Load necessary spatial files
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

site_limits = c(116.779, 117.544, -20.738, -20.282) # For Dampier match it to the first plot

aus_marine_parks <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp")

marine_parks <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Dampier")) %>%
  glimpse()

# Australian Marine Parks only (for separate ggplot legends)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  arrange(zone)

# Load 250m resolution spatial covariates
preds <- readRDS("data/spatial/rasters/DampierAMP_bathymetry-derivatives.rds")

# Load 30m bathymetry compilation dataset
mb <- rast("data/spatial/rasters/North_West_Shelf_DEM_v2_Bathymetry_2020_30m_MSL_cog.tif") %>%
  project("epsg:4326") %>%
  crop(e) %>%
  clamp(upper = 0, values = F)
plot(mb)
names(mb) <- "mb_depth"

# Create plots of each spatial covariate, with a different colour ramp and scale
# Depth
depth <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_depth)) +
  scale_fill_viridis_c(na.value = NA, option = "viridis", name = "Depth") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Aspect
aspect <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_aspect)) +
  scale_fill_viridis_c(na.value = NA, option = "inferno", name = "Aspect") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Roughness
roughness <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_roughness)) +
  scale_fill_viridis_c(na.value = NA, option = "turbo", name = "Roughness") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Detrended bathymetry
detrended <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_detrended)) +
  scale_fill_viridis_c(na.value = NA, option = "rocket", name = "Detrended") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Save out detrended bathymetry plot (used for sampling design)
detrended
ggsave("plots/spatial/DampierAMP_detrended.png",
       height = 6, width = 11, dpi = 300, bg = "white")

# Combine other covariates into a plot (used for modelling and not sampling design)
(depth + aspect)/(roughness + plot_spacer())

ggsave("plots/spatial/DampierAMP_bathymetry-derivatives.png",
       height = 6, width = 11, dpi = 300, bg = "white")

# Create 2 separate plots for comparison between 250m bathy and 30m bathy
depth <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_depth)) +
  scale_fill_viridis_c(na.value = NA, option = "viridis", name = "Depth (250m res)") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

mb_depth <- ggplot() +
  geom_spatraster(data = mb, aes(fill = mb_depth)) +
  scale_fill_viridis_c(na.value = NA, option = "viridis", name = "Depth (30m res)") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Combine and save out plot
depth / mb_depth + plot_annotation(tag_levels = "a") &
  theme(legend.justification = "left")
ggsave("plots/spatial/DampierAMP_multibeam-comparison.png",
       height = 6, width = 6, dpi = 300, bg = "white")
