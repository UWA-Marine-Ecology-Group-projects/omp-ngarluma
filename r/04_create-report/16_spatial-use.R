###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Raw fish and benthos annotations
# Task:    Format annotations to match the format returned from GlobalArchive synthesis API
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list = ls())

# Set the study name
name = "dampierAMP"

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggnewscale)
library(CheckEM)
library(tidyterra)
library(ggpattern)
library(patchwork)

# Set cropping extent - larger than most zoomed out plot
e <- ext(116.7, 117.7,-20.919, -19.8)

# Load necessary spatial files
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

# Load marine parks
# aus_marine_parks <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

# All australian marine parks - for inset plotting
aus_marine_parks <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp")

marine_parks <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Dampier")) %>%
  glimpse()

# Australian Marine Parks only (for separate ggplot legends)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  arrange(zone)

# State Marine Parks only (for separate ggplot legends)
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

# Make shapefile for the wreck exclusion area
kunmunya <- data.frame(x = 117.213333, y = -20.4301667, zone = "Sanctuary Zone", colour = "#bfd054") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(9473) %>%
  st_buffer(dist = 500)
plot(kunmunya)

# Terrestrial parks
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
plot(terrnp["leg_catego"])

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Bathymetry data
bathy <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = F)
names(bathy) <- "Depth"
plot(bathy)

bathdf <- as.data.frame(bathy, xy = T)

# Load Port Walcott shipping zones
channel <- st_read("data/spatial/shapefiles/port-walcott_shipping-channel.shp") %>%
  summarise(geometry = st_union(geometry)) %>%
  dplyr::mutate(Infrastructure = "Shipping channel") %>%
  glimpse()

spoil <- st_read("data/spatial/shapefiles/port-walcott_spoil-grounds.shp") %>%
  summarise(geometry = st_union(geometry)) %>%
  dplyr::mutate(Infrastructure = "Spoil ground") %>%
  glimpse()

infrastructure <- bind_rows(channel, spoil)

# Load data
spatial_use <- st_read("data/spatial/shapefiles/d_use_sdat_ngarluma_act_point.shp") %>%
  clean_names() %>%
  dplyr::mutate(use = case_when(activity %in% c("LineFishingDemersals", "Spearfishing", "LineFishingPelagics") ~ "Extractive",
                                activity %in% c("Beachgoing", "Swimming", "TowSport", "other") ~ "Non-extractive")) %>%
  dplyr::mutate(activity = case_when(activity %in% "other" ~ "Whale-watching",
                                     activity %in% "LineFishingDemersals" ~ "Demersal fishing",
                                     activity %in% "LineFishingPelagics" ~ "Pelagic fishing",
                                     activity %in% "TowSport" ~ "Tow-sports",
                                     .default = activity)) %>%
  glimpse()
unique(spatial_use$activity)
plot(spatial_use["use"])

p1 <- ggplot() +
  geom_spatraster_contour_filled(data = bathy,
                                 breaks = c(0, -30, -70, -200, - 700, -2000 , -4000, -6000),
                                 colour = NA, show.legend = F) +
  # scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080")) +
  new_scale_fill() +
  geom_spatraster_contour(data = bathy,
                          breaks = c(-30, -70, -200, - 700, -2000 , -4000, -6000), colour = "white",
                          alpha = 3/5, linewidth = 0.1, show.legend = F) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
  scale_fill_manual(name = "State Marine Parks", guide = "legend",
                    values = with(marine_parks_state, setNames(colour, zone))) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
  scale_fill_manual(name = "Australian Marine Parks", guide = "legend",
                    values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  geom_sf(data = kunmunya, aes(fill = zone), colour = NA, alpha = 0.4) +
  scale_fill_manual(name = "Closed Waters", guide = "legend",
                    values = with(kunmunya, setNames(colour, zone))) +
  new_scale_fill() +
  geom_sf(data = dplyr::filter(spatial_use, !is.na(use)), aes(fill = activity), 
          shape = 21, size = 3) +
  scale_fill_viridis_d(name = "Activity", option = "H", direction = 1) +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
  geom_sf_pattern(data = infrastructure, aes(pattern = Infrastructure, pattern_fill = Infrastructure, colour = Infrastructure), alpha = 0.7,
                  pattern_density = 0.8, pattern_size = 0.2, pattern_spacing = 0.005, pattern_colour = "grey80") +
  scale_colour_manual(values = c("#F35B04", "#D90429")) +
  scale_pattern_fill_manual(values = c("#F35B04", "#D90429")) +
  scale_pattern_manual(values = c("stripe", "crosshatch")) +
  labs(x = NULL, y = NULL) +
  # annotate("text", x = annotation_labels$x,
  #          y = annotation_labels$y,
  #          label = annotation_labels$label, size = 1.65,
  #          fontface = "italic") +
  coord_sf(xlim = c(116.859394871601, 117.538194740011), ylim = c(-20.6602460716533, -19.9115669759464), crs = 4326) +
  theme_minimal()

spatial_df <- spatial_use %>%
  mutate(lat = st_coordinates(.)[,2], 
         lon = st_coordinates(.)[,1]) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  glimpse()

p2 <- ggplot() +
  geom_spatraster_contour_filled(data = bathy,
                                 breaks = c(0, -30, -70, -200, - 700, -2000 , -4000, -6000),
                                 colour = NA, show.legend = F) +
  # scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080")) +
  new_scale_fill() +
  geom_spatraster_contour(data = bathy,
                          breaks = c(-30, -70, -200, - 700, -2000 , -4000, -6000), colour = "white",
                          alpha = 3/5, linewidth = 0.1, show.legend = F) +
  geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
  scale_fill_manual(name = "State Marine Parks", guide = "none",
                    values = with(marine_parks_state, setNames(colour, zone))) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
  scale_fill_manual(name = "Australian Marine Parks", guide = "none",
                    values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  geom_sf(data = kunmunya, aes(fill = zone), colour = NA, alpha = 0.4) +
  scale_fill_manual(name = "Closed Waters", guide = "none",
                    values = with(kunmunya, setNames(colour, zone))) +
  new_scale_fill() +
  stat_density_2d(data = spatial_df, aes(x = lon, y = lat, fill = after_stat(level), alpha = ..level..), 
                  geom = "polygon", bins = 5) +
  scale_fill_viridis_c(option = "H", name = "Kernel\nDensity", direction = -1) +
  scale_alpha(guide = 'none') +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
  scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                               "Nature Reserve" = "#e4d0bb"),
                    name = "Terrestrial Parks", guide = "none") +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round", show.legend = F) +
  geom_sf_pattern(data = infrastructure, aes(pattern = Infrastructure, pattern_fill = Infrastructure, colour = Infrastructure), alpha = 0.7,
                  pattern_density = 0.8, pattern_size = 0.2, pattern_spacing = 0.005, pattern_colour = "grey80") +
  scale_colour_manual(values = c("#F35B04", "#D90429"), guide = "none") +
  scale_pattern_fill_manual(values = c("#F35B04", "#D90429"), guide = "none") +
  scale_pattern_manual(values = c("stripe", "crosshatch"), guide = "none") +
  labs(x = NULL, y = NULL) +
  # annotate("text", x = annotation_labels$x,
  #          y = annotation_labels$y,
  #          label = annotation_labels$label, size = 1.65,
  #          fontface = "italic") +
  coord_sf(xlim = c(116.859394871601, 117.538194740011), ylim = c(-20.6602460716533, -19.9115669759464), crs = 4326) +
  theme_minimal()

p2 / p1 + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")
ggsave(filename = paste0("plots/spatial/", name, "_spatial-use.png"),
       height = 10, width = 6.5, dpi = 300, bg = "white")
