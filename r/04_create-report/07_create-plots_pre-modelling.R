###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Marine Park monitoring data syntheses, oceanographic data, marine park boundary files
# Task:    Create pre-modelling figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

# Table of contents
# 1. Overall location plot (including State and Commonwealth Marine Parks)
# 2. Sampling location plot
# 3. Key Ecological Features - none in the Dampier Marine Park
# 4. Historical Sea Levels
# 5. Bathymetry cross section

# Clear your environment
rm(list = ls())

# Set the study name
name <- "DampierAMP"

# Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(metR)
library(patchwork)
library(terra)
library(tidyterra)
library(ggnewscale)
library(CheckEM)
library(geosphere)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(116.7, 117.7,-20.919, -20)

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

# Key Ecological Features
# This shapefile has added columns in QGIS for hex colour code and abbreviated names
kef <- st_read("data/spatial/shapefiles/AU_DOEE_KEF_2015.shp") %>%
  CheckEM::clean_names() %>%
  st_make_valid() %>%
  st_crop(e) %>%
  arrange(desc(area_km2)) %>%
  glimpse()
unique(kef$abbrv) # None in Dampier

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

wrecks <- data.frame(x = c(117.042948333243, 117.213089999945),
                     y = c(-20.3212583329681, -20.4297716665203),
                     label = "Shipwreck",
                     wreck = c("Glenbank", "Kunmunuya & Samson II")) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# 1. Location overview plot
# Set plot inputs
plot_limits = c(116.779, 117.544, -20.738, -20.282) # Extent of the main plot
study_limits = c(116.86, 117.4,-20.51, -20.3) # Extent of sampling
annotation_labels = data.frame(x = c(117.1935, 116.8763, 116.8333), # Labels for annotation e.g. nearby towns
                               y = c(-20.6287, -20.3854, -20.5583),
                               label = c("Point\nSamson", "Legendre\nIsland", "Burrup\nPeninsula"))

# Create plot
location_plot(plot_limits,
              study_limits,
              annotation_labels) +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"))
# Save plot
ggsave(paste(paste0('plots/spatial/', name) , 'broad-site-plot.png',
             sep = "-"), dpi = 600, width = 8, height = 3.5, bg = "white")

# 2. Site level overview - with sampling point locations
metadata <- readRDS(paste0("data/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
  dplyr::mutate(method = case_when(str_detect(campaignid, "BRUV") ~ "BRUV",
                                   str_detect(campaignid, "BOSS") ~ "BOSS")) %>%
  glimpse()

# Set plot inputs
site_limits = c(116.779, 117.544, -20.738, -20.282) # For Dampier match it to the first plot

# Create plot
site_plot(site_limits, annotation_labels) +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm"))
# Save plot
ggsave(filename = paste(paste0('plots/spatial/', name) , 'sampling-locations.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 3.8)

# 3. Key Ecological Features - there are none in the Dampier Marine Park
# Create plot
# kef_plot(plot_limits, annotation_labels)
# Save plot
# ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'key-ecological-features.png',
#                         sep = "-"), units = "in", dpi = 600,
#        bg = "white",
#        width = 8, height = 6)

# 4. Historical sea levels
# Set coastline fills
depth_fills <- scale_fill_manual(values = c("#f9ddb1","#ee9f27", "#dc6601"),
                                 labels = c("9-10 Ka", "15-17 Ka", "20-30 Ka"),
                                 name = "Coastline age")

# Create plot
sealevel_plot(plot_limits, annotation_labels) +
  theme(panel.background = element_rect(fill = "#f9ddb1", colour = NA))
# Save plot
ggsave(filename = paste(paste0('plots/spatial/', name) , 'old-sea-levels.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 5)

# 5. Bathymetry cross sections
# Create data
bath_df1 <- dem_cross_section(116.7475, 116.9888, -20.6993, -20.2273, maxdist = 10)
# Set plot inputs
crosssection_labels = data.frame(x = c(-5, -10, -16), # Labels for annotation
                                 y = c(110, 80, 20),
                                 label = c("Burrup Peninsula", "Dolphin Island", "Legendre Island"))

segment_offset <- 5 # Length of the segment
label_offset <- segment_offset + 2 # Distance from end of segment to label
# Create plot
crosssection_plot(crosssection_labels, label_offset, segment_offset)
# Save plot
ggsave(filename = paste(paste0('plots/spatial/', name) , 'bathymetry-cross-section.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 4)
