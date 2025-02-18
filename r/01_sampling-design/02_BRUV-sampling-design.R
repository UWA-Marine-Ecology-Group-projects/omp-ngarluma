###
# Project: OMP Ngarluma
# Data:    LiDAR Bathymetry data
# Task:    Site selection for stereo-BRUVs
# Author:  Claude Spencer
# Date:    September 2023
##

# NOTES
# Data must be in projected (flat) CRS
# Need to make a shapefile of inclusion probs

rm(list = ls())

# Load libraries
library(spsurvey)
library(tidyverse)
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyterra)
library(viridis)

# Set the seed for reproducible plans
set.seed(15)

# Set the number of samples
n <- 96

# Load marine parks ----
# As a shapefile
aumpa_sf <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ResName %in% "Dampier") %>%
  dplyr::select(ZoneName, geometry) %>%
  dplyr::mutate(code = case_when(ZoneName %in% "Habitat Protection Zone" ~ 1,
                                 ZoneName %in% "National Park Zone" ~ 2,
                                 ZoneName %in% "Multiple Use Zone" ~ 3)) %>%
  st_transform(3112)

# As a spatvector
aumpa_vect <- aumpa_sf %>%
  vect() 

# Load exclusion area/port channel area
port <- st_read("data/spatial/shapefiles/rough_port-walcott_exclusions-zones.shp") %>%
  st_transform(3112)

# Load the bathymetry data, reproject and mask ----
preds <- readRDS("output/sampling-design/bathymetry-derivatives.rds") %>%
  crop(ext(116.8, 117.25, -20.7, -20.2)) %>%
  project("epsg:3112") %>%
  mask(aumpa_sf) %>%
  mask(port, inverse = T) %>%
  crop(ext(-1780000, -1715000, -2465000, -2430000)) %>%
  trim()
plot(preds)

# Rasterize the zones ----
blank_raster <- rast(preds, nlyr = 0)
zones <- rasterize(aumpa_vect, blank_raster, field = "code") %>%
  mask(preds[[1]])
plot(zones)

# Make strata ----
# Using detrended bathymetry and zone
hist(preds$detrended)
detrended_qs <- c(0, 1/3, 2/3, 1)
detrended_cuts   <- global(preds$detrended, probs = detrended_qs, fun = quantile, na.rm = T)
cat_detrended <- classify(preds$detrended, rcl = as.numeric(detrended_cuts[1,]))
plot(cat_detrended)

# As a categorical raster
inp_rasts <- as.data.frame(cat_detrended, xy = TRUE, na.rm = T) %>%
  dplyr::mutate(strata = case_when(detrended %in% "(-0.305395–0.606343]" ~ 2,
                                   detrended %in% "(0.606343–6.969479]" ~ 3,
                                   detrended %in% "(-9.2949–-0.305395]" ~ 1)) %>%
  dplyr::select(x, y, strata) %>%
  rast(type = "xyz", crs = crs(cat_detrended)) %>%
  resample(cat_detrended)
plot(inp_rasts)

# To stars object
inp_stars <- st_as_stars(inp_rasts)
plot(inp_stars)

# To simple features - and intersect with zones to create final strata
inp_sf <- st_as_sf(inp_stars) %>%
  group_by(strata) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_make_valid() %>%
  st_intersection(aumpa_sf) %>%                                                 # Intersect with zone
  dplyr::mutate(prop = case_when(strata %in% 1 ~ 0.4,                           # Proportion of samples within each detrended strata
                                 strata %in% 2 ~ 0.2, 
                                 strata %in% 3 ~ 0.4),
                zonesamps = case_when(ZoneName %in% "Habitat Protection Zone" ~ 24, # Number of samples in each zone
                                      ZoneName %in% "National Park Zone" ~ 32,
                                      ZoneName %in% "Multiple Use Zone" ~ 40),
                strata = paste0("strata.", row.names(.))) %>%
  dplyr::mutate(nsamps = round(prop * zonesamps, digits = 0)) %>%               # Number of samples * proportion
  glimpse()
plot(inp_sf)

# GRTS needs the number of samples in this horrible wide format for some reason
base_samps <- data.frame(nsamps = inp_sf$nsamps,
                         strata = inp_sf$strata) %>%
  pivot_wider(names_from = strata,
              values_from = nsamps) %>%
  glimpse()

# Run the sampling design ----
sample.design <- grts(inp_sf, 
                      n_base = base_samps, 
                      stratum_var = "strata", 
                      DesignID = "DM-BV",  # Prefix for sample name                          
                      mindis = 250)

# Have a look
ggplot() +
  geom_spatraster(data = inp_rasts, aes(fill = strata )) +
  scale_fill_viridis_c(na.value = NA, option = "D") +
  geom_sf(data = sample.design$sites_base, colour = "red") +
  coord_sf(crs = 4326) +
  theme_minimal()

# Select useful columns and export the design ----
samples <- sample.design$sites_base %>%
  dplyr::select(siteID, lon_WGS84, lat_WGS84, ip) %>%
  dplyr::mutate(ip = as.character(ip)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  glimpse()

write.csv(samples, file = "output/sampling-design/bruv_sampling-design.csv",
          row.names = F)
