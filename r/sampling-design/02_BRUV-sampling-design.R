###
# Project: OMP Ngarluma
# Data:    LiDAR Bathymetry data
# Task:    Site selection for stereo-BRUVs
# author:  Claude Spencer
# date:    September 2023
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

# Load marine parks ----
aumpa_sf <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ResName %in% "Dampier") %>%
  st_transform(3112)

aumpa_vect <- aumpa_sf %>%
  vect() 

# Load the bathymetry data, reproject and mask ----
preds <- readRDS("output/sampling-design/bathymetry-derivatives.rds") %>%
  project("epsg:3112") %>%
  mask(aumpa_sf)
plot(preds)

blank_raster <- rast(preds, nlyr = 0)
zones <- rasterize(aumpa_vect, blank_raster, field = "n_samps") %>%
  mask(preds[[1]])
plot(zones)

# Make inclusion probabilities ----
# Using detrended bathymetry
n = 96 # Set the number of samples

# Calculate inclusion probabilities based off detrended bathymetry
hist(preds$detrended)
detrended_qs <- c(0, 0.55, 0.9, 1)
detrended_cuts   <- global(preds$detrended, probs = detrended_qs, fun = quantile, na.rm = T)
cat_detrended <- classify(preds$detrended, rcl = as.numeric(detrended_cuts[1,]))
plot(cat_detrended)

detrended_split <- data.frame(zones = unique(cat_detrended),
                              split = c(1, 1, 1))

icr_df    <- as.data.frame(cat_detrended, xy = TRUE, na.rm = T) %>%
  group_by(detrended) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  left_join(detrended_split) %>%
  dplyr::mutate(prop = n / sum(n),
                inclp = split / prop,
                incl_prob = inclp / sum(inclp)) %>%
  glimpse()

inp_rasts <- as.data.frame(cat_detrended, xy = TRUE, na.rm = T) %>%
  left_join(icr_df) %>%
  dplyr::select(x, y, incl_prob, split) %>%
  rast(type = "xyz", crs = crs(cat_detrended)) %>%
  resample(cat_detrended)
plot(inp_rasts)

inp_stars <- st_as_stars(inp_rasts)
plot(inp_stars)

inp_sf <- st_as_sf(inp_stars) %>%
  group_by(incl_prob, split) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  dplyr::mutate(strata = paste("strata", row.names(.), sep = " "),
                nsamps = round(n * incl_prob, digits = 0)) %>%
  st_make_valid() %>%
  glimpse()

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

ggplot() +
  geom_spatraster(data = inp_rasts, aes(fill = incl_prob )) +
  scale_fill_viridis_c(na.value = NA, option = "D") +
  geom_sf(data = sample.design$sites_base, colour = "red") +
  coord_sf(crs = 4326) +
  theme_classic()

samples <- sample.design$sites_base %>%
  dplyr::select(siteID, lon_WGS84, lat_WGS84, incl_prob) %>%
  glimpse()

write.csv(samples, file = "output/sampling-design/bruv_sampling-design.csv",
          row.names = F)
