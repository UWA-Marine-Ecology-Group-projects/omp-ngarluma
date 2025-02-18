###
# Project: OMP Ngarluma
# Data:    LiDAR Bathymetry data
# Task:    Site selection for stereo-BRUVs & drop camera
# author:  Claude Spencer
# date:    September 2023
##

# Load libraries
library(terra)
library(sf)
library(stars)
library(starsExtra)

# Clear the environment
rm(list = ls())

# Set cropping extent
e <- ext(116.75, 117.62, -20.7, -20.25)

# Load in the bathymetry data
bathy <- rast(readRDS("data/spatial/rasters/GA_250m_bathy-trimmed.RDS"),
              crs = "epsg:4326") %>%
  crop(e)
plot(bathy)

# Make detrended bathymetry and roughness
# Roughness
rough <- terra::terrain(bathy, unit = "degrees", neighbors = 8,
                        v = c("roughness", "aspect", "TPI"))
plot(rough)

# Detrended bathymetry
zstar <- st_as_stars(bathy)
detre <- detrend(zstar, parallel = 8) %>%
  rast()
names(detre) <- c("detrended", "lineartrend")
plot(detre)

preds <- rast(list(bathy, rough, detre[[1]]))
names(preds)[1] <- "depth"
plot(preds)

# Save out
saveRDS(preds, "output/sampling-design/bathymetry-derivatives.rds")
