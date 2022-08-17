###
# Project: Parks Murajuga
# Data:    250m bathy
# Task:    Overview maps
# author:  Claude Spencer
# date:    August 2022
##

rm(list = ls())

# Load libraries
library(dplyr)
library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
library(stringr)
library(patchwork)
library(raster)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)

working.dir <- getwd()
setwd(working.dir)

# Set CRS for transformations
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     

# Get data and sort spatial boundaries
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
st_crs(aus)         <- st_crs(aumpa)
damp_mp <- aumpa[aumpa$ResName%in%"Dampier",]
damp_sanc <- damp_mp[damp_mp$ZoneName%in%"National Park Zone",]                 # Maybe not needed for this stuff
# No state sanctuaries here yet ?
# wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2018.shp")                  # All wa mpas
# wa_mp <- wampa[wampa$NAME%in%"Ningaloo",]
# wa_sanc <- wa_mp[wa_mp$ZONE_TYPE%in%"Sanctuary Zone (IUCN IA)",]

cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit

# bathy <- raster("data/spatial/rasters/large/tile2c.txt") # Old tile bathy
# crs(bathy) <- wgscrs

damp_spat <- as_Spatial(damp_mp)
# Crop to general project area
# bathy <- crop(bathy, buffer(damp_spat, width = 0.05))                           # Crop to general study area 
# bathy[bathy > 0] <- NA
# plot(bathy)
# plot(damp_mp, add = T)

slope <- terrain(bathy, opt='slope', unit='degrees')
aspect <- terrain(bathy, opt='aspect', unit='degrees')
hill <- hillShade(slope, aspect, angle = 70, azimuth = 0)

# To dataframes for plotting
hill <- as.data.frame(hill, xy = T, na.rm = T)
# bathy <- as.data.frame(bathy, xy = T, na.rm = T)
bathy <- readRDS("data/spatial/rasters/GA_250m_bathy-trimmed.RDS")
# colnames(bathy)[3] <- "Depth"

# saveRDS(bathy, file = "data/spatial/rasters/GA_250m_bathy-trimmed.RDS")

# build basic plot elements
p1 <- ggplot() +
  geom_tile(data = hill,aes(x = x, y = y, fill = layer), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = bathy, aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis() +
  geom_contour(data = bathy, aes(x = x, y = y, z = Depth), binwidth = 10, 
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = damp_mp, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                "National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.4) +
  # coord_sf(xlim = c(min(bathy$x), max(bathy$x)), ylim = c(min(bathy$y), max(bathy$y))) +
  coord_sf(xlim = c(116.8333, 117.5167), ylim = c(-20.56667, -20.3)) +
  labs(y = "Latitude", x = "Longitude")+
  theme_minimal()
png(filename = "plots/exploratory-site-plot.png", height = 6, width = 8,
    res = 300, units = "in")
p1
dev.off()

p2 <- ggplot() + # Inset closer to the NPZ and HPZ
  geom_tile(data = hill,aes(x = x, y = y, fill = layer), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = bathy, aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis() +
  geom_contour(data = bathy, aes(x = x, y = y, z = Depth), binwidth = 10, 
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = damp_mp, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                "National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.4) +
  # coord_sf(xlim = c(min(bathy$x), max(bathy$x)), ylim = c(min(bathy$y), max(bathy$y))) +
  coord_sf(xlim = c(116.84, 117.15), ylim = c(-20.4, -20.3)) +
  labs(y = "Latitude", x = "Longitude")+
  theme_minimal()
p2

png(filename = "plots/exploratory-site-plot-w-inset.png", height = 8, width = 8,
    res = 300, units = "in")
p1 / p2 + plot_layout(guides = "collect")
dev.off()
