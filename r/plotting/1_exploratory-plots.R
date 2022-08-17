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

fbath  <- readRDS("output/nesp_5m_bathy_interp_ptcloates.rds")                  # pt cloates
fbath <- projectRaster(fbath, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
ybath  <- readRDS("output/nesp_5m_bathy_interp_yardie.rds")                     # yardie creek
ybath <- projectRaster(ybath, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
plot(fbath)
plot(ybath)
fbathdf <- as.data.frame(fbath, xy = T)
colnames(fbathdf)[3] <- "Depth"
ybathdf <- as.data.frame(ybath, xy = T)
colnames(ybathdf)[3] <- "Depth"

test <- raster("data/spatial/raster/depth_195_50m.tif")
test <- flip(test, direction = "y")
slope <- terrain(test,opt='slope',unit='degrees')
aspect <- terrain(test,opt='aspect',unit='degrees')
hill <- hillShade(slope, aspect, 90, 90)

hill <- projectRaster(hill, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
hill <- as.data.frame(hill, xy = T, na.rm = T)
plot(test)
test <- projectRaster(test, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
test <- as.data.frame(test, xy = T, na.rm = T)
colnames(test)[3] <- "Depth"

# build basic plot elements
p1 <- ggplot() +
  geom_tile(data = test,aes(x = x, y = y, fill = Depth), alpha = 1)+
  scale_fill_viridis()+
  new_scale_fill()+
  geom_tile(data = hill,aes(x = x, y = y, fill = layer), alpha = 0.5)+
  scale_fill_gradient(low = "white", high = "black", guide = "none")+
  geom_contour(data = test, aes(x, y, z = Depth),
               binwidth = 10, colour = "white",
               alpha = 1, size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  # geom_sf(data = nin_mp, aes(color = ZoneName), fill = NA)+
  geom_sf(data = wa_sanc, color = "#bfd054", fill = "#bfd054", alpha = 0.1)+
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_point(data = yardie, aes(x = longitude, y = latitude, color = campaignid, shape = method))+
  # scale_color_manual(values = c("2021-05_NingalooTransect_BOSS" = "salmon4",
  #                               "2021-08_Yardie-Creek_Baited-BOSS" = "coral1",
  #                               "2021-08_Yardie-Creek_Flasher-BOSS" = "coral1",
  #                               "2021-08_Yardie-Creek_stereo-BRUVs" = "coral1"))+
  scale_shape_manual(values = c("BOSS" = 1, "BRUV" = 8))+
  coord_sf(xlim = c(min(yardie$longitude),113.76),
           ylim = c(min(yardie$latitude),-22.22))+
  labs(y = "Latitude", x = "Longitude")+
  theme_minimal()
p1

p2 <- ggplot() +
  geom_tile(data = test,aes(x = x, y = y, fill = Depth), alpha = 1)+
  scale_fill_viridis()+
  new_scale_fill()+
  geom_tile(data = hill,aes(x = x, y = y, fill = layer), alpha = 0.5)+
  scale_fill_gradient(low = "white", high = "black", guide = "none")+
  geom_contour(data = test, aes(x, y, z = Depth),
               binwidth = 10, colour = "white",
               alpha = 1, size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = nin_sanc, color = "#7bbc63", fill = "#7bbc63", alpha = 0.1)+
  # geom_sf(data = wa_sanc, color = "#bfd054", fill = NA)+
  geom_point(data = cloates, aes(x = longitude, y = latitude, color = campaignid, shape = method))+
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  scale_shape_manual(values = c("BOSS" = 1, "BRUV" = 8))+
  coord_sf(xlim = c(min(cloates$longitude),max(cloates$longitude)),
           ylim = c(min(cloates$latitude),max(cloates$latitude)))+
  labs(y = "Latitude", x = "Longitude")+
  theme_minimal()
p2

p1 <- ggplot() +
  # geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  # scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -40, -70, -120, -7000)) +
  # geom_contour(data = bathdf, aes(x = x, y = y, z = Depth),
  # binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = mb_mp, aes(fill = waname), alpha = 1, colour = NA) +
  wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp%>%dplyr::filter(leg_catego%in%c("National Park","Nature Reserve")), 
          aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "Terrestrial Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = aumpa, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  nmpa_cols +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -40, -70, -120), colour = "white",
               alpha = 1, size = 0.1) +
  labs(x = 'Longitude', y = 'Latitude', fill = "Australian Marine Parks") +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = min(metadata$longitude), xmax = max(metadata$longitude),
           ymin = min(metadata$latitude), ymax = max(metadata$latitude),
           colour = "grey15", fill = "white", alpha = 0.2, size = 0.1) +
  coord_sf(xlim = c(114.75,116.25), ylim = c(-21.2,-20))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
p1