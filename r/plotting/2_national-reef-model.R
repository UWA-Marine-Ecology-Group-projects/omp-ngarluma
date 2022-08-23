###
# Project: Parks Eastern Recherche
# Data:    Jac's habitat predictions - National Reef Model
# Task:    Plot it w/ parks depth contours
# Author:  Claude Spencer
# Date:    August 2022
##

rm(list = ls())

library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)
library(dplyr)
library(GlobalArchive)

# Set CRS for transformations
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")  

# bring in spatial layers
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
st_crs(aus)         <- st_crs(aumpa)
damp_mp <- aumpa[aumpa$ResName%in%"Dampier",]
damp_sanc <- damp_mp[damp_mp$ZoneName%in%"National Park Zone",]                 # Maybe not needed for this stuff

jacmap <- raster("data/spatial/rasters/ecosystem-types-19class-naland.tif")      # Jac's aus habitat predictions
e <- extent(116, 117.5167,-20.56667, -20.3)
jacmap <- crop(jacmap, e)
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
# reduce terrestrial parks
terrnp <- st_crop(terrnp, xmin = 116, ymin = -20.56667, xmax = 117.5167, ymax = -20.3)   # Just the study area
cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters line
bathdf <- readRDS("data/spatial/rasters/GA_250m_bathy-trimmed.RDS")

# Deleted all the other habitat plots, bring back in later

# jac's map, eh
# sort out the classes
jlevs  <- ratify(jacmap)
jclass <- read.csv("data/spatial/rasters/Ecosystem_categories-final-2021.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(classname = category.number) %>%
  dplyr::select(classname, exp.ecosystem.names) %>%
  glimpse()

jmap_df <- as.data.frame(jacmap, xy = TRUE, na.rm = TRUE) %>%
  dplyr::rename(classname = "ecosystem.types.19class.naland") %>%
  dplyr::left_join(jclass) %>%
  dplyr::mutate(exp.ecosystem.names = gsub("\\.", " ", exp.ecosystem.names)) %>%
  glimpse()

unique(jmap_df$exp.ecosystem.names)

# Set colours for Investigator Island
jcls_cols <- scale_fill_manual(values = c(
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Mesophotic coral reefs" = "orange",
  "Shallow coral reefs less than 30 m depth" = "coral2",
  "Shelf vegetated sediments" = "seagreen3"))

waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"),
                                 guide = "none")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb"))

# ab_nmp$ZoneName <- factor(ab_nmp$ZoneName, levels = c("Multiple Use Zone", "Special Purpose Zone",
#                                                       "Habitat Protection Zone","National Park Zone"))

# Build plot Investigator Island
p1 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +
  geom_tile(data = jmap_df, aes(x, y, fill = exp.ecosystem.names)) +
  jcls_cols +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Depth),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.6) +
  geom_sf(data = damp_mp, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  labs(color = "Australian Marine Parks") +
  new_scale_color() +
  annotate(geom = "text", x = c(120.8, 120.8, 120.8), y = c(-33.95, -34.08, -34.24), 
           label = c("30m", "70m", "70m"), size = 2, color = "black") +
  annotate(geom = "text", x = 117.1, y = -20.385, label = "30m",
           color = "black", size = 2) +
  coord_sf(xlim = c(116.8333, 117.5167), ylim = c(-20.56667, -20.3)) +
  labs(fill = "Habitat classification", x = NULL, y = NULL) +
  theme_minimal()

png(filename = "plots/jmonk_natmap.png", width = 10, height = 4,
    units = "in", res = 300)
p1
dev.off()
