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
library(terra)
library(tidyterra)
library(tidyverse)
library(sf)
library(patchwork)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set the study name
name <- "DampierAMP"

# Set the extent of the study
e <- ext(116.7, 117.7,-20.919, -20)

# Read in shapefile data for maps
aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

# Create spatial plots
## SST
sst <- rast(paste0("data/spatial/oceanography/", name, "_SST_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sst)
sst <- sst[[c("Jan", "Mar", "May", "Jul", "Sep", "Nov")]]
names(sst)

prediction_limits = c(116.779, 117.544, -20.738, -20.282)

plot_sst(prediction_limits) +
  theme(axis.text = element_text(size = 6))

ggsave(paste0("plots/spatial/", name, "_SST.png"),
       height = 3.8, width = 8, dpi = 600, bg = "white", units = "in")

## SLA
sla <- rast(paste0("data/spatial/oceanography/", name, "_SLA_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sla)

plot_sla(prediction_limits) +
  theme(axis.text = element_text(size = 6))

ggsave(paste0("plots/spatial/", name, "_SLA.png"),
       height = 3.8, width = 8, dpi = 600, bg = "white", units = "in")

## DHW
dhw <- rast(paste0("data/spatial/oceanography/", name, "_DHW_raster.rds"))
names(dhw)

plot_dhw(prediction_limits) +
  theme(axis.text = element_text(size = 6))

ggsave(paste0("plots/spatial/", name, "_DHW.png"),
       height = 2.7, width = 8, dpi = 600, bg = "white", units = "in")

pressure_data()

maxyear = c(2013, 2022)
pressure_plot(maxyear)

ggsave(filename = paste0('plots/spatial/', name, '_oceanography_time-series.png'),
       dpi = 300, units = "in", bg = "white",
       width = 6, height = 6.75)
