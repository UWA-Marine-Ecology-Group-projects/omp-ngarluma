
###
# Project: MBH Example
# Data:    Survey design covariates
# Task:    MBH Design Preparation - Dampier
# author:  Kingsley Griffin
# date:    July 2022
##

library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(stars)
library(starsExtra)
library(geosphere)

source("r/mbhdesign/functions.R")

# set CRS
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects - check the UTM Zone!

# get and sort spatial boundaries
aumpa <- readOGR("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")     # all aus mpas
aumpa <- aumpa[aumpa$ResName %in% c("Dampier"), ]
aumpa <- spTransform(aumpa, sppcrs)
# aumpa <- aumpa[aumpa$ZoneName == "National Park Zone", ]

bathy <- raster("data/spatial/rasters/large/North_West_Shelf_DEM_v2_Bathymetry_2020_30m_MSL_cog.tif")
crs(bathy) <- sppcrs
# bathy <- raster("data/spatial/rasters/tile2c.txt") # Old tile bathy
# plot(bathy)

# crop to general project area
bathy <- crop(bathy, extent(aumpa))
bathy[bathy > 0] <- NA
plot(bathy)
plot(aumpa, add = T)

# transform to utm
# proj4string(bathy) <- wgscrs
# bathy <- projectRaster(bathy, crs = sppcrs)
# plot(bathy)
# proj4string(aumpa) <- wgscrs
# aumpa          <- spTransform(aumpa, sppcrs)
# plot(aumpa, add = T)

saveRDS(aumpa, "output/mbh/aus_mp_dampier_utm.rds")
# saveRDS(bathy, "output/mbh/ga_250_dampier_utm.rds")
saveRDS(bathy, "output/mbh/ga_30_dampier_utm.rds")

# create df of aggregated raster
bathydf <- as.data.frame(bathy, xy = TRUE, na.rm = TRUE)
colnames(bathydf)[3] <- "Depth"

# get centrepoint of npz
npzcent <- centroid(aumpa[aumpa$ZoneName == "National Park Zone", ])

## Generate sites ----
# define project areas
studysite  <- newstrip(npzcent, xdim = 8000, ydim = 8000,
                       heading = 0, siteID = "1", projcrs = sppcrs)              # make a site box
plot(bathy)
plot(aumpa, add = T)
plot(studysite, add = T)

## prepare predictor rasters ----
# crop bathymetry to study site
sitebathy <- crop(bathy, studysite)
plot(sitebathy)

# create terrain
siteterr <- terrain(sitebathy, neighbours = 8, unit = "degrees",
                    opt = c("slope", "roughness"))
plot(siteterr)

# detrended bathymetry
zstar <- st_as_stars(sitebathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")
plot(detre)

# stack em up
all_covs <- stack(sitebathy, detre[[1]], siteterr)
names(all_covs) <- c("depth", "detrended", "roughness", "slope")
plot(all_covs)

saveRDS(all_covs, 'output/covariate_rasters.rds')


