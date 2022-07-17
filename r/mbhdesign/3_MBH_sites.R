
###
# Project: DEW - S. Kangaroo Island
# Data:    MBH Inclusion probabilities
# Task:    MBH Design Site Selection - S. Kangaroo Island
# author:  Kingsley Griffin
# date:    April 2022
##

library(raster)
# install.packages("MBHdesign")
library(MBHdesign)
library(sp)
library(ggplot2)
library(reshape2)

# get inclusion probs (from 'R/2_make_inclusionprobs.R')
inp_overall <- readRDS("output/inclusionp_rast.rds")
cellStats(inp_overall, "sum") # yay

# not sure why but multiply inclusion p values so the sum equals the number of samples?

inp_overall[] <- inp_overall[] * 40
plot(inp_overall)
cellStats(inp_overall, "sum")
inp_overall[is.na(inp_overall)] <- 0.000000000001

plot(inp_overall)

# fix design parameters
nbruv <- 40
# bruv >350m apart - adding a few extra sites, manually remove any that are too near

## select sites
set.seed(42) #

# pot_sites <- as.data.frame(inp_overall, na.rm = TRUE, xy = TRUE)
# pot_sites <- pot_sites[pot_sites$layer > 0, ]

#having issues choosing locations within these separate sites
tha_sites <- quasiSamp(n = nbruv, 
                       potential.sites = coordinates(inp_overall), 
                       inclusion.probs = values(inp_overall), 
                       nSampsToConsider = 20000)

tha_sites_sp <- SpatialPointsDataFrame(coords = cbind(tha_sites[1:2]), data = tha_sites)

## check spread of sites
# plot against inclusion probabilities
plot(inp_overall)
plot(tha_sites_sp, add = TRUE)


# get covariates
preds <- readRDS("output/covariate_rasters.rds")

site_covs <- cbind(tha_sites, extract(preds, tha_sites_sp))
site_c_w  <- melt(site_covs, id.vars = 1:4)
ggplot(site_c_w, aes(ID, value)) + 
  geom_point(alpha = 3/5) +
  geom_smooth() +
  facet_wrap(~ variable, scales = "free")

covdat <- readRDS("output/covariate_df.rds")
covd_w <- melt(covdat[3:6])
covd_w$source <- c("rasters")

sitedat <- data.frame("variable" = site_c_w$variable, "value" = site_c_w$value, "source" = c("sites"))
alldat  <- rbind(covd_w, sitedat)

ggplot(alldat, aes(variable, value, colour = source)) + 
  geom_violin() + 
  facet_wrap(~ variable, scales = "free")

hist(site_covs$site)
hist(site_covs$depth)
hist(site_covs$slope)


# # map of design
# aumpa      <- readRDS("output/aus_mp_dampier_utm.rds")
# allsite_sf <- readRDS("output/")
# 
# 
# studarea <- ggplot() +
#   geom_polygon(data = aumpa, aes(long, lat, group = group),
#                fill = "grey90", colour = "grey20", alpha = 1/5) +
#   geom_tile(data = covdat, aes(x, y, fill = depth)) +
#   scale_fill_viridis(option = "E") +
#   geom_sf(data = allsite_sf, aes(colour = as.factor(siteID)), fill = NA)+
#   geom_point(data = tha_sites, aes(x,y), shape = 3, colour = "grey90", alpha= 4/5) +
#   theme_dark() +
#   labs(x = NULL, y = NULL, colour = "Site")
# studarea

## outputting
# convert sites to wgs84 and export
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")
proj4string(tha_sites_sp) <- sppcrs
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sites_wgs <- spTransform(tha_sites_sp, wgscrs)
sites_df  <- as.data.frame(sites_wgs, xy = TRUE)

sites_df <- merge(sites_df, site_covs[ , 4:8], by = "ID")
colnames(sites_df)[1:6] <- c("unique_id", "easting", "northing", 
                             "inclusion_prob", "lat", "long")
sites_df <- sites_df[ , -7]

summary(sites_df)


# output to csv and shapefile for field
sites_df$pointnum  <- c(1:nrow(sites_df))
sites_df$dropcode  <- interaction("DAMP", sites_df$pointnum, sep = "")

sites_df <- sites_df[ , colnames(sites_df) %in% 
                        c("long", "lat", "dropcode", "site", 
                          "method", "pointnum")]
sites_df$selected <- c("MBH")
head(sites_df)
# sites_df$depth <- site_covs$depth

write.csv(sites_df, 'output/planned/dampier_bruv_mbh.csv')

sites_sp <- SpatialPointsDataFrame(coords = sites_df[1:2], data = sites_df)
shapefile(sites_sp, "output/planned/dampier_boss_mbh", overwrite = TRUE)


