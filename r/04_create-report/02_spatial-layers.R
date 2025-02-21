###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Spatial covariates
# Task:    Format spatial covariates, extract covariates for each sampling location
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list = ls())

# Set the study name
name <- "DampierAMP"

# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(tidyterra)
library(patchwork)
library(RNetCDF)
library(rerddap)

# Set the extent of the study
e <- ext(116.7, 117.7,-20.919, -20)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(bathy)

# Create terrain metrics (bathymetry derivatives)
aspect <- terrain(bathy, neighbors = 8,
                 v = "aspect",
                 unit = "degrees")
roughness <- terrain(bathy, neighbors = 8,
                  v = "roughness",
                  unit = "degrees")
preds <- rast(list(aspect, roughness))
names(preds) <- c("geoscience_aspect", "geoscience_roughness")

# Create detrended bathymetry
zstar <- st_as_stars(bathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "SpatRaster")
names(detre) <- c("geoscience_detrended", "lineartrend")

# Join depth, terrain metrics and detrended bathymetry
preds <- rast(list(bathy, preds, detre[[1]]))
names(preds)[1] <- "geoscience_depth"

# Save the bathymetry derivatives
saveRDS(preds, file = paste0("data/spatial/rasters/",
                      name, "_bathymetry-derivatives.rds"))

# Read in the metadata

metadatabruv <- readRDS(paste0("data/staging/", name, "_BRUVs_metadata.RDS")) %>%
  dplyr::select(campaignid, opcode, longitude_dd, latitude_dd, status) %>%
  dplyr::rename(sample = opcode) %>%
  glimpse()

metadataboss <- readRDS(paste0("data/staging/", name, "_BOSS_metadata.RDS")) %>%
  dplyr::select(campaignid, period, longitude_dd, latitude_dd, status) %>%
  dplyr::rename(sample = period) %>%
  glimpse()
metadata <- bind_rows(metadatabruv, metadataboss)

# Convert metadata to a spatial file and check alignment with bathymetry
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Check that samples align with bathymetry derivatives
plot(preds[[1]])
plot(metadata_sf, add = T)

# Extract bathymetry derivatives at each of the samples
metadata.bathy.derivatives   <- cbind(metadata,
                                      terra::extract(preds, metadata_sf, ID = F)) %>%
  filter_at(vars(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
            all_vars(!is.na(.))) %>% # Removes samples missing bathymetry derivatives - check these!!
  glimpse()

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/tidy/", name, "_metadata-bathymetry-derivatives.rds"))

# Oceanography/Pressures
# Sea surface temperature
nc_sst <- open.nc(paste0("data/spatial/oceanography/SST.nc"), write = TRUE)
print.nc(nc_sst) # shows you all the file details
time_nc <- var.get.nc(nc_sst, 'time')  # NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_sst <- utcal.nc("seconds since 1981-01-01 00:00:00", time_nc, type = "c")
dates_sst <- as.Date(time_nc_sst)
close.nc(nc_sst) # GDAL errors otherwise

rast_sst <- rast(paste0("data/spatial/oceanography/SST.nc"),
                 subds = "sea_surface_temperature") %>%
  crop(e) %>%
  trim()
plot(rast_sst)
names(rast_sst) <- dates_sst
time(rast_sst) <- dates_sst

winter_sst_ts <- rast_sst[[names(rast_sst)[str_detect(names(rast_sst), "-06-|-07-|-08-")]]]

for (month in unique(month(time(rast_sst)))) {
  print(month)
  monthly_rast <- subset(rast_sst, month(time(rast_sst)) == month) %>%
    mean(na.rm = T) %>%
    app(fun = function(i) {i - 273.15})
  names(monthly_rast) <- month.abb[month]
  if (month == 3) {
    sst <- monthly_rast
  }
  else {
    sst <- rast(list(sst, monthly_rast))
  }
}

saveRDS(sst, paste0("data/spatial/oceanography/", name, "_SST_raster.rds"))

sst_tsdf <- terra::global(rast_sst, fun = "mean", na.rm = T) %>%
  tibble::rownames_to_column() %>%
  cbind(terra::global(rast_sst, fun = "sd", na.rm = T)) %>%
  # dplyr::mutate(temp = mean - 273.15, # Convert kelvin to celsius
  #               date = date(rowname)) %>%
  tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::group_by(year, month) %>%
  summarise(sst = mean(mean, na.rm = T) - 273.15, # Convert kelvin to celsius
            sd = mean(sd, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(season = case_when(month %in% c("03", "04", "05") ~ "Autumn",
                                   month %in% c("06", "07", "08") ~ "Winter",
                                   month %in% c("09", "10", "11") ~ "Spring",
                                   month %in% c("12", "01", "02") ~ "Summer")) %>%
  glimpse()

saveRDS(sst_tsdf, paste0("data/spatial/oceanography/", name, "_SST_time-series.rds"))

# This stuff below has not been included in the report
# 
# # Sea Level Anomaly
# nc_sla <- open.nc(paste0("data/", park, "/spatial/oceanography/SLA.nc"),
#                   write = TRUE)
# print.nc(nc_sla) # shows you all the file details
# time_nc <- var.get.nc(nc_sla, 'TIME')
# time_nc_sla <- utcal.nc("days since 1985-01-01 00:00:00 UTC", time_nc, type = "c")
# dates_sla <- as.Date(time_nc_sla)
# close.nc(nc_sla)
# 
# rast_sla <- terra::rast(paste0("data/", park, "/spatial/oceanography/SLA.nc"),
#                         subds = "GSLA")
# time(rast_sla) <- dates_sla
# names(rast_sla) <- dates_sla
# plot(rast_sla)
# 
# for (month in unique(month(time(rast_sla)))) {
#   print(month)
#   monthly_rast <- subset(rast_sla, month(time(rast_sla)) == month) %>%
#     mean(na.rm = T)
#   names(monthly_rast) <- month.abb[month]
#   if (month == 1) {
#     sla <- monthly_rast
#   }
#   else {
#     sla <- rast(list(sla, monthly_rast))
#   }
# }
# 
# saveRDS(sla, paste0("data/", park, "/spatial/oceanography/", name, "_SLA_raster.rds"))
# 
# sla_tsdf <- terra::global(rast_sla, fun = "mean", na.rm = T) %>%
#   tibble::rownames_to_column() %>%
#   cbind(terra::global(rast_sla, fun = "sd", na.rm = T)) %>%
#   tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
#   dplyr::group_by(year, month) %>%
#   summarise(sla = mean(mean, na.rm = T),
#             sd = mean(sd, na.rm = T)) %>%
#   ungroup() %>%
#   dplyr::mutate(season = case_when(month %in% c("03", "04", "05") ~ "Autumn",
#                                    month %in% c("06", "07", "08") ~ "Winter",
#                                    month %in% c("09", "10", "11") ~ "Spring",
#                                    month %in% c("12", "01", "02") ~ "Summer")) %>%
#   glimpse()
# 
# saveRDS(sla_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_SLA_time-series.rds"))
# 
# # Degree Heating Weeks
# # https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html
# # Specify the new desired filename
# new_filename <- paste0("data/", park, "/spatial/oceanography/DHW.nc")
# 
# # Only run the griddap function if the file doesn't exist
# if (!file.exists(new_filename)) {
#   response <- griddap("NOAA_DHW",
#                       stride = 7,
#                       time = c('1992-03-18T12:00:00Z', '2024-09-30T12:00:00Z'),
#                       latitude = c(-20.77, -19.98),
#                       longitude = c(116.59, 117.34),
#                       fields = "CRW_DHW",
#                       store = disk(path = paste0("data/", park, "/spatial/oceanography"),
#                                    overwrite = TRUE))
# 
#   # Get the actual filename that was saved
#   downloaded_file <- str_replace_all(response$summary$filename, "\\\\", "/") %>%
#     str_remove_all(paste0(getwd(), "/"))
# 
#   # Rename the file
#   file.rename(from = downloaded_file, to = new_filename)
# 
#   # Clear the rerddap cache
#   rerddap::cache_list()
#   rerddap::cache_delete_all()
# } else {
#   message("File already exists: ", new_filename)
# }
# 
# nc_dhw <- open.nc(paste0("data/", park, "/spatial/oceanography/DHW.nc"),
#                   write = TRUE)
# print.nc(nc_dhw) # shows you all the file details
# time_nc <- var.get.nc(nc_dhw, 'time')
# time_nc_dhw <- utcal.nc("seconds since 1970-01-01T00:00:00Z", time_nc, type = "c")
# dates_dhw <- as.Date(time_nc_dhw)
# close.nc(nc_dhw)
# 
# rast_dhw <- terra::rast(paste0("data/", park, "/spatial/oceanography/DHW.nc"),
#                         subds = "CRW_DHW")
# time(rast_dhw) <- dates_dhw
# names(rast_dhw) <- dates_dhw
# plot(rast_dhw)
# 
# dhw_tsdf <- terra::global(rast_dhw, fun = "mean", na.rm = T) %>%
#   tibble::rownames_to_column() %>%
#   cbind(terra::global(rast_dhw, fun = "sd", na.rm = T)) %>%
#   tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
#   dplyr::group_by(year, month) %>%
#   summarise(dhw = mean(mean, na.rm = T),
#             sd = mean(sd, na.rm = T)) %>%
#   ungroup() %>%
#   dplyr::mutate(season = case_when(month %in% c("03", "04", "05") ~ "Autumn",
#                                    month %in% c("06", "07", "08") ~ "Winter",
#                                    month %in% c("09", "10", "11") ~ "Spring",
#                                    month %in% c("12", "01", "02") ~ "Summer")) %>%
#   glimpse()
# 
# # Plot to find maximal values for DHW to plot
# test <- dhw_tsdf %>%
#   dplyr::mutate(year = as.numeric(year)) %>%
#   dplyr::group_by(year) %>%
#   summarise(dhw = mean(dhw, na.rm = T), sd = mean(sd, na.rm = T)) %>%
#   ungroup() %>%
#   glimpse()
# ggplot() +
#   geom_line(data = test, aes(x = year, y = dhw)) +
#   scale_x_continuous(labels = as.character(test$year), breaks = test$year)
# 
# saveRDS(dhw_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_DHW_time-series.rds"))
# 
# dhw.2011 <- subset(rast_dhw, year(time(rast_dhw)) == 2013 & month(time(rast_dhw)) == 3) %>%
#   mean(na.rm = T)
# names(dhw.2011) <- "March 2013"
# plot(dhw.2011)
# dhw.2012 <- subset(rast_dhw, year(time(rast_dhw)) == 2022 & month(time(rast_dhw)) == 4) %>%
#   mean(na.rm = T)
# names(dhw.2012) <- "April 2022"
# plot(dhw.2012)
# 
# dhw <- rast(list(dhw.2011, dhw.2012))
# plot(dhw)
# 
# saveRDS(dhw, paste0("data/", park, "/spatial/oceanography/", name, "_DHW_raster.rds"))
# 
# # Acifidication
# nc_acid <- open.nc(paste0("data/", park, "/spatial/oceanography/Acidification.nc"),
#                   write = TRUE)
# print.nc(nc_acid) # shows you all the file details
# time_nc <- var.get.nc(nc_acid, 'TIME')
# time_nc_acid <- utcal.nc("months since 1800-01-01 00:00:00", time_nc, type = "c")
# dates_acid <- as.Date(time_nc_acid)
# close.nc(nc_acid)
# 
# rast_acid <- terra::rast(paste0("data/", park, "/spatial/oceanography/Acidification.nc"),
#                          subds = "pH_T")
# time(rast_acid) <- dates_acid
# names(rast_acid) <- dates_acid
# plot(rast_acid)
# 
# acid_tsdf <- terra::global(rast_acid, fun = "mean", na.rm = T) %>%
#   tibble::rownames_to_column() %>%
#   cbind(terra::global(rast_acid, fun = "sd", na.rm = T)) %>%
#   tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
#   dplyr::group_by(year, month) %>%
#   summarise(acidification = mean(mean, na.rm = T),
#             sd = mean(sd, na.rm = T)) %>%
#   ungroup() %>%
#   dplyr::mutate(season = case_when(month %in% c("03", "04", "05") ~ "Autumn",
#                                    month %in% c("06", "07", "08") ~ "Winter",
#                                    month %in% c("09", "10", "11") ~ "Spring",
#                                    month %in% c("12", "01", "02") ~ "Summer")) %>%
#   glimpse()
# 
# saveRDS(acid_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_Acidification_time-series.rds"))
