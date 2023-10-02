###
# Project: OMP Ngarluma
# Data:    BRUVS, BOSS
# Task:    Add metadata to meg labsheets from marks added to iPads using collector 
# author:  Claude
# date:    October 2023
##

rm(list = ls())

library(tidyverse)
library(GlobalArchive)
library(lubridate)
library(googlesheets4)
library(sf)
library(measurements)

url <- "https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit?usp=sharing"

# Find valid timezone ----
# Search for Perth, change to other location if the samples were not in WA.
grep("Perth", OlsonNames(), value = TRUE)

# Spatial files ----
sf_use_s2(F)

marine.parks <- st_read("data/spatial/shapefiles/marine-parks-all.shp") %>%
  st_transform(4326) %>%
  glimpse()

plot(marine.parks)

sampling.never.starts.after <- "12:00:00"

# BOSS
boss.metadata.names <- c(period = NA_real_,
                         latitude = NA_real_,
                         longitude = NA_real_,
                         date_time = NA_real_,
                         site = NA_real_,
                         location = NA_real_,
                         # status = NA_real_, # add in from shapefiles
                         depth = NA_real_,
                         successful_count = NA_real_,
                         successful_length = NA_real_,
                         observer_count = NA_real_,
                         observer_length = NA_real_,
                         forwards_habitat_image_saved = NA_real_,
                         downwards_habitat_image_saved = NA_real_,
                         successful_habitat_forwards = NA_real_,
                         successful_habitat_downwards = NA_real_,
                         observer_habitat_forwards = NA_real_,
                         observer_habitat_downwards = NA_real_,
                         comment = NA_real_)

# BOSS
boss.cameras <- read.csv("data/metadata/BOSS/2023-09_Dampier_BOSS_cameras_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = system.) %>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC"))  %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  dplyr::mutate(noon = paste(date, sampling.never.starts.after)) %>% 
  dplyr::mutate(noon = ymd_hms(noon, tz = "Australia/Perth"))  %>%
  dplyr::mutate(date = ymd(date)) %>%
  dplyr::mutate(date = if_else(noon < local.time, (date + days(1)), date)) %>%
  dplyr::select(date, system.number, 
                north.top.camera, north.bottom.camera,
                east.top.camera, east.bottom.camera,
                south.top.camera, south.bottom.camera,
                west.top.camera, west.bottom.camera,
                downwards.camera) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(!c(date, system.number), names_to = "position", values_to = "camera.number") %>%
  separate(position, into = c("face", "position"), sep = "[^[:alnum:]]+", extra = "merge") %>%
  dplyr::mutate(position = if_else(face %in% "downwards", "top.camera", position)) %>%
  distinct() %>%
  pivot_wider(names_from = position, values_from = camera.number) %>%
  dplyr::select(system.number, everything()) %>%
  dplyr::mutate(system.number = "Sharktip BOSS") %>%
  glimpse()

names(boss.cameras)

# add to labsheet on google drive
sheet_append(url, boss.cameras, sheet = "2023-09_Dampier_BOSS_cameras")

# this is the only one that goes to the metadata!!
boss.metadata <- read.csv("data/metadata/BOSS/2023-09_Dampier_BOSS_metadata_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = system., depth = depth.m., longitude = x, latitude = y, comment = notes) %>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC")) %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::select(system.number, sample, depth, comment, latitude, longitude, time, local.time) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  dplyr::mutate(local.time = as.character(local.time)) %>%
  dplyr::mutate(date_time = paste0(str_replace_all(local.time, " ", "T"), "+08:00")) %>%
  glimpse()

# metadata as sf
boss.metadata.sf <- st_as_sf(boss.metadata, coords = c("longitude", "latitude"), crs = 4326)

boss.metadata.parks <- boss.metadata.sf %>%
  st_intersection(marine.parks %>% dplyr::select(geometry, ZoneName)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::mutate(status = ifelse(ZoneName %in% c("National Park Zone", "Sanctuary Zone"), "No-take", "Fished")) %>%
  dplyr::rename(zone = ZoneName) %>%
  mutate(across(everything(), as.character)) %>%
  arrange(date_time) %>%
  glimpse()

boss.metadata.zones <- boss.metadata %>%
  mutate(across(everything(), as.character)) %>%
  full_join(boss.metadata.parks) %>%
  dplyr::mutate(status = if_else(status %in% c(NA, NULL), "Fished", status)) %>%
  add_column(!!!boss.metadata.names[!names(boss.metadata.names) %in% names(.)]) %>%
  dplyr::select(sample, latitude, longitude, date_time,  
                site, location, status, depth, successful_count, successful_length, 
                observer_count, observer_length, forwards_habitat_image_saved, downwards_habitat_image_saved, 
                successful_habitat_forwards, successful_habitat_downwards, observer_habitat_forwards,
                observer_habitat_downwards, comment) %>%
  glimpse()


# add to labsheet on google drive
sheet_append(url, boss.metadata.zones, sheet = "2023-09_Dampier_BOSS")

# BRUV
bruv.metadata.names <- c(opcode = NA_real_,
                         latitude = NA_real_,
                         longitude = NA_real_,
                         date_time = NA_real_,
                         site = NA_real_,
                         location = NA_real_,
                         # status = NA_real_, # add in from shapefiles
                         depth = NA_real_,
                         successful_count = NA_real_,
                         successful_length = NA_real_,
                         observer_count = NA_real_,
                         observer_length = NA_real_,
                         forwards_habitat_image_saved = NA_real_,
                         backwards_habitat_image_saved = NA_real_,
                         successful_habitat_forwards = NA_real_,
                         successful_habitat_backwards = NA_real_,
                         observer_habitat_forwards = NA_real_,
                         observer_habitat_backwards = NA_real_,
                         comment = NA_real_,
                         left_cam = NA_real_,
                         right_cam = NA_real_,
                         rear_cam = NA_real_)

bruv.cameras <- read.csv("data/metadata/BRUV/2023-09_Dampier_BRUV_cameras_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = system., left_cam = left.camera, right_cam = right.camera, rear_cam = rear.camera)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC"))  %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  dplyr::mutate(noon = paste(date, sampling.never.starts.after)) %>% 
  dplyr::mutate(noon = ymd_hms(noon, tz = "Australia/Perth"))  %>%
  dplyr::mutate(date = ymd(date)) %>%
  dplyr::mutate(date = if_else(noon < local.time, (date + days(1)), date)) %>%
  dplyr::select(system.number, left_cam, right_cam, rear_cam, date) %>%
  dplyr::mutate(date = as.character(date)) %>%
  glimpse()

names(bruv.cameras)

# this is the only one that goes to the metadata!!
bruv.metadata <- read.csv("data/metadata/BRUV/2023-09_Dampier_BRUV_metadata_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = system., depth = depth.m., longitude = x, latitude = y, comment = notes) %>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC")) %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::select(system.number, sample, depth, comment, latitude, longitude, time, local.time) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  left_join(bruv.cameras) %>%
  dplyr::mutate(local.time = as.character(local.time)) %>%
  dplyr::mutate(date_time = paste0(str_replace_all(local.time, " ", "T"), "+08:00")) %>%
  glimpse()

# metadata as sf
bruv.metadata.sf <- st_as_sf(bruv.metadata, coords = c("longitude", "latitude"), crs = 4326)

bruv.metadata.parks <- bruv.metadata.sf %>%
  st_intersection(marine.parks %>% dplyr::select(geometry, ZoneName)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::mutate(status = ifelse(ZoneName %in% c("National Park Zone", "Sanctuary Zone"), "No-take", "Fished")) %>%
  dplyr::rename(zone = ZoneName) %>%
  mutate(across(everything(), as.character)) %>%
  arrange(date_time) %>%
  glimpse()

bruv.metadata.zones <- bruv.metadata %>%
  mutate(across(everything(), as.character)) %>%
  full_join(bruv.metadata.parks) %>%
  dplyr::mutate(status = if_else(status %in% c(NA, NULL), "Fished", status)) %>%
  add_column(!!!bruv.metadata.names[!names(bruv.metadata.names) %in% names(.)]) %>%
  dplyr::select(sample, latitude, longitude, date_time,  
                site, location, status, depth, successful_count, successful_length, 
                observer_count, observer_length, forwards_habitat_image_saved, backwards_habitat_image_saved, 
                successful_habitat_forwards, successful_habitat_backwards, observer_habitat_forwards,
                observer_habitat_backwards, comment, left_cam, right_cam, rear_cam) %>%
  glimpse()

# add to labsheet on google drive
sheet_append(url, bruv.metadata.zones, sheet = "2023-09_Dampier_stereo-BRUVs")

