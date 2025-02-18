###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Species occurence from this study, ALA and Keesing 2019
# Task:    Create species lists
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(CheckEM)
library(sf)

# Load species occurence data downloaded from https://www.ala.org.au/
ala <- read.csv("data/raw/records-2025-01-29.csv") %>%
  clean_names() %>%
  dplyr::select(decimallatitude, decimallongitude, class, order, family, genus, species) %>%
  distinct() %>%
  dplyr::filter(class %in% c("Actinopterygii", "Chondrichthyes")) %>%
  glimpse()

# Convert to spatial file
ala_sf <- st_as_sf(ala, coords = c("decimallongitude", "decimallatitude"), crs = 4326)

# Load Dampier Marine Park
parks <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% "Dampier") %>%
  glimpse()

# Intersect ALA species with the marine park
mp_species <- st_intersection(ala_sf, parks) %>%
  distinct(family, genus, species) %>%
  dplyr::filter(!species %in% "") %>%
  dplyr::mutate(species = sub("^\\S+\\s+(\\S+)$", "\\1", species)) %>%
  glimpse()

ggplot() +
  geom_sf(data = parks) +
  geom_sf(data = ala_sf) +
  theme_minimal() +
  coord_sf()

# Load BRUV count data
dat <- readRDS("data/raw/dampierAMP_BRUVs_complete_count.RDS") %>%
  dplyr::filter(!family %in% c("Elapidae", "Cheloniidae", "Sepiidae", "SUS", "Loliginidae", "Unknown")) %>%
  distinct(family, genus, species) %>%
  dplyr::filter(!species %in% c("spp", "sp1", "sp3", "sp")) %>%
  distinct() %>%
  glimpse()

# Checking the spps
dat_not <- readRDS("data/raw/dampierAMP_BRUVs_complete_count.RDS") %>%
  dplyr::filter(!family %in% c("Elapidae", "Cheloniidae", "Sepiidae", "SUS", "Loliginidae", "Unknown")) %>%
  distinct(family, genus, species) %>%
  dplyr::filter(species %in% c("spp", "sp1", "sp3", "sp")) %>%
  distinct() %>%
  glimpse()

# Find species in UWA data but not in ALA data
UWA_not_ALA <- anti_join(dat, mp_species, by = c("family", "genus", "species"))

# Find species in ALA data but not in UWA data
ALA_not_UWA <- anti_join(mp_species, dat, by = c("family", "genus", "species"))

# Load the data from Keesing 2019 report
keesing <- read.csv("data/raw/keesing_2019_species.csv") %>%
  clean_names() %>%
  separate_wider_delim(species, names = c("genus", "species"), delim = " ",
                       too_many = "drop") %>%
  # dplyr::filter(!is.na(pres_2017)) %>%
  dplyr::filter(!species %in% c("sp.", "-")) %>%
  dplyr::select(family, genus, species) %>%
  glimpse()

# Find species in UWA data but not in keesing data
UWA_not_keesing <- anti_join(dat, keesing, by = c("family", "genus", "species"))

# Find species in keesing data but not in UWA data
keesing_not_UWA <- anti_join(keesing, dat, by = c("family", "genus", "species"))

# Make a list of all the unique species seen across the three datasets
all_species <- mp_species %>%
  full_join(dat, by = c("family", "genus", "species")) %>%
  full_join(keesing, by = c("family", "genus", "species"))
