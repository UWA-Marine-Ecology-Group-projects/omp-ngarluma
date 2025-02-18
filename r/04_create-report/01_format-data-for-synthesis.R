###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Raw fish and benthos annotations
# Task:    Format annotations to match the format returned from GlobalArchive synthesis API
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list = ls())

# Set the study name
name = "dampierAMP"

# Load libraries
library(tidyverse)
library(CheckEM)

## BRUV synthesis
# Load and format metadata
metadata <- read_metadata("data/raw/temp/BRUVs", method = "BRUVs") %>%
  clean_names() %>%
  dplyr::select(campaignid, opcode, longitude_dd, latitude_dd, date_time, depth_m, status, site, location, successful_count, successful_length,
                successful_habitat_forward, successful_habitat_backward,
                observer_count, observer_length, observer_habitat_forward, observer_habitat_backward) %>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  glimpse()

# Save metadata
saveRDS(metadata, file = paste0("data/raw/", name, "_BRUVs_metadata.RDS"))

# Load and format count data
count <- read_points("data/raw/temp/BRUVs", method = "BRUVs") %>%
  clean_names() %>%
  right_join(metadata) %>% # Join back samples with no fish
  dplyr::filter(successful_count %in% "Yes") %>%
  dplyr::select(campaignid, opcode, family, genus, species, number, frame) %>%
  group_by(campaignid, opcode, family, genus, species, frame) %>%
  summarise(count = sum(number)) %>%
  ungroup() %>%
  dplyr::select(-frame) %>%
  dplyr::group_by(campaignid, opcode, family, genus, species) %>%
  dplyr::slice(which.max(count)) %>%
  dplyr::ungroup() %>%
  tidyr::complete(nesting(campaignid, opcode), nesting(family, genus, species)) %>%
  replace_na(list(count = 0)) %>%
  dplyr::filter(!is.na(family)) %>% # If you have samples with no fish, then complete will add NAs in
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  left_join(metadata) %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  glimpse()

# Save count data
saveRDS(count, file = paste0("data/raw/", name, "_BRUVs_complete_count.RDS"))

# Load and format length data
length <- read_em_length("data/dampier/raw/temp/BRUVs") %>%
  clean_names() %>%
  right_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes", !is.na(family)) %>%
  dplyr::select(campaignid, opcode, family, genus, species, length_mm, number) %>%
  uncount(number) %>%
  dplyr::mutate(number = 1) %>%
  tidyr::complete(nesting(campaignid, opcode), nesting(family, genus, species)) %>%
  replace_na(list(number = 0)) %>%
  dplyr::filter(!is.na(family)) %>% # If you have samples with no fish, then complete will add NAs in
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  left_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  glimpse()

# Save length data
saveRDS(length, file = paste0("data/dampier/raw/", name, "_BRUVs_complete_length.RDS"))

# Load and format benthos data
benthos <- read_TM("data/dampier/raw/temp/BRUVs", sample = "opcode") %>%
  dplyr::filter(relief_annotated %in% "no") %>%
  dplyr::select(campaignid, sample, level_2, level_3, scientific) %>%
  dplyr::filter(!level_2 %in% c("Fishes", "Unscorable", NA)) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(campaignid, sample, level_2, level_3, scientific) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  dplyr::rename(opcode = sample) %>% # Change the function
  left_join(metadata) %>%
  glimpse()

saveRDS(benthos, file = paste0("data/dampier/raw/", name, "_BRUVs_benthos.RDS"))

# Load and format relief data
relief <- read_TM("data/dampier/raw/temp/BRUVs", sample = "opcode") %>%
  dplyr::mutate(campaignid = "2023-09_Dampier_stereo-BRUVs") %>% # REMOVE - data needs fixing
  dplyr::filter(relief_annotated %in% c("yes", "Yes"),
                !is.na(sample)) %>%
  dplyr::select(campaignid, sample, level_5) %>%
  dplyr::filter(!level_5 %in% c(NA)) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(mean_relief = mean(as.numeric(level_5)),
                   sd_relief = sd(as.numeric(level_5))) %>%
  ungroup() %>%
  dplyr::rename(opcode = sample) %>% # Change the function
  left_join(metadata) %>%
  glimpse()

# Save relief data
saveRDS(relief, file = paste0("data/dampier/raw/", name, "_BRUVs_relief.RDS"))

## BOSS synthesis
# Load and format metadata
metadata <- read_metadata("data/dampier/raw/temp/BOSS", method = "BOSS") %>%
  clean_names() %>%
  dplyr::select(campaignid, period, longitude_dd, latitude_dd, date_time, depth_m, status, site, location, successful_count, successful_length,
                successful_habitat_panoramic, successful_habitat_downwards,
                observer_count, observer_length, observer_habitat_panoramic, observer_habitat_downwards) %>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  glimpse()

# Save metadata
saveRDS(metadata, file = paste0("data/dampier/raw/", name, "_BOSS_metadata.RDS"))

# Load and format benthos data
benthos <- read_TM("data/dampier/raw/temp/BOSS", sample = "opcode") %>%
  dplyr::filter(relief_annotated %in% "no") %>%
  dplyr::select(campaignid, sample, level_2, level_3, scientific) %>%
  dplyr::filter(!level_2 %in% c("Fishes", "Unscorable", NA)) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(campaignid, sample, level_2, level_3, scientific) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  dplyr::rename(period = sample) %>% # Change the function
  left_join(metadata) %>%
  glimpse()

# Save benthos data
saveRDS(benthos, file = paste0("data/dampier/raw/", name, "_BOSS_benthos.RDS"))

# Load and format relief data
relief <- read_TM("data/dampier/raw/temp/BOSS", sample = "opcode") %>%
  dplyr::filter(relief_annotated %in% "yes") %>%
  dplyr::select(campaignid, sample, level_5) %>%
  dplyr::filter(!level_5 %in% c(NA)) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(mean_relief = mean(as.numeric(level_5)),
                   sd_relief = sd(as.numeric(level_5))) %>%
  ungroup() %>%
  dplyr::rename(period = sample) %>% # Change the function
  left_join(metadata) %>%
  glimpse()

# Save relief data
saveRDS(relief, file = paste0("data/dampier/raw/", name, "_BOSS_relief.RDS"))
