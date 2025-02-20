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
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, depth_m, status, site, location, successful_count, successful_length,
                successful_habitat_forward, successful_habitat_backward,
                observer_count, observer_length, observer_habitat_forward, observer_habitat_backward) %>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  glimpse()

# Save metadata
write.csv(metadata, file = paste0("data/raw/", name, "_BRUVs_metadata.csv"), row.names = F)

# Load and format count data
codes <- australia_life_history %>%
  dplyr::select(family, genus, species, caab_code)

count <- read_points("data/raw/temp/BRUVs", method = "BRUVs") %>%
  clean_names() %>%
  right_join(metadata) %>% # Join back samples with no fish
  dplyr::filter(successful_count %in% "Yes") %>%
  dplyr::select(campaignid, sample, family, genus, species, number, frame) %>%
  group_by(campaignid, sample, family, genus, species, frame) %>%
  summarise(count = sum(number)) %>%
  ungroup() %>%
  dplyr::select(-frame) %>%
  dplyr::mutate(family = if_else(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), 
                                 "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = if_else(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), 
                                "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = if_else(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_, "sp", "sp1", "sp3", "sp10"), 
                                  "spp", as.character(species))) %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::slice(which.max(count)) %>%
  dplyr::ungroup() %>%
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  replace_na(list(count = 0)) %>%
  dplyr::filter(!is.na(family)) %>% # If you have samples with no fish, then complete will add NAs in
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  left_join(metadata) %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  left_join(codes) %>%
  dplyr::mutate(caab_code = if_else(genus %in% "Iniistius" & species %in% "opalus", "37384923", caab_code)) %>%
  glimpse()

missing <- count %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(family, genus, species) # These are fine to get yeet

# Save count data
write.csv(count, file = paste0("data/raw/", name, "_BRUVs_complete_count.csv"), row.names = F)

# Load and format length data
length <- read_em_length("data/raw/temp/BRUVs") %>%
  clean_names() %>%
  right_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes", !is.na(family)) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, range, precision, rms, number) %>%
  dplyr::rename(range_mm = range, precision_mm = precision, rms_mm = rms) %>%
  dplyr::mutate(family = if_else(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), 
                                 "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = if_else(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), 
                                "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = if_else(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_, "sp", "sp1", "sp3", "sp10"), 
                                  "spp", as.character(species))) %>%
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  replace_na(list(number = 0)) %>%
  dplyr::filter(!is.na(family)) %>% # If you have samples with no fish, then complete will add NAs in
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  left_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  left_join(codes) %>%
  dplyr::mutate(caab_code = if_else(genus %in% "Iniistius" & species %in% "opalus", "37384923", caab_code)) %>%
  glimpse()

missing <- length %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(family, genus, species) # These are fine to get yeet

# Save length data
write.csv(length, file = paste0("data/raw/", name, "_BRUVs_complete_length.csv"),
          row.names = F)

# Load and format benthos data
benthos <- read_TM("data/raw/temp/BRUVs", sample = "opcode") %>%
  dplyr::filter(relief_annotated %in% "no") %>%
  dplyr::select(campaignid, sample, level_2, level_3, scientific) %>%
  dplyr::filter(!level_2 %in% c("Fishes", "Unscorable", NA)) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(campaignid, sample, level_2, level_3, scientific) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  dplyr::mutate(level_4 = if_else(level_2 %in% "Sessile invertebrates", "", NA),
                scientific = if_else(level_2 %in% "Sessile invertebrates", "", scientific),
                level_3 = if_else(level_2 %in% "Sessile invertebrates", "", level_3)) %>%
  left_join(catami) %>%
  glimpse()

write.csv(benthos, file = paste0("data/raw/", name, "_BRUVs_benthos.csv"),
          row.names = F)

# Load and format relief data
relief <- read_TM("data/raw/temp/BRUVs", sample = "opcode") %>%
  dplyr::filter(relief_annotated %in% c("yes", "Yes"),
                !is.na(sample)) %>%
  dplyr::select(campaignid, sample, level_5) %>%
  dplyr::filter(!level_5 %in% c(NA)) %>%
  left_join(metadata) %>%
  left_join(catami) %>%
  glimpse()

# Save relief data
write.csv(relief, file = paste0("data/raw/", name, "_BRUVs_relief.csv"),
          row.names = F)

## BOSS synthesis
# Load and format metadata
metadata <- read_metadata("data/raw/temp/BOSS", method = "BOSS") %>%
  clean_names() %>%
  dplyr::select(campaignid, period, longitude_dd, latitude_dd, date_time, depth_m, status, site, location, successful_count, successful_length,
                successful_habitat_panoramic, successful_habitat_downwards,
                observer_count, observer_length, observer_habitat_panoramic, observer_habitat_downwards) %>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  glimpse()

# Save metadata
write.csv(metadata, file = paste0("data/raw/", name, "_BOSS_metadata.csv"),
          row.names = F)

# Load and format benthos data
benthos <- read_TM("data/raw/temp/BOSS", sample = "opcode") %>%
  dplyr::filter(relief_annotated %in% "no") %>%
  dplyr::select(campaignid, sample, level_2, level_3, scientific) %>%
  dplyr::filter(!level_2 %in% c("Fishes", "Unscorable", NA)) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(campaignid, sample, level_2, level_3, scientific) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  dplyr::mutate(level_4 = if_else(level_2 %in% "Sessile invertebrates", "", NA),
                scientific = if_else(level_2 %in% "Sessile invertebrates", "", scientific),
                level_3 = if_else(level_2 %in% "Sessile invertebrates", "", level_3)) %>%
  left_join(catami) %>%
  left_join(metadata) %>%
  glimpse()

# Save benthos data
write.csv(benthos, file = paste0("data/raw/", name, "_BOSS_benthos.csv"),
          row.names = F)

# Load and format relief data
relief <- read_TM("data/raw/temp/BOSS", sample = "opcode") %>%
  dplyr::filter(relief_annotated %in% "yes") %>%
  dplyr::select(campaignid, sample, level_5) %>%
  dplyr::filter(!level_5 %in% c(NA)) %>%
  left_join(metadata) %>%
  left_join(catami) %>%
  glimpse()

# Save relief data
write.csv(relief, file = paste0("data/raw/", name, "_BOSS_relief.csv"))
