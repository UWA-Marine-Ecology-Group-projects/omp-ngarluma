###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Fish count data from this study & Ningaloo
# Task:    Create comparison metrics between marine parks
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)

# Set the study name
name <- "DampierAMP"

# Load Dampier count data
tidy_maxn <- readRDS(paste0("data/tidy/", name, "_tidy-count.rds")) %>%
  glimpse()

# Summarise average depth across deployments
average_depth <- tidy_maxn %>%
  dplyr::mutate(depth_m = as.numeric(depth_m)) %>%
  summarise(depth = mean(depth_m, na.rm = T))

# Load Dampier length data
tidy_length <- readRDS(paste0("data/tidy/", name, "_tidy-length.rds")) %>%
  glimpse()

# Average number of fish per BRUV
fish_per_bruv <- tidy_maxn %>%
  dplyr::filter(response %in% "total_abundance") %>%
  summarise(number = mean(number, na.rm = T)) %>%
  glimpse()

# Average species richness per BRUV
sr_per_bruv <- tidy_maxn %>%
  dplyr::filter(response %in% "species_richness") %>%
  summarise(number = mean(number, na.rm = T)) %>%
  glimpse()

# Average number of large-bodied carnivores > Lm per BRUV
biglm_per_bruv <- tidy_length %>%
  dplyr::filter(response %in% "greater than Lm carnivores") %>%
  summarise(number = mean(number, na.rm = T)) %>%
  glimpse()

# Average number of large-bodied carnivores < Lm per BRUV
smollm_per_bruv <- tidy_length %>%
  dplyr::filter(response %in% "smaller than Lm carnivores") %>%
  summarise(number = mean(number, na.rm = T)) %>%
  glimpse()

# Compare to Ningaloo Marine Park data
count <- read.csv("data/raw/Parks-Ningaloo-synthesis.complete.maxn.csv") %>%
  dplyr::rename(count = maxn) %>%
  dplyr::select(campaignid, sample, family, genus, species, count) %>%
  dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
  glimpse()

# Average depth
average_depth <- read.csv("data/raw/Parks-Ningaloo-synthesis.complete.maxn.csv") %>%
  dplyr::filter(!depth %in% "?") %>%
  dplyr::mutate(depth = as.numeric(depth)) %>%
  summarise(depth = mean(depth, na.rm = T)) %>%
  glimpse()

# Create total abundance and species richness
ta.sr <- count %>%
  dplyr::select(-c(family, genus, species)) %>%
  dplyr::group_by(campaignid, sample, scientific_name) %>%
  pivot_wider(names_from = "scientific_name", values_from = count) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_abundance = rowSums(.[, 3:(ncol(.))], na.rm = T),
                species_richness = rowSums(.[, 3:(ncol(.))] > 0)) %>%
  dplyr::select(campaignid, sample, total_abundance, species_richness) %>%
  pivot_longer(cols = c("total_abundance", "species_richness"), names_to = "response", values_to = "number") %>%
  glimpse()

# Average fish per BRUV
fish_per_bruv <- ta.sr %>%
  dplyr::filter(response %in% "total_abundance") %>%
  summarise(number = mean(number, na.rm = T)) %>%
  glimpse()

# Average species richness per BRUV
sr_per_bruv <- ta.sr %>%
  dplyr::filter(response %in% "species_richness") %>%
  summarise(number = mean(number, na.rm = T)) %>%
  glimpse()

# Maturity data from WA
maturity_mean <- CheckEM::maturity %>%
  dplyr::filter(!marine_region %in% c("SW")) %>% # Change here for each marine park
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(l50 = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()

# Create large bodied carnivore metric
large_bodied_carnivores <- CheckEM::australia_life_history %>%
  dplyr::filter(fb_trophic_level > 2.8) %>%
  dplyr::filter(length_max_cm > 40) %>%
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes",
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes")) %>%
  left_join(maturity_mean) %>%
  dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm * 10) %>%
  dplyr::mutate(l50 = if_else(is.na(l50), fb_length_at_maturity_mm, l50)) %>%
  dplyr::filter(!is.na(l50)) %>%
  dplyr::select(family, genus, species, l50) %>%
  glimpse()

# Load Ningaloo length data
length <- read.csv("data/dampier/raw/Parks-Ningaloo-synthesis.expanded.length.csv") %>%
  dplyr::rename(length_mm = length) %>%
  dplyr::mutate(number = 1) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, number) %>%
  left_join(large_bodied_carnivores) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  glimpse()

# Check species
test_species <- length %>%
  dplyr::filter(!is.na(l50)) %>%
  distinct(scientific_name) %>%
  glimpse()

metadata_length <- length %>%
  distinct(campaignid, sample) %>%
  glimpse()

# Create large bodied carnivore > Lm
big_carn <- length %>%
  dplyr::filter(length_mm > l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "greater than Lm carnivores") %>%
  dplyr::glimpse()

# Summarise per BRUV
biglm_per_bruv <- big_carn %>%
  summarise(number = mean(number, na.rm = T)) %>%
  glimpse()

# Create large bodied carnivores < Lm
small_carn <- length %>%
  dplyr::filter(length_mm < l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "smaller than Lm carnivores") %>%
  dplyr::glimpse()

# Summarise per BRUV
smollm_per_bruv <- small_carn %>%
  summarise(number = mean(number, na.rm = T)) %>%
  glimpse()
