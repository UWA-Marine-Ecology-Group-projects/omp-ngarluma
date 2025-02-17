###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Benthic data synthesis
# Task:    Create benthic metrics for modelling
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list = ls())

# Load libraries
library(tidyverse)

# Set the study name
name <- "DampierAMP"

# Load benthic data
benthosboss <- readRDS(paste0("data/staging/", name, "_BOSS_benthos.RDS")) %>%
  dplyr::rename(sample = period)
benthosbruv <- readRDS(paste0("data/staging/", name, "_BRUVs_benthos.RDS")) %>%
  dplyr::rename(sample = opcode)

# Join benthic data and create metrics
benthos <- bind_rows(benthosboss, benthosbruv) %>%
  dplyr::select(campaignid, sample, level_2, level_3, count) %>%
  dplyr::mutate(habitat = case_when(level_2 %in% "Macroalgae" ~ "macroalgae",
                                    level_3 %in% "Unconsolidated (soft)" ~ "sand",
                                    level_3 %in% "Consolidated (hard)" ~ "rock",
                                    level_2 %in% "Cnidaria" ~ "black_octocorals",
                                    level_2 %in% "Sessile invertebrates" ~ "sessile_invertebrates",
                                    level_2 %in% "Sponges" ~ "sessile_invertebrates")) %>%
  dplyr::group_by(campaignid, sample, habitat) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  pivot_wider(names_from = habitat, values_from = count, values_fill = 0) %>%
  dplyr::mutate(total_pts = rowSums(.[c("sand", "sessile_invertebrates", "rock", "macroalgae", "black_octocorals")], na.rm = T),
                reef = macroalgae + rock + sessile_invertebrates + black_octocorals,
                sessile_invertebrates_all = sessile_invertebrates + black_octocorals) %>%
  glimpse()

# Save benthic data to use in modelling scripts
saveRDS(benthos, paste0("data/tidy/", name, "_benthos-count.RDS"))