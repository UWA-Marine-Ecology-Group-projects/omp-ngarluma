###
# Project: Parks Australia - Our Marine Parks Ngarluma
# Data:    Fish data synthesis
# Task:    Create benthic metrics for modelling
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list = ls())

# Load necessary libraries
library(CheckEM)
library(tidyverse)
library(sf)
library(here)
library(leaflet)
library(googlesheets4)
library(terra)
library(tidyterra)
library(plotly)

# Set the study name
name <- "DampierAMP"

# Load joined metadata and bathymetry derivatives
metadata_bathy_derivatives <- readRDS(paste0("data/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

# Load BRUV metadata
metadata <- readRDS(paste0("data/staging/", name, "_BRUVs_metadata.RDS")) %>%
  dplyr::rename(sample = opcode) %>%
  glimpse()

# Load formatted benthic data from 03_create-metrics_habitat
benthos <- readRDS(paste0("data/tidy/", name, "_benthos-count.RDS")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, sample, reef, total_pts) %>%
  dplyr::mutate(reef = reef/total_pts) %>% # Model reef as proportion for fish prediction
  glimpse()

# Load WA specific length of maturity data
maturity_mean <- CheckEM::maturity %>%
  dplyr::filter(!marine_region %in% c("SW")) %>% # Change here for your spatial location
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

# Load BRUV fish count data
count <- readRDS(paste0("data/staging/", name, "_BRUVs_complete_count.RDS")) %>%
  dplyr::rename(sample = opcode) %>%
  dplyr::select(campaignid, sample, family, genus, species, count) %>%
  dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
  glimpse()

# Create total abundance (ta) and species richness (sr)
ta.sr <- count %>%
  dplyr::select(-c(family, genus, species)) %>%
  dplyr::group_by(campaignid, sample, scientific_name) %>%
  pivot_wider(names_from = "scientific_name", values_from = count) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_abundance = rowSums(.[, 3:(ncol(.))], na.rm = T),
                species_richness = rowSums(.[, 3:(ncol(.))] > 0)) %>%
  dplyr::select(campaignid, sample, total_abundance, species_richness) %>%
  pivot_longer(cols = c("total_abundance", "species_richness"), names_to = "response", values_to = "number") %>%
  glimpse() # Should be nsamps * 2 = 594

# Create CTI using CheckEM function
cti <- CheckEM::create_cti(data = count) %>%
  dplyr::rename(number = cti) %>%
  dplyr::mutate(response = "cti") %>%
  glimpse()

# Join together all count (maxn) fish metrics
tidy_maxn <- bind_rows(ta.sr, cti) %>%
  dplyr::select(-c(log_count, w_sti)) %>%
  dplyr::left_join(benthos) %>%
  dplyr::left_join(metadata) %>% # To join samples without valid bathymetry derivatives
  dplyr::left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(reef), # GBR3-4 has no habitat
                !is.na(geoscience_aspect)) %>% # Not valid values for modelling so will remove them now
  glimpse()

# Save metric data for use in modelling scripts
saveRDS(tidy_maxn, file = paste0("data/tidy/", name, "_tidy-count.rds"))

# Load BRUV fish length data
length <- readRDS(paste0("data/staging/", name, "_BRUVs_complete_length.RDS")) %>%
  dplyr::rename(sample = opcode) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, number) %>%
  left_join(large_bodied_carnivores) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  glimpse()
length(unique(length$sample))

# Test to check which species are contained in the metric
all_species <- length %>%
  distinct(scientific_name) %>%
  glimpse()

test_species <- length %>%
  dplyr::filter(!is.na(l50)) %>%
  distinct(scientific_name) %>%
  glimpse()

metadata_length <- length %>%
  distinct(campaignid, sample) %>%
  glimpse()

# Create metric for large bodied carnivores greater than length of maturity
big_carn <- length %>%
  dplyr::filter(length_mm > l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "greater than Lm carnivores") %>%
  left_join(benthos) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(big_carn, number > 0))/nrow(big_carn)

# Create metric for large bodied carnivores smaller than length of maturity
small_carn <- length %>%
  dplyr::filter(length_mm < l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "smaller than Lm carnivores") %>%
  left_join(benthos) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(small_carn, number > 0))/nrow(small_carn)

# Join the two datasets together and join back to the metadata
tidy_length <- bind_rows(big_carn, small_carn) %>%
  dplyr::left_join(metadata) %>%
  dplyr::left_join(metadata_bathy_derivatives) %>%
  glimpse()

# Visualise spatial patterns in length metrics
preds <- readRDS(paste0("data/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
plot(preds)
names(preds)

ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_depth)) +
  geom_point(data = tidy_length,
             aes(x = longitude_dd, y = latitude_dd, size = number, colour = I(if_else(number == 0, "white", "darkblue"))),
             show.legend = F, alpha = 0.5) +
  facet_wrap(~response) +
  theme_classic() +
  coord_sf(crs = 4326)

# Convert the raster data to a dataframe for use in Plotly
preds_df <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)

# Create an interactive plot using Plotly
fig <- plot_ly() %>%
  # Add raster layer as a heatmap
  add_trace(
    data = preds_df,
    x = ~x, y = ~y, z = ~geoscience_depth,
    type = "heatmap",
    colors = colorRamp(c("white", "blue"))
  ) %>%
  # Add points with hover text showing the "number" column
  add_trace(
    data = dplyr::filter(tidy_length, response %in% "smaller than Lm carnivores"),
    x = ~longitude_dd, y = ~latitude_dd,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = ~number, color = ~if_else(number == 0, "white", "darkblue")),
    text = ~sample,
    hoverinfo = "text"
  ) %>%
  layout(
    title = "Interactive Plot with Raster and Points",
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude")
  )

fig

# Save tidy length data for use in modelling scripts
saveRDS(tidy_length, file = paste0("data/tidy/", name, "_tidy-length.rds"))

