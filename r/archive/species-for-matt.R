library(tidyverse)
library(CheckEM)
library(sf)
library(terra)
library(tidyterra)
library(ggnewscale)
library(png)

dat <- read.delim("data/raw/em export/2023-09_Dampier_stereo-BRUVs_Points.txt") %>%
  CheckEM::clean_names() %>%
  dplyr::select(opcode, family, genus, species, number, periodtime) %>%
  dplyr::group_by(opcode, periodtime, family, genus, species) %>%
  dplyr::summarise(maxn = sum(number)) %>%
  ungroup() %>%
  dplyr::group_by(opcode, family, genus, species) %>%
  slice_max(maxn) %>%
  ungroup() %>%
  glimpse()

all_species <- dat %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(sum_maxn = sum(maxn)) %>%
  ungroup() %>%
  left_join(CheckEM::australia_life_history) %>%
  dplyr::select(scientific_name, australian_common_name, sum_maxn) %>%
  dplyr::filter(!is.na(scientific_name)) %>%
  glimpse()

metadata <- read.csv("data/raw/em export/2023-09_Dampier_stereo-BRUVs_metadata.csv") %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  dplyr::select(opcode, longitude_dd, latitude_dd) %>%
  glimpse()

plot_data <- dat %>%
  left_join(metadata) %>%
  left_join(CheckEM::australia_life_history) %>%
  dplyr::select(opcode, longitude_dd, latitude_dd, scientific_name, australian_common_name, maxn) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  glimpse()

bathy <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(ext(116.5, 117.4, -20.9, -20.2)) %>%
  clamp(upper = 0, values = F)
plot(bathy)  

aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

amp <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ResName %in% "Dampier") %>%
  dplyr::mutate(ZoneName = factor(.$ZoneName, levels = c("Multiple Use Zone", "Habitat Protection Zone", "National Park Zone"))) %>%
  arrange(ZoneName)

amp_cols <- scale_colour_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "National Park Zone" = "#7bbc63"),
                                name = "Australian Marine Park")

g.s <- as.raster(readPNG("data/images/Gnathanodon speciosus-3cmL.png"))
t.g <- as.raster(readPNG("data/images/Carangoides gymnostethus 3cm.png"))
s.c <- as.raster(readPNG("data/images/ScomberoidesCommersonnianusFishBase.png"))

# Golden trevally
p1 <- ggplot() +
  geom_spatraster(data = bathy, show.legend = F) +
  scale_fill_gradient(low = "#060d61", high = "#b1eafc", na.value = "transparent") +
  new_scale_fill() +
  geom_sf(data = aus) +
  geom_sf(data = amp, aes(colour = ZoneName), fill = NA, linewidth = 1, lineend = "square") +
  amp_cols +
  new_scale_colour() +
  geom_sf(data = filter(plot_data, scientific_name %in% "Gnathanodon speciosus"), aes(size = maxn),
          fill = "#c79d4c", shape = 21, colour = "black") +
  annotate(geom = "text", x = c(116.93, 117.07, 117.21), 
           y = c(-20.4, -20.42, -20.6287), 
           label = c("Legendre\nIsland", "Delambre\nIsland", "Point\nSamson"),
           fontface = "italic", size = 2) +
  annotation_raster(g.s, xmin = 116.87, xmax = 117.13, ymin = -20.6, ymax = -20.5) +
  labs(size = "Golden trevally\nAbundance", x = "", y = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b1eafc", colour = NA)) +
  coord_sf(xlim = c(116.82, ext(plot_data)[2]),
           ylim = c(-20.7, ext(plot_data)[4]),
           crs = 4326)
png(filename = "plots/golden-trevally-abundance.png", units = "in", res = 300, 
    height = 5.5, width = 8)
p1
dev.off()

# Bludger trevally
p2 <- ggplot() +
  geom_spatraster(data = bathy, show.legend = F) +
  scale_fill_gradient(low = "#060d61", high = "#b1eafc", na.value = "transparent") +
  new_scale_fill() +
  geom_sf(data = aus) +
  geom_sf(data = amp, aes(colour = ZoneName), fill = NA, linewidth = 1, lineend = "square") +
  amp_cols +
  new_scale_colour() +
  geom_sf(data = filter(plot_data, scientific_name %in% "Turrum gymnostethus"), aes(size = maxn),
          fill = "#c79d4c", shape = 21, colour = "black") +
  annotate(geom = "text", x = c(116.93, 117.07, 117.21), 
           y = c(-20.4, -20.42, -20.6287), 
           label = c("Legendre\nIsland", "Delambre\nIsland", "Point\nSamson"),
           fontface = "italic", size = 2) +
  annotation_raster(t.g, xmin = 116.9, xmax = 117.1, ymin = -20.59, ymax = -20.51) +
  labs(size = "Bludger trevally\nAbundance", x = "", y = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b1eafc", colour = NA)) +
  coord_sf(xlim = c(116.82, ext(plot_data)[2]),
           ylim = c(-20.7, ext(plot_data)[4]),
           crs = 4326)
png(filename = "plots/bludger-trevally-abundance.png", units = "in", res = 300, 
    height = 5.5, width = 8)
p2
dev.off()

# Queenfish
p3 <- ggplot() +
  geom_spatraster(data = bathy, show.legend = F) +
  scale_fill_gradient(low = "#060d61", high = "#b1eafc", na.value = "transparent") +
  new_scale_fill() +
  geom_sf(data = aus) +
  geom_sf(data = amp, aes(colour = ZoneName), fill = NA, linewidth = 1, lineend = "square") +
  amp_cols +
  new_scale_colour() +
  geom_sf(data = filter(plot_data, scientific_name %in% "Scomberoides commersonnianus"), aes(size = maxn),
          fill = "#c79d4c", shape = 21, colour = "black") +
  annotate(geom = "text", x = c(116.93, 117.07, 117.21), 
           y = c(-20.4, -20.42, -20.6287), 
           label = c("Legendre\nIsland", "Delambre\nIsland", "Point\nSamson"),
           fontface = "italic", size = 2) +
  annotation_raster(s.c, xmin = 116.87, xmax = 117.13, ymin = -20.64, ymax = -20.46) +
  labs(size = "Queenfish\nAbundance", x = "", y = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b1eafc", colour = NA)) +
  coord_sf(xlim = c(116.82, ext(plot_data)[2]),
           ylim = c(-20.7, ext(plot_data)[4]),
           crs = 4326)
png(filename = "plots/queenfish-abundance.png", units = "in", res = 300, 
    height = 5.5, width = 8)
p3
dev.off()
