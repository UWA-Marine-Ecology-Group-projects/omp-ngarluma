###
# Project: Parks Murajuga
# Data:    250m bathy
# Task:    Overview maps
# author:  Claude Spencer
# date:    August 2022
##

rm(list = ls())

# Load libraries
library(dplyr)
library(nngeo)
library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)
library(marmap)
library(geosphere)

working.dir <- getwd()
setwd(working.dir)

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84" 

# Get data and sort spatial boundaries
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
st_crs(aus)         <- st_crs(aumpa)
damp_mp <- aumpa[aumpa$ResName%in%"Dampier",]
damp_sanc <- damp_mp[damp_mp$ZoneName%in%"National Park Zone",]                 # Maybe not needed for this stuff
# No state sanctuaries here yet ?
# wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2018.shp")                  # All wa mpas
# wa_mp <- wampa[wampa$NAME%in%"Ningaloo",]
# wa_sanc <- wa_mp[wa_mp$ZONE_TYPE%in%"Sanctuary Zone (IUCN IA)",]

cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit

# bathy <- rast("data/spatial/rasters/large/tile2c.txt") # Old tile bathy
# crs(bathy) <- wgscrs
# 
# damp_spat <- vect(damp_mp)
# bath_c <- crop(bathy, ext(116.5, 117.606993293854,
#                           -20.8, -19))                # Crop to general study area
# bath_c[bath_c > 0] <- NA
# plot(bath_c)
# plot(damp_mp, add = T)
# bathdf <- as.data.frame(bath_c, xy = T)
# saveRDS(bathdf, file = "data/spatial/rasters/GA_250m_bathy-trimmed.RDS")

bathy <- readRDS("data/spatial/rasters/GA_250m_bathy-trimmed.RDS")
bathy <- rast(bathy)
crs(bathy) <- wgscrs

slope <- terrain(bathy, v='slope', unit='degrees')
aspect <- terrain(bathy, v='aspect', unit='degrees')
hill <- shade(slope, aspect, angle = 70, direction = 0)

# To dataframes for plotting
hill <- as.data.frame(hill, xy = T, na.rm = T)
bathy <- as.data.frame(bathy, xy = T, na.rm = T)

colnames(bathy)[3] <- "Depth"

# saveRDS(bathy, file = "data/spatial/rasters/GA_250m_bathy-trimmed.RDS")

# build basic plot elements
p1 <- ggplot() +
  geom_tile(data = hill,aes(x = x, y = y, fill = lyr1), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = bathy%>%dplyr::filter(Depth > -40), aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis() +
  geom_contour(data = bathy, aes(x = x, y = y, z = Depth), breaks = c( -30, -70), 
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = damp_mp, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                "National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.4) +
  annotate(geom = "text", x = 117.1, y = -20.385, label = "30m",
           color = "white", size = 2) +
  coord_sf(xlim = c(116.8333, 117.5167), ylim = c(-20.56667, -20.3)) +
  labs(y = "Latitude", x = "Longitude")+
  geom_segment(aes(x = 116.847, xend = 116.847, y = -20.5424, yend = -20.1), 
               linetype = 2, alpha = 0.7, colour = "gray60", size = 1) +
  geom_segment(aes(x = 117.08, xend = 117.08, y = -20.67, yend = -20.1), 
               linetype = 2, alpha = 0.7, colour = "gray60", size = 1) +
  geom_segment(aes(x = 117.3, xend = 117.3, y = -20.76, yend = -20.1), 
               linetype = 2, alpha = 0.7, colour = "gray60", size = 1) +
  theme_minimal()
png(filename = "plots/exploratory-site-plot.png", height = 4, width = 10,
    res = 300, units = "in")
p1
dev.off()

# Bit more zoomed out for some more context
# build basic plot elements
p1.5 <- ggplot() +
  geom_tile(data = hill,aes(x = x, y = y, fill = lyr1), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = bathy, aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis() +
  geom_contour(data = bathy, aes(x = x, y = y, z = Depth), breaks = c( -30, -70, -200), 
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = damp_mp, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                "National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.4) +
  annotate(geom = "text", x = 117.1, y = c(-20.41, -19.75, -19.11), label = c("30m", "70m", "200m"),
           color = "white", size = 2) +
  coord_sf(xlim = c(116.74, 117.5167), ylim = c(-20.75, -19)) +
  labs(y = "Latitude", x = "Longitude") +
  theme_minimal()
png(filename = "plots/exploratory-site-plot-zoomed.png", height = 8.5, width = 6,
    res = 300, units = "in")
p1.5
dev.off()

# Bathy cross section
# Thorugh madeline shoals
cbathy <- as.data.frame(read.table(file = "data/spatial/rasters/large/tile2c.txt", 
                                   header = T, sep = ",")) %>%
  dplyr::filter(abs(X - 116.847) == min(abs(X - 116.847)),
                Y > -20.5424) %>% 
  glimpse()

bath_cross <- st_as_sf(x = cbathy, coords = c("X", "Y"), crs = wgscrs)

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif")
st_crs(aus) <- st_crs(aumpa)
aus <- st_transform(aus, wgscrs)
aus <- aus[aus$FEAT_CODE %in% "mainland", ]
aus <- st_union(aus)
plot(aus)
ausout <- st_cast(aus, "MULTILINESTRING")
plot(ausout)

bath_sf <- bath_cross %>%
  dplyr::mutate("distance.from.coast" = st_distance(bath_cross, ausout),
                x = unlist(map(bath_cross$geometry, 1)),
                y = unlist(map(bath_cross$geometry, 2)),
                land = lengths(st_intersects(bath_cross, aus)) > 0) %>%
  glimpse()

bath_df1 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "Z") %>%
  dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>%
  dplyr::mutate(distance.from.coast = ifelse(land %in% "FALSE", distance.from.coast*-1, distance.from.coast)) %>%
  dplyr::filter(depth > -250) %>%
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),
                    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"))

for (i in 1:nrow(paleo)) {
  temp <- bath_df1 %>%
    dplyr::filter(abs(bath_df1$depth - paleo$depth[i]) == min(abs(bath_df1$depth - paleo$depth[i]))) %>%
    dplyr::select(depth, distance.from.coast) %>%
    slice(1)
  
if (i == 1) {
  dat <- temp
} 
  else {
  dat <- bind_rows(dat, temp)
  }}

paleo$distance.from.coast <- dat$distance.from.coast

p2 <- ggplot() +
  geom_rect(aes(xmin = min(bath_df1$distance.from.coast), xmax = max(bath_df1$distance.from.coast), ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  annotate("text", x = -18, y = 60, label = "Legendre \nIsland", size = 3) +
  annotate("text", x = -25, y = 20, label = "Madeleine \nShoals", size = 3) +
  geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 20, 
                                 y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  geom_text(data = paleo, aes(x = distance.from.coast + 26, y = depth, label = label), size = 3) +
  annotate("segment", x = -21.69741, xend = -21.69741, y = -34, yend = 0, colour = "red", size = 1.2)
p2

# Through the npz
cbathy <- as.data.frame(read.table(file = "data/spatial/rasters/large/tile2c.txt", 
                                   header = T, sep = ",")) %>%
  dplyr::filter(abs(X - 117.08) == min(abs(X - 117.08))) %>%
  glimpse()

bath_cross <- st_as_sf(x = cbathy, coords = c("X", "Y"), crs = wgscrs)

bath_sf <- bath_cross %>%
  dplyr::mutate("distance.from.coast" = st_distance(bath_cross, ausout),
                x = unlist(map(bath_cross$geometry, 1)),
                y = unlist(map(bath_cross$geometry, 2)),
                land = lengths(st_intersects(bath_cross, aus)) > 0) %>%
  glimpse()

bath_df2 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "Z") %>%
  dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>%
  dplyr::mutate(distance.from.coast = ifelse(land %in% "FALSE", distance.from.coast*-1, distance.from.coast)) %>%
  dplyr::filter(depth > -250) %>%
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),
                    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"))

for (i in 1:nrow(paleo)) {
  temp <- bath_df2 %>%
    dplyr::filter(abs(bath_df2$depth - paleo$depth[i]) == min(abs(bath_df2$depth - paleo$depth[i]))) %>%
    dplyr::select(depth, distance.from.coast) %>%
    slice(1)
  
  if (i == 1) {
    dat <- temp
  } 
  else {
    dat <- bind_rows(dat, temp)
  }}

paleo$distance.from.coast <- dat$distance.from.coast

p3 <- ggplot() +
  geom_rect(aes(xmin = min(bath_df2$distance.from.coast), xmax = 5, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_df2, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df2, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = c(150, 0, -150, -300, -450, -600),expand = c(0,0), limits = c(-700, 240)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)")  +
  annotate("text", x = -20, y = 40, label = "Delambre \nIsland", size = 3) +
  geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 20, 
                                 y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  geom_text(data = paleo, aes(x = distance.from.coast + 26, y = depth, label = label), size = 3) +
  annotate("segment", x = -25.94324, xend = -25.94324, y = -31.5, yend = 0, colour = "red", size = 1.2) +
  annotate("segment", x = -26.4, xend = -26.4, y = -31.5, yend = 0, colour = "#7bbc63", size = 1.2) +
  annotate("segment", x = -35.1044, xend = -35.1044, y = -33, yend = 0, colour = "#7bbc63", size = 1.2) +
  annotate("text", x = -31, y = 10, label = "NPZ", size = 3) 
p3

# Through the lumpy bits
cbathy <- as.data.frame(read.table(file = "data/spatial/rasters/large/tile2c.txt", 
                                    header = T, sep = ",")) %>%
  dplyr::filter(abs(X - 117.3) == min(abs(X - 117.3))) %>%
  glimpse()

bath_cross <- st_as_sf(x = cbathy, coords = c("X", "Y"), crs = wgscrs)

bath_sf <- bath_cross %>%
  dplyr::mutate("distance.from.coast" = st_distance(bath_cross, ausout),
                x = unlist(map(bath_cross$geometry, 1)),
                y = unlist(map(bath_cross$geometry, 2)),
                land = lengths(st_intersects(bath_cross, aus)) > 0) %>%
  glimpse()

bath_df3 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "Z") %>%
  dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>%
  dplyr::mutate(distance.from.coast = ifelse(land %in% "FALSE", distance.from.coast*-1, distance.from.coast)) %>%
  dplyr::filter(depth > -250,
                distance.from.coast < 10) %>%
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),
                    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"))

for (i in 1:nrow(paleo)) {
  temp <- bath_df3 %>%
    dplyr::filter(abs(bath_df3$depth - paleo$depth[i]) == min(abs(bath_df3$depth - paleo$depth[i]))) %>%
    dplyr::select(depth, distance.from.coast) %>%
    slice(1)
  
  if (i == 1) {
    dat <- temp
  } 
  else {
    dat <- bind_rows(dat, temp)
  }}

paleo$distance.from.coast <- dat$distance.from.coast

p4 <- ggplot() +
  geom_rect(aes(xmin = min(bath_df3$distance.from.coast), xmax = 5, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_df3, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df3, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = c(150, 0, -150, -300, -450, -600),expand = c(0,0), limits = c(-700, 240)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 20, 
                                 y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  geom_text(data = paleo, aes(x = distance.from.coast + 26, y = depth, label = label), size = 3) +
  annotate("segment", x = -13.36314, xend = -13.36314, y = -12.5, yend = 0, colour = "red", size = 1.2)
p4

bath_plot <- p2 / p3 / p4 + plot_annotation(tag_levels = "a")
png(filename = "plots/bathy-cross-section.png", bath_plot, 
    res = 300, units = "in", height = 10, width = 8)
bath_plot
dev.off()

