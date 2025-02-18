###
# Project: OMP Ngarluma
# Data:    LiDAR Bathymetry data
# Task:    Export spatially balanced designs to cplot format
# Author:  Claude Spencer
# Date:    September 2023
##

rm(list = ls())
gc()

library(tidyverse)
library(gpx)
library(sf)
library(nngeo)
library(GlobalArchive)
# source('R/01_MBH-design/functions.R')

boss <- read.csv("output/sampling-design/boss_sampling-design.csv") %>%
  glimpse()

bruv <- read.csv("output/sampling-design/bruv_sampling-design.csv") %>%
  glimpse()

# BRUVs
cplot.bruv <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             lon = measurements::conv_unit(bruv$lon_WGS84, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             lat = measurements::conv_unit(bruv$lat_WGS84, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             "symbol" = c("Green Star"),
                             "ptcode" = bruv$siteID,
                             c(0)) %>%
  separate(lon, into = c("lon.hour", "lon.dm"), sep = " ") %>%
  separate(lat, into = c("lat.hour", "lat.dm"), sep = " ") %>%
  separate(lon.dm, into = c("lon.min", "lon.dec"), sep = "\\.") %>%
  separate(lat.dm, into = c("lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.min = str_pad(.$lon.min, side = "left", pad = "0", width = 2),
                lat.dec = str_trunc(.$lat.dec, width = 4,  side = "right", ellipsis = ""),
                lat.min = str_pad(.$lat.min, side = "left", pad = "0", width = 2)) %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = "")) %>%
  dplyr::mutate(lon.dec = str_pad(.$lon.dec, side = "right", pad = "0", width = 4),
                lon = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), "E"),
                lat = sub('.', '',paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), "S"))) %>%
  dplyr::select(mark, PXYCSLM, lon, lat, symbol, ptcode, c.0.) %>%
  glimpse()

bruv.head <- data.frame(mark = c('"TMQ CPlot Chart Type 2   ",',
                                 '"Description: Dampier      ",',
                                 paste0('"Bounds: "', paste(min(cplot.bruv$lat), min(cplot.bruv$lon),
                                                            max(cplot.bruv$lat),max(cplot.bruv$lon), sep = ","),
                                        '      ",'),
                                 '"Format: NM3      ",',
                                 '"Rev. Date: 110819/160554     ",',
                                 '"Scale: 1:00      ",',
                                 '"Mag. Variation: 0     ",',
                                 '"Cautions:       ",',
                                 '"  "  ,',
                                 '"<EOH>       "',
                                 ''))

cplot.bruv.head <- bind_rows(bruv.head, cplot.bruv)

write.table(cplot.bruv.head , "output/sampling-design/bruv_sampling-design_CPLOT.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE, na = "")

paste(min(cplot.bruv$lat), min(cplot.bruv$lon),
      max(cplot.bruv$lat),max(cplot.bruv$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 

# BOSS
cplot.boss <- data.frame("mark" = c("mark"),
                         "PXYCSLM" = c("PXYCSLM"),
                         lon = measurements::conv_unit(boss$lon_WGS84, 
                                                       from = "dec_deg", 
                                                       to = "deg_dec_min"),
                         lat = measurements::conv_unit(boss$lat_WGS84, 
                                                       from = "dec_deg", 
                                                       to = "deg_dec_min"),
                         "symbol" = c("Blue Star"),
                         "ptcode" = boss$siteID,
                         c(0)) %>%
  separate(lon, into = c("lon.hour", "lon.dm"), sep = " ") %>%
  separate(lat, into = c("lat.hour", "lat.dm"), sep = " ") %>%
  separate(lon.dm, into = c("lon.min", "lon.dec"), sep = "\\.") %>%
  separate(lat.dm, into = c("lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.min = str_pad(.$lon.min, side = "left", pad = "0", width = 2),
                lat.dec = str_trunc(.$lat.dec, width = 4,  side = "right", ellipsis = ""),
                lat.min = str_pad(.$lat.min, side = "left", pad = "0", width = 2)) %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = "")) %>%
  dplyr::mutate(lon.dec = str_pad(.$lon.dec, side = "right", pad = "0", width = 4),
                lon = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), "E"),
                lat = sub('.', '',paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), "S"))) %>%
  dplyr::select(mark, PXYCSLM, lon, lat, symbol, ptcode, c.0.) %>%
  glimpse()

boss.head <- data.frame(mark = c('"TMQ CPlot Chart Type 2   ",',
                            '"Description: Dampier      ",',
                            paste0('"Bounds: "', paste(min(cplot.boss$lat), min(cplot.boss$lon),
                                                     max(cplot.boss$lat),max(cplot.boss$lon), sep = ","),
                                   '      ",'),
                            # '"Bounds: 20.18.0161S,116.53.8166E,20.27.2458S,117.14.7872E      ",',
                            '"Format: NM3      ",',
                            '"Rev. Date: 110819/160554     ",',
                            '"Scale: 1:00      ",',
                            '"Mag. Variation: 0     ",',
                            '"Cautions:       ",',
                            '"  "  ,',
                            '"<EOH>       "',
                            ''))

cplot.boss.head <- bind_rows(boss.head, cplot.boss)

write.table(cplot.boss.head , "output/sampling-design/boss_sampling-design_CPLOT.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE, na = "")

# then run this to duplicate each file and convert the copy to .MRK file
txts <- list.files("output/sampling-design/", "*.txt", full.names = T)
for(filei in txts){
  file.copy(filei, paste(gsub(".txt", ".MRK", filei)))
}

# Old header to paste in to top of file - should hopefully be automated now

# "TMQ CPlot Chart Type 2   ",
# "Description: Ningaloo      ",
# "Bounds: 21.30.0000S,113.20.0000E,22.55.0000S,114.20.0000E      ",
# "Format: NM3      ",
# "Rev. Date: 110819/160554     ",
# "Scale: 1:00      ",
# "Mag. Variation: 0     ",
# "Cautions:       ",
# "  "  ,
# "<EOH>       "