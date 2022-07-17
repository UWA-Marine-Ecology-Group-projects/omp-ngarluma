
###
# Project: Parks - Ningaloo Post-Survey
# Task:    Utility functions
# author:  Kingsley Griffin
##

# Function to create rectangular site box from coordinates based on center x and y coordinates and heading angle
sf_newstrip <- function(centerxy, xdim, ydim, heading, siteID, projcrs){
  require(geosphere)
  wgscrs <- CRS("+proj=longlat +datum=WGS84")
  centersp <- SpatialPoints(coords = data.frame(centerxy[1], centerxy[2]), 
                            proj4string = projcrs)
  centerll   <- spTransform(centersp, wgscrs)
  firstbear  <- ifelse((heading - 90) < 0, heading + 270, heading - 90) 
  secondbear <- ifelse((heading + 90) > 360, heading - 270, heading + 90) 
  thirdbear  <- ifelse((secondbear + 90) > 360, (secondbear + 90) - 360, 
                       secondbear + 90) 
  midl <- destPoint(p = centerll, b = firstbear, d = xdim/2)
  topl <- destPoint(midl, heading, ydim/2)
  topr <- destPoint(topl, secondbear, xdim)
  btmr <- destPoint(topr, thirdbear, ydim)
  btml <- destPoint(btmr, firstbear, xdim)
  
  coords <- rbind(topl, topr, btmr, btml, topl)
  pstring <- SpatialPoints(coords, proj4string = wgscrs)
  pstring <- spTransform(pstring, projcrs)
  pstringdf <- as.data.frame(coordinates(pstring))
  colnames(pstringdf) <- c("easting", "northing")
  sitepoly <- st_polygon(list(coordinates(pstring)))
  return(
    sitepoly
  )
}

# Function to create rectangular site box from coordinates based on center x and y coordinates and heading angle
newstrip <- function(centerxy, xdim, ydim, heading, siteID, projcrs){
  require(geosphere)
  wgscrs <- CRS("+proj=longlat +datum=WGS84")
  centersp <- SpatialPoints(coords = data.frame(centerxy[1], centerxy[2]), 
                            proj4string = projcrs)
  centerll   <- spTransform(centersp, wgscrs)
  firstbear  <- ifelse((heading - 90) < 0, heading + 270, heading - 90) 
  secondbear <- ifelse((heading + 90) > 360, heading - 270, heading + 90) 
  thirdbear  <- ifelse((secondbear + 90) > 360, (secondbear + 90) - 360, 
                       secondbear + 90) 
  midl <- destPoint(p = centerll, b = firstbear, d = xdim/2)
  topl <- destPoint(midl, heading, ydim/2)
  topr <- destPoint(topl, secondbear, xdim)
  btmr <- destPoint(topr, thirdbear, ydim)
  btml <- destPoint(btmr, firstbear, xdim)
  
  coords <- rbind(topl, topr, btmr, btml, topl)
  pstring <- SpatialPoints(coords, proj4string = wgscrs)
  pstring <- spTransform(pstring, projcrs)
  sitepoly   <- Polygon(coords = as.matrix(coordinates(pstring)))
  sitepolys  <- Polygons(list(sitepoly), c(siteID))
  sitepolys  <- SpatialPolygons(list(sitepolys), proj4string = projcrs)
  return(
    sitepolys
  )
}

# Create a blank raster of defined extent, crs and resolution
Blank.Raster <- function(extent, crsobj, resolution){
  require(raster)
  rast         <- raster( )
  crs(rast)    <- crsobj
  extent(rast) <- extent(extent)
  res(rast)    <- resolution
  return(rast)
}


# Convert decimal degrees to Lat/Long CPLOT format
DDtolatlon <- function(coordxy){
  require(measurements)
  coordxy[ , 1] <- measurements::conv_unit(coordxy[, 1], 
                                           from = "dec_deg", 
                                           to = "deg_dec_min")
  coordxy[ , 2] <- coordxy[ , 2] * -1
  coordxy[ , 2] <- measurements::conv_unit(coordxy[, 2], 
                                           from = "dec_deg", 
                                           to = "deg_dec_min")
  coordxy[ , 1] <- gsub(' ', '.', coordxy[ , 1])
  coordxy[ , 2] <- gsub(' ', '.', coordxy[ , 2])
  coordxy[ , 1] <- substr(coordxy[ , 1], start = 1, stop = 11)
  coordxy[ , 1] <- paste0(coordxy[ , 1], "E", sep = "")
  coordxy[ , 2] <- substr(coordxy[ , 2], start = 1, stop = 10)
  coordxy[ , 2] <- paste0(coordxy[ , 2], "S", sep = "")
  return(coordxy)
  }
