# this script creates a raster mask for subsetting global temperature or other data to the domain of white hake and black vulture, respectively 

#######
# LOAD PACKAGES
####### 
library(here)
library(oceanmap)
library(terra)
library(rnaturalearth)
#######
# white hake
####### 

# create bounding box for Northeast US shelf 
sea_lonrange <- c(-77, -66)
sea_latrange <- c(35, 45)
maxdepth = 300
mindepth = 15 

# get bathymetry data with "oceanmap" package 
if(file.exists(here("data","bbox_bathy.tif"))==FALSE){
  bathy <- get.bathy(lon = sea_lonrange, lat = sea_latrange, visualize = F, res = 15)
  writeRaster(bathy, filename=here("data","bbox_bathy.tif"))
} else {
  bathy <- rast(here("data","bbox_bathy.tif"))
}

# inspect map
# plot(bathy) # note that this goes very far out to sea, and also includes estuaries and lakes. let's get rid of those! 

# trim to a reasonable bathymetry range 
msk <- ifel(bathy > maxdepth | bathy < mindepth, NA, 1)
# plot(msk)
bathy_trim <- mask(bathy, msk)
# plot(bathy_trim) # better, but still goes beyond the US EEZ. let's fix that next

# trim to extent of US EEZ (takes a few seconds, it's a large-ish file)
worldeez <- vect(here("data/World_EEZ_v12_20231025","eez_v12.shp")) # downloaded from: Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632
useez <- subset(worldeez, worldeez$SOVEREIGN1=="United States")
useez <- project(useez, bathy_trim) # reproject for consistent CRS 
# plot(useez) # takes a minute but looks good 
bathy_trim_eez <- mask(bathy_trim, useez)
# plot(bathy_trim_eez)

writeRaster(bathy_trim_eez, filename=here("data","hake_mask.tif"))

#######
# black vulture
####### 

# this is easier, we just want a mask of continental North America 

nameroutline <- vect(ne_countries(country=c("united states of america","canada"))[1]) # creates a SpatVector object 
writeVector(nameroutline, filename = here("data","vulture_mask.shp"))

rm(list=ls())
