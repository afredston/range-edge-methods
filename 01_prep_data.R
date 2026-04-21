# download from FISHGLOB: https://github.com/AquaAuma/FishGlob_data/blob/main/outputs/Cleaned_data/NEUS_clean.RData
# too big to track on GH (added to gitignore)

# download bird data from https://netapp.audubon.org/CBCObservation/Historical/ResultsBySpecies.aspx?1
# the historical data download has some bugs so I used the "current year" tab and then went back and manually downloaded every count from 1975 to present for black vulture (as per pers comm w CBC)

# to regenerate the fish mask you'll need:
# worldeez <- vect(here("data/World_EEZ_v12_20231025","eez_v12.shp")) 
# downloaded from: Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632

library(data.table)
library(tidyverse)
library(here)
library(sf)
library(terra)
bird_mask_path <- here("data","vulture_mask.shp")
fish_mask_path <- here("data","hake_mask.tif")

# this script creates a raster mask for subsetting global temperature or other data to the domain of white hake and black vulture, respectively 

#######
# CREATE MASKS 
####### 

# these are used to drop data points outside a reasonable study region for each species 

if(!file.exists(fish_mask_path)) {
  library(oceanmap)
  
  # get bathymetry data with "oceanmap" package 
  if(file.exists(here("data","bbox_bathy.tif"))==FALSE){
    
    # create bounding box for Northeast US shelf 
    sea_lonrange <- c(-77, -66)
    sea_latrange <- c(35, 45)
    
    bathy <- get.bathy(lon = sea_lonrange, lat = sea_latrange, visualize = F, res = 15)
    writeRaster(bathy, filename=here("data","bbox_bathy.tif"))
  
  } else {
    bathy <- rast(here("data","bbox_bathy.tif"))
  }
  
  # inspect map
  # plot(bathy) # note that this goes very far out to sea, and also includes estuaries and lakes. let's get rid of those! 
  
  # trim to a reasonable bathymetry range 
  maxdepth = 300
  mindepth = 15 
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
} else {
  fish_mask <- rast(here("data","hake_mask.tif"))
}

if(!file.exists(bird_mask_path)) {
  
  library(rnaturalearth)
  
  # this is easier, we just want a mask of continental North America 
  
  usoutline <- vect(ne_countries(country="united states of america")[1]) # creates a SpatVector object 
  usmask <- rast()
  ext(usmask) <- c(-130, -60, 20, 55)
  usoutline_crop <- crop(usoutline, ext(usmask))
  #plot(usoutline_crop)
  writeVector(usoutline_crop, filename = here("data","vulture_mask.shp"))
  
  
} else {
  bird_mask <- vect(here("data","vulture_mask.shp"))
}

#########
# BIRD DATA
#########

file_names <- dir(here("data","cbc_black_vulture_manual_download"), full.names=TRUE)

birddat <- read_csv(file_names, id = "name", skip=3) %>% # first three rows have no data except saying "black vulture"
  mutate(year = as.numeric(str_sub(sub(".csv","",name), start=-4)),
         num_cpue = Number_By_Party_Hours, 
         latitude = Latitude,
         longitude = Longitude) %>% # extract year value from file name
  select(latitude, longitude, year, num_cpue)

birddat_sf <- st_as_sf(birddat, coords=c("longitude","latitude"), crs = 4326)

birddat_crop <- st_crop(birddat_sf, bird_mask) |> # crop to extent of bird mask 
  mutate(longitude = sf::st_coordinates(geometry)[,1],
         latitude = sf::st_coordinates(geometry)[,2]) |> 
  as.data.frame() |> 
  select(-geometry)  # un-spatialize after cropping 
  

save(birddat_crop, file=here("data","birddat.Rdata"))
#########
# FISH DATA
#########

# pull in data

load(here("data","NEUS_clean.Rdata"))
# contains two objects, 'data' and 'readme'
# be sure to read the readme to understand the column units and values 

data <- data.table(data) 

dat <- data[season == 'Fall']

# note that the standardized catch per unit effort / area columns are NA in this dataset

# this is because NEUS doesn't report these, and they are complicated to calculate; see https://github.com/AquaAuma/FishGlob_data/tree/main/metadata_docs

# we approach this as follows...

# STEP 1: get rid of hauls that are not close to the NOAA standard (complicating things, this was 30 min before 2009 and 20 min after)

neus_bad_hauls <- unique(dat[(year < 2009 & (haul_dur < 0.42 | haul_dur > 0.58)) | (year >= 2009 & (haul_dur < 0.25  | haul_dur > 0.42)),haul_id]) # pulls out haul IDs of bad hauls 

dat <- dat[!haul_id %in% neus_bad_hauls]

# STEP 2: calculate CPUE. as per NOAA staff, the reported biomass is calibrated to the pre-2009 30-minute tow duration (which is why we divide all wgt by 0.5 regardless of year). the average trawl swept area value is also provided by NOAA staff. 

fishdat <- dat[,wgt_cpue := wgt/0.5][,wgt_cpua := wgt/0.0384][,num_cpue := num/0.5][,num_cpua := num/0.0384]

fishdat_sf <- st_as_sf(fishdat, coords=c("longitude","latitude"), crs = 4326)

# note this is all the species! not filtering by species yet 
fishdat_crop <- st_crop(fishdat_sf, fish_mask) |> # crop to extent of fish mask. this changes very little because the survey is already on the shelf 
  mutate(longitude = sf::st_coordinates(geometry)[,1],
         latitude = sf::st_coordinates(geometry)[,2]) |> 
  as.data.frame() |> 
  select(-geometry)  # un-spatialize after cropping 

# add in zeros if needed. this file will be really big, so don't track it on gh 
# fishdat_zeros <- expand.grid(haul_id = unique(fishdat$haul_id), accepted_name = unique(fishdat$accepted_name)) |> 
#   left_join(fishdat_crop |> select(haul_id, accepted_name, num:wgt_cpua), by=c("accepted_name", "haul_id")) |> 
#   mutate(
#     across(everything(), ~replace_na(.x, 0)) # replace all NAs with zeros in all columns 
#   ) |> # now add in the haul-level info that we want 
#   left_join(fishdat |> select(haul_id, year, month, day, latitude, longitude, depth, sbt, sst) |> distinct(), by="haul_id")

save(fishdat_crop, file=here("data","fishdat.Rdata"))
#save(fishdat_zeros, file=here("data","fishdat_with_zeros.Rdata"))
rm(list = ls())
