# download from FISHGLOB: https://github.com/AquaAuma/FishGlob_data/blob/main/outputs/Cleaned_data/NEUS_clean.RData
# too big to track on GH (added to gitignore)

# choosing a bird

# lasorte <- read_csv(here("data","la_sorte_thompson_2007_supp.csv")) #https://doi.org/10.6084/m9.figshare.c.3299831.v1

# TABLE A1. A table summarizing the associations and findings for all 254 avian species considered in the analysis. For each species, the migratory status (Migratory) and the center of each species winter range is indicated (N/S). Migratory status was determined based on examinations of winter and breeding distribution maps. The location of the center of the range was determined based on the average latitude of CBC circles in 1975 using 36º north latitude as the demarcation. In addition, the number of CBC circles is indicated where each species was designated as common (COM), common and extirpated (EXT), common and colonized (COL), and the number of CBC circles where the species was present but not identified in any of the three common categories (NCOM). These values were estimated based on an analysis of temporal turnover dynamics (see the text for details on the analysis and description of categories). Finally, slope coefficients from individual linear models are presented for each species that estimate latitudinal trends for the northern boundary (NB), center of occurrence (CO), and center of abundance (CA) of geographic ranges (see text for description of linear models and range components).

# spp_of_interest <- lasorte %>% 
#   filter(`N/S`=="S", # focus on southerly species so they don't run out of the study region
#          COM > 100, # and common species
#          NB > 0 # with a northward shift in 2007
#   )

# looked up some range maps
# let's focus on the black vulture--it definitively has a poleward edge in the US 
library(data.table)
library(tidyverse)
library(here)
library(sf)
library(terra)
bird_mask <- vect(here("data","vulture_mask.shp"))
fish_mask <- rast(here("data","hake_mask.tif"))

#########
# BIRD DATA
#########

# download data from https://netapp.audubon.org/CBCObservation/Historical/ResultsBySpecies.aspx?1
# the historical data download has some bugs so I used the "current year" tab and then went back and manually downloaded every count from 1975 to present for black vulture (as per pers comm w CBC)

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
fishdat_zeros <- expand.grid(haul_id = unique(fishdat$haul_id), accepted_name = unique(fishdat$accepted_name)) |> 
  left_join(fishdat_crop |> select(haul_id, accepted_name, num:wgt_cpua), by=c("accepted_name", "haul_id")) |> 
  mutate(
    across(everything(), ~replace_na(.x, 0)) # replace all NAs with zeros in all columns 
  ) |> # now add in the haul-level info that we want 
  left_join(fishdat |> select(haul_id, year, month, day, latitude, longitude, depth, sbt, sst) |> distinct(), by="haul_id")

save(fishdat_crop, file=here("data","fishdat.Rdata"))
save(fishdat_zeros, file=here("data","fishdat_with_zeros.Rdata"))
rm(list = ls())
