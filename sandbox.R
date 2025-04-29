library(geodata)
library(here)
load(here("data","birddat.RData"))
load(here("data","fishdat.RData"))

sea_lonrange <- c(min(fishdat_crop$longitude)-0.5, max(fishdat_crop$longitude)+0.5)
sea_latrange <- c(min(fishdat_crop$latitude)-0.5, max(fishdat_crop$latitude)+0.5)

land_lonrange <- c(min(birddat_crop$longitude)-0.5, max(birddat_crop$longitude)+0.5)
land_latrange <- c(min(birddat_crop$latitude)-0.5, max(birddat_crop$latitude)+0.5)

boo <- rast(here("data/climate","wc2.1_30s_tmin_GFDL-ESM4_ssp370_2021-2040.tif"))
