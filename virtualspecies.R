# this script creates virtual species and runs SDMs for them 

########
# setup 
########

# packages
library(terra)
library(here)
library(elevatr) 
library(virtualspecies)
library(tidyverse)

# data
bird_mask <- vect(here("data","vulture_mask.shp"))
fish_mask <- rast(here("data","hake_mask.tif"))
load(here("data","birddat.Rdata"))
load(here("data","fishdat.Rdata")) 

fishdat <- fishdat |> filter(accepted_name == "Urophycis tenuis")

#########
# get predictor data 
#########

# each species gets two predictors, one static one and one dynamic one (temperature) 

# elevation for bird 
if(file.exists(here("data","elev_rast.tif")) == TRUE) {
  
  elev_rast <- rast(here("data","elev_rast.tif"))
  
}else if(file.exists(here("data","elev_rast.tif")) == FALSE) {
  elev_raw <- get_elev_raster(locations = data.frame(x = c(-130, -60), y = c(20, 55)), # bounding box from bird mask 
                      z  = 5, #finer resolution to temperature data; this is < 5000 m, which is 5 km, which is finer res than 1/12 degree https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
                      prj = sf::st_crs(bird_mask)) 
  elev_rast <- rast(elev_raw) # will not need this step in future versions of elevatr 
  writeRaster(elev_rast, filename=here("data","elev_rast.tif"))
}

elev <- crop(x = elev_rast, y = bird_mask, mask=TRUE)
plot(elev)
names(elev)
names(elev) <- "elev"

# temperature for bird 


# temperature for fish 
oisst <- rast(here("data","oisst.tif"))
hauldat <- fishdat |> 
  select(year, month, day, latitude, longitude, depth) |> 
  distinct()
#########
# get preference function parameters from real data 
#########
oisst_start <- min(time(oisst))
oisst_end <- max(time(oisst))
# 
# # test one layer 
# oisst_lyr <- subset(oisst, "sst_zlev=0_1") # doesn't work
# oisst_1 <- oisst[[1]]
# 
# oisst_dates <- data.frame(date = terra::time(oisst), 
#                           layer = terra::names(oisst)) # why is the same layer assigned to multiple dates? 

# fish * temperature 
sstprep <- fishdat |> 
  select(longitude, latitude, year, month, day) |> 
  mutate(date = as_date(paste0(year,"-",month,"-",day)), .keep = "unused") |> 
  filter(date >= oisst_start, date <= oisst_end)
  
lyr <- na.omit(terra::match(sstprep$date, time(oisst, "days")))
tmp <- terra::extract(oisst, sstprep[,longitude:latitude], layer=lyr, bind=TRUE)
fishsst <- terra::as.data.frame(tmp, geom="XY") # need to merge this with the dates, but the layer name isn't actually unique to each date -- there are fewer layers than dates -- why? 

bzz <- terra::extract(oisst, sstprep[,longitude:latitude], layer=1, bind=TRUE)

# bird * elevation 
bird_elev <- terra::extract(elev[[1]], vect(birddat, geom=c("longitude", "latitude")), ID = FALSE) # na.rm drops points outside the US that are not in the elevation layer so we don't actually need to mask the data anymore 

birddat_elev <- bind_cols(birddat, bird_elev) %>% 
  filter(!is.na(elev)) # drops rows outside of the elevation raster, which is the continental US, so we don't actually need to mask these data -- you can check this by running plot(birddat_elev$longitude, birddat_elev$latitude) 

floof <- birddat_elev %>% 
  mutate(elev_clean = ifelse(elev > 0, elev, 0.01), 
         num_cpue_clean = ifelse(num_cpue > 0, num_cpue, 0.00000001))

elev_lm <- lm(num_cpue ~ elev, data = birddat_elev)
elev_lm_p2 <- lm(num_cpue ~ poly(elev, 2), data = birddat_elev)
elev_lm_p3 <- lm(num_cpue ~ poly(elev, 3), data = birddat_elev)
elev_lm_p7 <- lm(num_cpue ~ poly(elev, 7), data = birddat_elev)
elev_lm_log <- lm(log1p(num_cpue) ~ elev, data = birddat_elev) # can't log-transform because of zeros, so doing log(x+1); see https://robjhyndman.com/hyndsight/transformations/
boop <- lm()
# which is the best model? 

# visualize model fits 
blankdat = data.frame(elev = seq(min(birddat_elev$elev), max(birddat_elev$elev), length.out = 100))
blankdat$pred = predict(elev_lm_p3, newdata = blankdat)
boopdat <- data.frame(elev = seq(min(birddat_elev$elev), max(birddat_elev$elev), 1), pred = 0.455 * exp(-0.028*elev))
plot(num_cpue ~ elev, data = birddat_elev)
abline()
with(blankdat, lines(x=elev, y=exp(pred), col="red"))
plot(log(num_cpue) ~ log(elev), data = birddat_elev)
boople <- lm(log(num_cpue_clean) ~ log(elev_clean), data = floof)
abline(lm(log(num_cpue_clean) ~ log(elev_clean), data = floof))
#########
# set up virtual species 
#########
sim_bird_param <- formatFunctions(
  elev = c(fun = "dnorm", mean = 100, sd = 1000) # prefers lowland habitat 
)

sim_bird <- generateSpFromFun(
  raster.stack = elev, 
  parameters = sim_bird_param, 
  plot = TRUE
)


library(ncdf4)
library(terra)
library(rerddap)
library(here)
library(sf)
library(rnaturalearth)
# get historical SST for white hake and black vulture 

if(file.exists(here("data","rerddap"))==FALSE){
  
  # NEED TO UPDATE THE BELOW TO ACTUALLY SAVE THE .NC FILES, FOR REPRODUCIBILITY 
  # # SST for NEUS shelf:
  # neus_latrange <- c(35, 45)
  # neus_lonrange <- c(-78, -66) 
  # 
  # oisst <- "ncdcOisst2Agg_LonPM180"
  # # split up into decades for easier downloading
  # oisst_time1 <- c("1982-01-01","1989-12-31") # same time interval for all regions
  # oisst_time2 <- c("1990-01-01","1999-12-31") 
  # oisst_time3 <- c("2000-01-01","2009-12-31") 
  # oisst_time4 <- c("2010-01-01","2019-12-31") 
  # 
  # oisst_fields <- "sst"
  # 
  # 
  # neus_oisst_grid1 <- griddap(oisst, time=oisst_time1, latitude = neus_latrange, longitude = neus_lonrange, fields=oisst_fields)
  # neus_oisst_grid2 <- griddap(oisst, time=oisst_time2, latitude = neus_latrange, longitude = neus_lonrange, fields=oisst_fields)
  # neus_oisst_grid3 <- griddap(oisst, time=oisst_time3, latitude = neus_latrange, longitude = neus_lonrange, fields=oisst_fields)
  # neus_oisst_grid4 <- griddap(oisst, time=oisst_time4, latitude = neus_latrange, longitude = neus_lonrange, fields=oisst_fields)
  # 
  # neus_oisst_nc_file1 <- neus_oisst_grid1$summary$filename
  # neus_oisst_nc_file2 <- neus_oisst_grid2$summary$filename
  # neus_oisst_nc_file3 <- neus_oisst_grid3$summary$filename
  # neus_oisst_nc_file4 <- neus_oisst_grid4$summary$filename
  
} else { 
  
  file_ls <- list.files(here("data","rerddap"), full.names = TRUE)
  
  oisst_stack <- rast(file_ls)
  
}

neus.bathy.mask <- vect(here("data","neus_bathy_mask.shp"))

neus.depth.cutoff <- -300

eezs <- st_read(here("data/World_EEZ_v10_20180221","eez_v10.shp")) # download from http://www.marineregions.org/downloads.php and move to data folder
useez <- eezs %>% 
  dplyr::filter(Sovereign1 == "United States") # need to get the new bathy masks in the same CRS

oisst_stack_crop1 <- mask(oisst_stack, as_Spatial(neus.bathy.mask))

