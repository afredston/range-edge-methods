# this script creates virtual species and runs SDMs for them 
# see: https://borisleroy.com/files/virtualspecies-tutorial.html 

# to re-run it in full you would need to download worldclim data:
# go to https://www.worldclim.org/data/worldclim21.html
# click on "tmin 5min" and download it
# put the contents of that download in a folder data/climate/wc2.1_5m in this project directory 

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

fishdat_crop <- fishdat_crop |> filter(accepted_name == "Urophycis tenuis")

#########
# bird 
#########

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

# temperature for bird; we're only using December since that's the month in which the bird count happens 
# months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
# tmin_past_files <- file.path(here("data/climate/wc2.1_5m"), paste0("wc2.1_5m_tmin_", months, ".tif"))
# wclim_tmin <- rast(file.path(here("data/climate/wc2.1_5m/wc2.1_5m_tmin_12.tif")))
# plot(wclim_tmin) 

# bird * elevation 
bird_elev <- terra::extract(elev, vect(birddat_crop, geom=c("longitude", "latitude")), ID = FALSE) # extract raster values at the lat/lon of the bird records. 

birddat_elev <- bind_cols(birddat_crop, bird_elev) %>% 
  filter(!is.na(elev)) # pair elevation with the bird abundance data 

# sanity check that this worked 
birddat_elev |> 
  ggplot() +
  geom_point(aes(x=longitude, y=latitude, color=elev, fill=elev))

quantile(birddat_elev$elev)

# bird * temperature
wclim_tavg_glob <- rast(file.path(here("data/climate/wc2.1_5m/wc2.1_5m_tavg_12.tif")))
plot(wclim_tavg_glob) 
# need to do some work to line this up with the elevation raster
wclim_mask = resample(wclim_tavg_glob, elev)
wclim_tavg = mask(wclim_mask, elev)
plot(wclim_tavg)
bird_tavg <- terra::extract(wclim_tavg, vect(birddat_crop, geom=c("longitude", "latitude")), ID = FALSE) # 
birddat_tavg <- bind_cols(birddat_crop, bird_tavg) |> 
  rename(tavg = wc2.1_5m_tavg_12) |> 
  filter(!is.na(tavg))

# now check if these variables actually have any relationship with presence/abundance 

plot(birddat_elev$elev, birddat_elev$num_cpue)
plot(birddat_tmin$tmin, birddat_tmin$num_cpue)
plot(birddat_tavg$tavg, birddat_tmin$num_cpue)

bird_tavg_lm_p2 <- lm(num_cpue ~ poly(tavg, 2, raw = TRUE), data = birddat_tavg)
blankdat = data.frame(tavg = seq(min(birddat_tavg$tavg), max(birddat_tavg$tavg), length.out = 100))
blankdat$pred = predict(bird_tavg_lm_p2, newdata = blankdat)
plot(num_cpue ~ tavg, data = birddat_tavg)
abline()
with(blankdat, lines(x=tavg, y=exp(pred), col="red"))

bird_elev_lm_p2 <- lm(num_cpue ~ poly(elev, 2, raw=TRUE), data = birddat_elev)
blankdat = data.frame(elev = seq(min(birddat_elev$elev), max(birddat_elev$elev), length.out = 100))
blankdat$pred = predict(bird_elev_lm_p2, newdata = blankdat)
plot(num_cpue ~ elev, data = birddat_elev)
abline()
with(blankdat, lines(x=elev, y=exp(pred), col="red"))

# if interested in log transforming see https://robjhyndman.com/hyndsight/transformations/

# turn these fitted lines into functions for virtualspecies

bird_elev_coef <- coef(bird_elev_lm_p2)
bird_elev_fn <- function(x) bird_elev_coef[1] + bird_elev_coef[2]*x + bird_elev_coef[3]*x^2
bird_tavg_coef <- coef(bird_tavg_lm_p2)
bird_tavg_fn <- function(x) bird_tavg_coef[1] + bird_tavg_coef[2]*x + bird_tavg_coef[3]*x^2

# pass these functions to virtualspecies 
generateSpFromFun(c(elev, wc2.1_5m_tavg_12),
                  parameters = formatFunctions(elev = c(fun = "bird_elev_fn"), wc2.1_5m_tavg_12 = c(fun = "bird_tavg_fn")), plot = TRUE)


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


