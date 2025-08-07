library(rangr)
library(terra)
library(here)

# step 1: get the K files (this is the harder part)
# start with a temperature map
# reclassify the values by where they sit relative to a species' thermal optimum
# then do the same with a projected temperature map 
# for both land and sea 
# let's do it! 

bird_mask <- vect(here("data","vulture_mask.shp"))
load(here("data","birddat.Rdata"))
ebird <- rast(here("data","blkvul_abundance_seasonal_year_round_mean_2023.tif"))

# baseline climate -- min temp -- for black vulture poleward edge sim
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
wc_tmin_files <- file.path(here("data/worldclim/wc2.1_30s_tmin"), paste0("wc2.1_30s_tmin_", months, ".tif"))
wc_tmin_rast_bymonth <- rast(wc_tmin_files)
# crop to extent of bird mask: 
crs(wc_tmin_rast_bymonth)
crs(bird_mask) # they're the same, we are good 
wc_tmin_rast_bymonth_crop <- crop(x = wc_tmin_rast_bymonth, y = bird_mask, mask=TRUE) # takes a few minutes
plot(wc_tmin_rast_bymonth_crop) 
wc_tmin_rast_bymonth_crop

wc_tmin_rast <- terra::mean(wc_tmin_rast_bymonth_crop, na.rm=TRUE) # get annual average of tmin over 12 months of data 
wc_tmin_rast # confirm that it has the same dimensions and extent as wc_tmin_rast_bymonth_crop, just fewer layers (12 --> 1) 
plot(wc_tmin_rast)

# let's do the same for the future layers 
# I'm sure there is a way to group_by and average within terra but I'm brute forcing this for now 
wc_tmin_2021_2040_file <- file.path(here("data/worldclim/wc2.1_30s_tmin_GFDL-ESM4_ssp370_2021-2040.tif")) 
wc_tmin_2041_2060_file <- file.path(here("data/worldclim/wc2.1_30s_tmin_GFDL-ESM4_ssp370_2041-2060.tif")) 
wc_tmin_2061_2080_file <- file.path(here("data/worldclim/wc2.1_30s_tmin_GFDL-ESM4_ssp370_2061-2080.tif")) 
wc_tmin_2081_2100_file <- file.path(here("data/worldclim/wc2.1_30s_tmin_GFDL-ESM4_ssp370_2081-2100.tif")) 
wc_tmin_2021_2040_bymonth <- rast(wc_tmin_2021_2040_file)
wc_tmin_2041_2060_bymonth <- rast(wc_tmin_2041_2060_file)
wc_tmin_2061_2080_bymonth <- rast(wc_tmin_2061_2080_file)
wc_tmin_2081_2100_bymonth <- rast(wc_tmin_2081_2100_file)

wc_tmin_2021_2040_bymonth # again has 12 layers, one for each month 
# as above, crop and then average for speed's sake 
crs(wc_tmin_2021_2040_bymonth) # same as bird, we are good 
wc_tmin_2021_2040_bymonth_crop <- crop(x = wc_tmin_2021_2040_bymonth, y = bird_mask, mask=TRUE) # crop to area of interest 
plot(wc_tmin_2021_2040_bymonth_crop)
wc_tmin_2021_2040 <- terra::mean(wc_tmin_2021_2040_bymonth_crop, na.rm=TRUE) # get average across 12 months of tmin data 
plot(wc_tmin_2021_2040) # looks good 


# repeat for other layers 
wc_tmin_2041_2060_bymonth_crop <- crop(x = wc_tmin_2041_2060_bymonth, y = bird_mask, mask=TRUE) 
wc_tmin_2041_2060 <- terra::mean(wc_tmin_2041_2060_bymonth_crop, na.rm=TRUE)
wc_tmin_2061_2080_bymonth_crop <- crop(x = wc_tmin_2061_2080_bymonth, y = bird_mask, mask=TRUE) 
wc_tmin_2061_2080 <- terra::mean(wc_tmin_2061_2080_bymonth_crop, na.rm=TRUE)
wc_tmin_2081_2100_bymonth_crop <- crop(x = wc_tmin_2081_2100_bymonth, y = bird_mask, mask=TRUE) 
wc_tmin_2081_2100 <- terra::mean(wc_tmin_2081_2100_bymonth_crop, na.rm=TRUE)


# now we need to translate temperature into K units. that's pretty challenging 
#  Buckley, N. J., B. M. Kluever, R. Driver, and S. A. Rush (2022). Black Vulture (Coragyps atratus), version 2.0. In Birds of the World (P. G. Rodewald and B. K. Keeney, Editors). Cornell Lab of Ornithology, Ithaca, NY, USA. https://doi.org/10.2173/bow.blkvul.02
# "Most of the area where Black Vulture occurs in winter has average minimum January temperature above –1°C"

# so let's set K to 0 if T < -1 C 
# our resolution is APPROXIMATELY 1000m x 1000m (1km square)
# it looks like the ebird "relative abundance" metric is per 3km squared?
# so let's divide it by 9 
# the max value on https://birdsoftheworld.org/bow/species/blkvul/2.0/distribution is 4.5 
# so it thinks the max carrying capacity is 0.5 per grid cell (assuming a grid cell is approx 1 km on a side)? hmmmm maybe our grid cells are TOO fine? 
# redo with 2.5 min 


wc_tmin_files <- file.path(here("data/worldclim/wc2.1_2.5m_tmin"), paste0("wc2.1_2.5m_tmin_", months, ".tif"))
wc_tmin_rast_bymonth <- rast(wc_tmin_files)
# crop to extent of bird mask: 
crs(wc_tmin_rast_bymonth)
crs(bird_mask) # they're the same, we are good 
wc_tmin_rast_bymonth_crop <- crop(x = wc_tmin_rast_bymonth, y = bird_mask, mask=TRUE) # takes a few minutes
plot(wc_tmin_rast_bymonth_crop) 
wc_tmin_rast_bymonth_crop

wc_tmin_rast <- terra::mean(wc_tmin_rast_bymonth_crop, na.rm=TRUE) # get annual average of tmin over 12 months of data 
wc_tmin_rast # confirm that it has the same dimensions and extent as wc_tmin_rast_bymonth_crop, just fewer layers (12 --> 1) 
plot(wc_tmin_rast)

# let's do the same for the future layers 
# I'm sure there is a way to group_by and average within terra but I'm brute forcing this for now 
wc_tmin_2021_2040_file <- file.path(here("data/worldclim/wc2.1_2.5m_tmin_GFDL-ESM4_ssp370_2021-2040.tif")) 
wc_tmin_2041_2060_file <- file.path(here("data/worldclim/wc2.1_2.5m_tmin_GFDL-ESM4_ssp370_2041-2060.tif")) 
wc_tmin_2061_2080_file <- file.path(here("data/worldclim/wc2.1_2.5m_tmin_GFDL-ESM4_ssp370_2061-2080.tif")) 
wc_tmin_2081_2100_file <- file.path(here("data/worldclim/wc2.1_2.5m_tmin_GFDL-ESM4_ssp370_2081-2100.tif")) 
wc_tmin_2021_2040_bymonth <- rast(wc_tmin_2021_2040_file)
wc_tmin_2041_2060_bymonth <- rast(wc_tmin_2041_2060_file)
wc_tmin_2061_2080_bymonth <- rast(wc_tmin_2061_2080_file)
wc_tmin_2081_2100_bymonth <- rast(wc_tmin_2081_2100_file)

wc_tmin_2021_2040_bymonth # again has 12 layers, one for each month 
# as above, crop and then average for speed's sake 
crs(wc_tmin_2021_2040_bymonth) # same as bird, we are good 
wc_tmin_2021_2040_bymonth_crop <- crop(x = wc_tmin_2021_2040_bymonth, y = bird_mask, mask=TRUE) # crop to area of interest 
plot(wc_tmin_2021_2040_bymonth_crop)
wc_tmin_2021_2040 <- terra::mean(wc_tmin_2021_2040_bymonth_crop, na.rm=TRUE) # get average across 12 months of tmin data 
plot(wc_tmin_2021_2040) # looks good 

# repeat for other layers 
wc_tmin_2041_2060_bymonth_crop <- crop(x = wc_tmin_2041_2060_bymonth, y = bird_mask, mask=TRUE) 
wc_tmin_2041_2060 <- terra::mean(wc_tmin_2041_2060_bymonth_crop, na.rm=TRUE)
wc_tmin_2061_2080_bymonth_crop <- crop(x = wc_tmin_2061_2080_bymonth, y = bird_mask, mask=TRUE) 
wc_tmin_2061_2080 <- terra::mean(wc_tmin_2061_2080_bymonth_crop, na.rm=TRUE)
wc_tmin_2081_2100_bymonth_crop <- crop(x = wc_tmin_2081_2100_bymonth, y = bird_mask, mask=TRUE) 
wc_tmin_2081_2100 <- terra::mean(wc_tmin_2081_2100_bymonth_crop, na.rm=TRUE)

# now the resolution is ROUGHLY 4.5 km x 4.5 km 
# working from 4.5 black vultures, max, in the 3 km x 3 km ebird map (see notes above), that gives us 10-11 black vultures in a grid cell at most here 
# now we can reclassify! 
# seems like they really like warm temperatures? what's the warmest winter temperature a black vulture was ever found in? 
get_max_bird_K <- function(x) {
  out <- pmax((11/26)*x + 11/26, 0) # slope of a line calculated to hit zero when temperature (x) is -1, and to reach ~11 when temperature is around 25 C, based on the eBird entry for black vulture 
  return(out)
}
wc_tmin_K_baseline <- terra::app(wc_tmin_rast, fun = get_max_bird_K)
plot(wc_tmin_K_baseline)
wc_tmin_K_2021_2040 <- terra::app(wc_tmin_2021_2040, fun = get_max_bird_K)
wc_tmin_K_2041_2060 <- terra::app(wc_tmin_2041_2060, fun = get_max_bird_K)
wc_tmin_K_2061_2080 <- terra::app(wc_tmin_2061_2080, fun = get_max_bird_K)
wc_tmin_K_2081_2100 <- terra::app(wc_tmin_2081_2100, fun = get_max_bird_K)

bird_K_rast <- c(wc_tmin_K_baseline, wc_tmin_K_2021_2040, wc_tmin_K_2041_2060, wc_tmin_K_2061_2080, wc_tmin_K_2081_2100)
names(bird_K_rast) <- c("baseline",
                      "2021_2040",
                      "2041_2060",
                      "2061_2080",
                      "2081_2100")

# we are ready to interpolate K for rangr 
# the K_get_interpolation function can be used to prepare K_map that changes over time. This may be useful, when simulating environmental change or exploring the effects of ecological disturbances. Either K_time_points or the time parameter is needed to perform interpolation. If the interpolation should be calculated between two carrying capacity maps, there is no need to pass the time points, because 1 will be set as the starting time point and time will be used as the ending point. On the other hand, in the absence of the time argument, the maximum element of K_time_points is considered to be the ending point for the interpolation.

K_interpolated_bird <- K_get_interpolation(
  bird_K_rast,
  K_time_points = c(1, 45, 65, 85, 105) # corresponding to 1985, 2030, 2050, 2070, 2090 (midpoints)
) # note this is 105 years of data which is a lot! takes a long time 

# now we need our initial map of N. let's use the point count data in birddat, convert it into a spatraster, and average it within grid cells 
pts <- vect(birddat_crop, geom = c("longitude", "latitude"), crs = crs(bird_K_rast))
template <- bird_K_rast[[1]]
bird_N1 <- rasterize(pts, template, field = "num_cpue", fun = "mean")  
plot(bird_N1) # very hard to see but they're in there! 
# but recall these are in the wrong units; we need to convert to birds per grid cell. 
# our max K is around 11
# the max value in birddat_crop is 33 (well there's one 67 value but it's a huge outlier. so let's go with 33)
# so 
bird_N1
bird_N1 <- bird_N1 * (11/33)
bird_N1 # perfect! 

sim_data_bird <- initialise(
  n1_map = bird_N1, # population sizes at t = 0
  K_map = K_interpolated_bird, # carrying capacities
  r = log(2), # intrinsic pop growth rate; leaving at example in rangr documentation for now
  rate = 1 / 1e3 # dispersal rate (parameter of kernel); leaving at example in rangr documentation for now 
)
# Error: n1_map and K_map have NA values in different grid cells 
# hmmmmm let's switch to the ebird abundance map, it's probably more continuous 
ebird_reproj <- project(ebird, crs(bird_mask))
ebird_N1_allpts <- crop(x = ebird_reproj, y = bird_mask, mask=TRUE)

# now need to get ebird onto the wc grid (hopefully this is right? leaned on chat here)
template_vec <- as.polygons(template, dissolve = FALSE, na.rm = FALSE) 
ebird_vals <- terra::extract(ebird_N1_allpts, template_vec, fun = mean, na.rm = TRUE)
template_vec$ebird_mean <- ebird_vals[,2] 
ebird_resampled <- rasterize(template_vec, template, field = "ebird_mean")
plot(ebird_resampled)

# OK and now, get this into the right units 
# ebird's resolution is 0.01628556 squared
# template's resolution is 0.04166667 squared
# so, to get between them, multiply by (0.04166667 / 0.01628556) ^2 
ebird_resampled <- ebird_resampled * 6.55
# and impute zeros where there are NAs 
values(ebird_resampled)[is.na(values(ebird_resampled))] <- 0 # overwrite NAs with 0 

# FINALLY let's put the NAs from K into the N raster so they match 
na_mask <- app(K_interpolated_bird, fun = function(x) any(is.na(x)))
ebird_N1 <- terra::ifel(na_mask, NA, ebird_resampled)
plot(ebird_N1)

sim_data_bird <- initialise(
  n1_map = ebird_N1, # population sizes at t = 0
  K_map = K_interpolated_bird, # carrying capacities
  r = log(2), # intrinsic pop growth rate; leaving at example in rangr documentation for now
  rate = 1 / 1e3 # dispersal rate (parameter of kernel); leaving at example in rangr documentation for now 
) # this will be really slow for a big model 

summary(sim_data_bird) # note this hasn't run a model yet, it's just holding the initial conditions 


sim_result_01 <- sim(obj = sim_data_bird, time = 100) # we can then run it for t time steps 
# note that any sim_result object can be transformed into a spat_raster for plotting etc:
# my_rast <- to_rast(
# sim_result_01,
# time_points = 1:sim_result_01$simulated_time,
# template = sim_data_01$K_map
# )

summary(sim_result_01) # gives a plot of mean abundance (y) vs time (x) 

save(sim_result_01, file=here("rangr_sim_result_01.Rdata"))

# there are way more functions to visualize the outputs

# generate visualisation
plot(sim_result_01,
     time_points = c(1, 10, 25, 50),
     template = sim_data_01$K_map
)
# other possible params:
# K_sd: numeric vector of length 1 with value ⁠>= 0⁠ (default 0); this parameter can be used if additional environmental stochasticity is required; if K_sd > 0, random numbers are generated from a log-normal distribution with the mean K_map and standard deviation K_sd
# r_sd: numeric vector of length 1 with value ⁠>= 0⁠ (default 0); if additional demographic stochasticity is required, r_sd > 0 is the standard deviation for a normal distribution around r (defined for each time step)
# growth: character vector of length 1; the name of a population growth function, either defined in growth or provided by the user (case-sensitive, default "gompertz", can also be "exponential" or "ricker")
# A: strength of Allee effect (see growth function)
# dens_dep: character vector of length 1 specifying if the probability of settling in a target grid cell is (case-sensitive, default "K2N"):   "none" - fully random, "K" - proportional to the carrying capacity of a target cell, "K2N" - density-dependent, i.e. proportional to the ratio of carrying capacity of a target cell to the number of individuals already present in a target cell
# border: character vector of length 1 defining how to deal with borders (case-sensitive, default "absorbing"): "reprising" - cells outside the study area are not allowed as targets for dispersal "absorbing" - individuals that disperse outside the study area are removed from the population
# kernel_fun: character vector of length 1; name of a random number generation function defining a dispersal kernel (case-sensitive, default "rexp")
# max_dist: numeric vector of length 1; maximum distance of dispersal to pre-calculate target cells. default is the 0.99 kernel of kernel_fun 
# calculate_dist: logical vector of length 1; determines if target cells will be precalculated. this speeds up the simulation 
# dlist: list; target cells at a specified distance calculated for every cell within the study area


