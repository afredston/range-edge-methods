library(RangeShiftR)
library(terra)
library(RColorBrewer)
library(viridis)
library(grid)
library(gridExtra)
library(here)
library(tidyverse)
select <- dplyr::select
# following along with https://rangeshifter.github.io/RangeshiftR-tutorials/tutorial_1.html
# but adapting it to an ocean example (especially given that the tutorial is not reproducible since there are no source files!)

load(here("data","fishdat.Rdata")) 

# get bathymetry data with "oceanmap" package 
if(file.exists(here("data","bbox_bathy.tif"))==FALSE){
  bathy <- get.bathy(lon = sea_lonrange, lat = sea_latrange, visualize = F, res = 4)
  writeRaster(bathy, filename=here("data","bbox_bathy.tif"))
} else {
  bathy <- rast(here("data","bbox_bathy.tif"))
}
bathy[bathy >= 1000] <- NA # this stands in for "UKmap" in the tutorial 
# for the sake of following the rangeshiftr tutorial, let's discretize this into shelf (<200 m) depth 
rcl <- matrix(c(-Inf, 49.999, 1, 
                50, 91.499, 2,   
                91.5, 183, 3,
                183.001, Inf, 4),  
              ncol = 3, byrow = TRUE)

# Apply the reclassification
bathy_hab <- classify(bathy, rcl)
plot(bathy_hab)
# get starting species distribution 
load(here("data","fishdat.Rdata")) 
spdist_df <- fishdat_crop |> 
  filter(num_cpue > 0, accepted_name=="Paralichthys dentatus", year <= 1990) |> # note the year filter here 
  select(longitude, latitude) |> 
  distinct()

spdist_vect <- vect(spdist_df, geom = c("longitude", "latitude"), crs = "EPSG:4326")  # geographic coordinates

# reproject to a metric CRS to make 10km x 10km boxes around points 
v_proj <- project(spdist_vect, "EPSG:3857")  

# create a template raster with 10km × 10km resolution
r_template <- rast(v_proj, res = 10000)  # 10,000 meters = 10 km

# 4. Rasterize: you must have a field to use for values
# If no value column, just assign 1 to every point
if (!"value" %in% names(spdist_df)) {
  v_proj$value <- 1
}

r <- rasterize(v_proj, r_template, field = "value", fun = "mean") 

# project back to lat/lon
spdist_rast <- project(r, "EPSG:4326")
plot(spdist_rast)

# overlay species distribution onto the bathymetric map 
plot(bathy_hab)
plot(as.polygons(spdist_rast, dissolve=F), add=T, col="white")
# looks good!

# now, rangeshiftr requires that maps be in a .txt format for ArcGIS, presumably for consistency with their windows GUI
# let's write these out as text files and check that they look OK 

writeRaster(bathy_hab, "habitatmap.asc", overwrite=T)
writeRaster(spdist_rast, "speciesmap.asc", overwrite=T)

carrycap <- c(10000, 5000, 2500, 0) # I'm very confused about the units here. in the tutorial it says they are in individuals per hectare. but then the tutorial sets Resolution = 1000, which creates boxes that are 1000 m on a side, which is 1000000 m2 or 100 hectares (according to the function documentation, Resolution is "cell size in meters defaults to 100"). I'm using individuals per km2 which is the same units as num_cpue in the fish data file. so let's say the carrying capacity for good habitat is 10,000 individuals. 
land <- ImportedLandscape(LandscapeFile = "habitatmap.asc", 
                          Resolution = 1000, # 100 m on a side = 1 ha grid cell = 0.01 km2; 1000m on a side = 1 km2 
                          Nhabitats = 4, 
                          K_or_DensDep = carrycap, 
                          SpDistFile = "speciesmap.asc", 
                          SpDistResolution = 10000)
# ran without error on the first try, holy smokes! 

# should revisit later to add in overlapping generations and age structure 

demo <- Demography(Rmax = 1.5)

disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.1), 
                   Transfer = DispersalKernel(Distances = 2000), 
                   Settlement = Settlement() )


init <- Initialise(InitType = 1, # = initialisation from a loaded species distribution map
                   SpType = 0,   # = all suitable cells within all distribution presence cells
                   InitDens = 0) # = at carrying capacity



sim_0 <- Simulation(Simulation = 0, 
                    Replicates = 20, 
                    Years = 100,
                    OutIntPop = 5,
                    OutIntOcc = 5,
                    OutIntRange = 5)

s <- RSsim(land = land, demog = demo, dispersal = disp, simul = sim_0, init = init)

s 
validateRSparams(s)

RunRS(s, dirpath = here())
