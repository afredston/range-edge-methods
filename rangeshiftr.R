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
# but adapting it to an ocean example

# rangeshiftr decisions

# model parameters 
set_resolution = 10000 # 10 km2 on a side 
set_nhabitats = 4 
set_carrycap = c(10000, 5000, 2500, 0) # vector, must be the same length as the number of habitats, containing the number of individuals / hectare FOR EACH HABITAT TYPE. (or is it individuals per box, whatever size you set the box at, in the resolution parameter above? I've set it to 1 km2, not 1 ha (which would be 100 not 1000 above). I'm aiming for 10,000 fish / km2 as carrying capacity; need to double-check that's what's actually happening here.)
set_spdistresolution = 10000

# run specs
set_replicates = 1 
set_years = 20
set_outintpop = 0
set_outintocc = 0
set_outintrange = 0

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

land <- ImportedLandscape(LandscapeFile = "habitatmap.asc", 
                          Resolution = set_resolution, 
                          Nhabitats = set_nhabitats, 
                          K_or_DensDep = set_carrycap, 
                          SpDistFile = "speciesmap.asc", 
                          SpDistResolution = set_spdistresolution)
# ran without error on the first try, holy smokes! 

# should revisit later to add in overlapping generations and age structure 

demo <- Demography(Rmax = 1.5)

disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.1), 
                   Transfer = DispersalKernel(Distances = 10000), 
                   Settlement = Settlement() )


init <- Initialise(InitType = 1, # = initialisation from a loaded species distribution map
                   SpType = 0,   # = all suitable cells within all distribution presence cells
                   InitDens = 0) # = at carrying capacity



sim_0 <- Simulation(Simulation = 0, 
                    Replicates = set_replicates, 
                    Years = set_years,
                    OutIntPop = set_outintpop,
                    OutIntOcc = set_outintocc,
                    OutIntRange = set_outintrange)

s <- RSsim(land = land, demog = demo, dispersal = disp, simul = sim_0, init = init)

s 
validateRSparams(s)

RunRS(s, dirpath = here())
