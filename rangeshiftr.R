###########
# LOAD PACKAGES
###########

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

###########
# RANGESHIFTR USER DECISIONS 
###########

# relative path from working directory:
dirpath = "rangeshiftr_dir/"
# rangeshiftr WILL NOT RUN unless you give it its own subdirectory within your working directory that in turn contains the folders below. 
# see https://github.com/RangeShifter/RangeshiftR-tutorials/discussions/30

dir.create(file.path(dirpath, "Inputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(dirpath, "Outputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(dirpath, "Output_Maps"), recursive = TRUE, showWarnings = FALSE)
crs_m <- "EPSG:3857"

# model parameters 
set_resolution = 10000 # 10 km2 on a side 
set_nhabitats = 4 
#set_carrycap = c(10000, 5000, 2500, 0) # vector, must be the same length as the number of habitats, containing the number of individuals / hectare FOR EACH HABITAT TYPE. (or is it individuals per box, whatever size you set the box at, in the resolution parameter above? I've set it to 1 km2, not 1 ha (which would be 100 not 1000 above). I'm aiming for 10,000 fish / km2 as carrying capacity; need to double-check that's what's actually happening here.)
set_carrycap = c(100, 50, 25, 0)# temporarily reduced this to get it to run faster 
set_spdistresolution = 10000
set_distances = 10000 # kernel distance in m

# run specs
set_replicates = 1 
set_years = 20
set_outintpop = 0
set_outintocc = 0
set_outintrange = 0

###########
# GET SPECIES DISTRIBUTION MAP
###########

load(here("data","fishdat.Rdata")) 
spdist_df <- fishdat_crop |> 
  filter(num_cpue > 0, accepted_name=="Paralichthys dentatus", year <= 1990) |> # note the year filter here 
  select(longitude, latitude) |> 
  distinct()

spdist_vect <- vect(spdist_df, geom = c("longitude", "latitude"), crs = "EPSG:4326")  # geographic coordinates

###########
# GET HABITAT MAP
###########

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

###########
# ALIGN MAP GRIDS
###########

# code adapted from chat to align the cell size exactly to set_resolution 
# we start by creating a template file for the grid 
# code below snaps the extent to multiples of set_resolution so cellsize is exactly set_resolution

bathy_reproj <- project(bathy_hab, crs_m)
e <- ext(bathy_reproj) 
xmin <- floor(xmin(e) / set_resolution) * set_resolution
xmax <- ceiling(xmax(e) / set_resolution) * set_resolution
ymin <- floor(ymin(e) / set_resolution) * set_resolution
ymax <- ceiling(ymax(e) / set_resolution) * set_resolution
tmpl <- rast(ext(xmin, xmax, ymin, ymax), res = set_resolution, crs = crs_m)

# now put the habitat (bathymetry) data on this grid 
hab_m <- resample(bathy_reproj, tmpl, method = "near")
hab_m <- as.int(hab_m)
hab_m <- ifel(is.na(hab_m), -999L, hab_m) # put -999 for NA which I saw in the rangeshiftr documentation https://github.com/RangeShifter/RangeshiftR-tutorials/discussions/37

# now put the species data on this grid
spdist_reproj <- project(spdist_vect, crs_m)
spdist_reproj$value <- 1L
sp_m <- rasterize(spdist_reproj, tmpl, field = "value", fun = "max", background = NA) # fun="max" makes a cell present if any point falls in it
sp_m <- as.int(sp_m)
sp_m <- ifel(is.na(sp_m), -999L, sp_m)

###########
# CHECK FILES AND WRITE OUT
###########
plot(hab_m)
plot(as.polygons(sp_m, dissolve = TRUE), add = TRUE, border = "white")

hab_hdr <- readLines(file.path(dirpath,"Inputs","habitatmap.asc"), n = 7)
sp_hdr  <- readLines(file.path(dirpath,"Inputs","speciesmap.asc"), n = 7)
hab_hdr
sp_hdr
# visually inspect these headers and ensure they match exactly 

writeRaster(
  hab_m,
  filename = file.path(dirpath, "Inputs", "habitatmap.asc"),
  overwrite = TRUE,
  NAflag = -999,
  datatype = "INT4S"
)

writeRaster(
  sp_m,
  filename = file.path(dirpath, "Inputs", "speciesmap.asc"),
  overwrite = TRUE,
  NAflag = -999,
  datatype = "INT4S"
)

###########
# CREATE RANGESHIFTR OBJECTS AND RUN
###########

land <- ImportedLandscape(LandscapeFile = "habitatmap.asc", # rangeshiftr will automatically look for this in dirpath/Inputs  
                          Resolution = set_resolution, 
                          Nhabitats = set_nhabitats, 
                          K_or_DensDep = set_carrycap, 
                          SpDistFile = "speciesmap.asc", 
                          SpDistResolution = set_spdistresolution)
# ran without error on the first try, holy smokes! 

# should revisit later to add in overlapping generations and age structure 

demo <- Demography(Rmax = 1.5)

disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.1), 
                   Transfer = DispersalKernel(Distances = set_distances), 
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

t0 <- Sys.time()
RunRS(s, dirpath = dirpath)
Sys.time() - t0
