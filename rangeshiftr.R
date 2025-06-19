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
rcl <- matrix(c(-Inf, 183, 1,   
                183, Inf, 2),  
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


writeRaster(bathy_hab, "test.asc", overwrite=T)

carrycap <- c(0, 0, 0, 5, 0, 0)
land <- ImportedLandscape(LandscapeFile = "UKmap_1km.txt", 
                          Resolution = 10, # 10 m^2 cells.  
                          Nhabitats = 6, 
                          K_or_DensDep = carrycap, 
                          SpDistFile = "Species_Distribution_10km.txt", 
                          SpDistResolution = 10000)


