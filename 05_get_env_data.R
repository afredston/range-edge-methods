library(rerddap)
library(terra)

##########
# create and write out environmental data for virtual species  
##########

# temperature for fish 

sea_lonrange <- c(-77, -66)
sea_latrange <- c(35, 45)

#if(file.exists(here("data","oisst.tif"))==FALSE){
oisst <- "ncdcOisst2Agg_LonPM180" # https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst2Agg_LonPM180.html 
oisst_fields <- "sst"
# split up into decades for easier downloading
oisst_time1 <- c("1982-01-01T12:00:00Z","1989-12-31") # same time interval for all regions
oisst_time2 <- c("1990-01-01","1999-12-31")
oisst_time3 <- c("2000-01-01","2009-12-31")
oisst_time4 <- c("2010-01-01","2019-12-31")

oisst_grid1 <- griddap(oisst, time=oisst_time1, latitude = sea_latrange, longitude = sea_lonrange, fields=oisst_fields)
oisst_grid2 <- griddap(oisst, time=oisst_time2, latitude = sea_latrange, longitude = sea_lonrange, fields=oisst_fields)
oisst_grid3 <- griddap(oisst, time=oisst_time3, latitude = sea_latrange, longitude = sea_lonrange, fields=oisst_fields)
oisst_grid4 <- griddap(oisst, time=oisst_time4, latitude = sea_latrange, longitude = sea_lonrange, fields=oisst_fields)

oisst_nc_dat1 <- oisst_grid1$data
oisst_nc_dat2 <- oisst_grid2$data
oisst_nc_dat3 <- oisst_grid3$data
oisst_nc_dat4 <- oisst_grid4$data

# NOTE: the order of columns here is important, and another dimension cannot be easily added without changing the rast() code below
tmp1 <- data.frame(longitude = oisst_nc_dat1$longitude, latitude = oisst_nc_dat1$latitude, time = oisst_nc_dat1$time, sst = oisst_nc_dat1$sst)
tmp2 <- data.frame(longitude = oisst_nc_dat2$longitude, latitude = oisst_nc_dat2$latitude, time = oisst_nc_dat2$time, sst = oisst_nc_dat2$sst)
tmp3 <- data.frame(longitude = oisst_nc_dat3$longitude, latitude = oisst_nc_dat3$latitude, time = oisst_nc_dat3$time, sst = oisst_nc_dat3$sst)
tmp4 <- data.frame(longitude = oisst_nc_dat4$longitude, latitude = oisst_nc_dat4$latitude, time = oisst_nc_dat4$time, sst = oisst_nc_dat4$sst)

# SLOW 
oisst_rast <- rast(tmp1, tmp2, tmp3, tmp4, type = "xylz", crs="+proj=longlat +datum=WGS84") # type = "xylz" tells rast() that the input dataframes have columns in this order: x, y, layer name, and value. that's why we carefully ordered the dfs above. but note that this call to rast() cannot handle another dimension or more complex dataframes, and rast() will take liberties and build incorrect rasters without extremely careful supervision! 

writeRaster(oisst_rast, filename=here("data","oisst.tif"))

rm(list=ls())
# }# else {
#  oisst_rast <- rast(here("data","oisst.tif"))
#}
