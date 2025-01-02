library(rerddap)
library(terra)

##########
# get environmental data for virtual fish 
##########

sea_lonrange <- c(-77, -66)
sea_latrange <- c(35, 45)

if(file.exists(here("data","oisst.tif"))==FALSE){
  oisst <- "ncdcOisst2Agg_LonPM180"
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
  
  oisst_nc_file1 <- oisst_grid1$summary$filename
  oisst_nc_file2 <- oisst_grid2$summary$filename
  oisst_nc_file3 <- oisst_grid3$summary$filename
  oisst_nc_file4 <- oisst_grid4$summary$filename
  
  oisst_rast <- rast(c(oisst_nc_file1, oisst_nc_file2, oisst_nc_file3, oisst_nc_file4))
  
  writeRaster(oisst_rast, filename=here("data","oisst.tif"))
  
  rm(oisst, oisst_fields, oisst_time1, oisst_time2, oisst_time3, oisst_time4, oisst_grid1, oisst_grid2, oisst_grid3, oisst_grid4, oisst_nc_file1, oisst_nc_file2, oisst_nc_file3, oisst_nc_file4, oisst_rast)
} else {
  oisst_rast <- rast(here("data","oisst.tif"))
}
