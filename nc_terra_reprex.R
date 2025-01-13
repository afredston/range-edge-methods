library(terra)
library(rerddap)
latrange <- c(38, 40)
lonrange <- c(-37, -35) 

oisst <- "ncdcOisst2Agg_LonPM180"
oisst_time <- c("1982-01-02","1982-01-31")
oisst_fields <- "sst"
oisst_grid <- griddap(oisst, time=oisst_time, latitude = latrange, longitude = lonrange, fields=oisst_fields)
oisst_grid # inspect the griddap_nc object; has columns longitude, latitude, zlev, time, sst 
oisst_nc_file <- oisst_grid$summary$filename

oisst_rast <- rast(oisst_nc_file) 
oisst_rast # see how it creates weird layer names with "zlev" that are not unique to each lat, lon, and time combination? 

set.seed(10)
points <- data.frame(x = runif(5, -37, -35), 
                     y = runif(5, 38, 40), 
                     date = c("1982-01-05","1982-01-10","1982-01-15","1982-01-20","1982-01-25"))

lyr <- match(points$date, time(oisst_rast, "days"))
boop <- terra::extract(oisst_rast, points[,1:2], layer=lyr)
