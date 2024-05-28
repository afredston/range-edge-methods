library(here)

load(here("data","summer_flounder.RData"))

# read in all functions
funs <- list.files("functions")
sapply(funs, function(x)
  source(file.path("functions", x)))


# calculate annual edge with different methods 

# latitudinal distal

# latitudinal quantile of presence

# latitudinal quantile of abundance

# 2D contour distal

# 2D contour quantile of presence

# 2D contour quantile of abundance

# SDM outputs 
