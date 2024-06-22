library(here)
library(tidyverse)
#library(ggnewscale)

source(here("functions","calc_all_edges.R"))

##########
# FISH DATA
##########

load(here("data","fishdat.RData"))
# hauldat <- read_csv(here("data","haul_data.csv"), col_types = cols(haul_id = col_character())) 

focal_fish <- c('Urophycis tenuis')
fish_edgetype <- "eq"

fishdat <- as_tibble(dat) %>% 
  filter(accepted_name==focal_fish) 

# rm(dat_zeros)

# read in all functions
# funs <- list.files("functions")
# sapply(funs, function(x)
#   source(file.path("functions", x)))

fishdat <- fishdat %>% 
  filter(year>1971)

fishedgetidy <- calc_all_edges(fishdat, fish_edgetype, focal_fish)

write_csv(fishedgetidy, file=here("results",paste0(focal_fish, "_", fish_edgetype, "_results.csv")))

##########
# BIRD DATA
##########
load(here("data","birddat.RData"))

focal_bird <- c('Coragyps atratus')
bird_edgetype <- "pol"

birdedgetidy <- calc_all_edges(birddat, bird_edgetype, focal_bird)

write_csv(birdedgetidy, file=here("results",paste0(focal_bird, "_", bird_edgetype, "_results.csv")))
