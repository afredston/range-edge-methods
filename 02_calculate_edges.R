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

# choosing a bird

lasorte <- read_csv(here("data","la_sorte_thompson_2007_supp.csv")) #https://doi.org/10.6084/m9.figshare.c.3299831.v1

# TABLE A1. A table summarizing the associations and findings for all 254 avian species considered in the analysis. For each species, the migratory status (Migratory) and the center of each species winter range is indicated (N/S). Migratory status was determined based on examinations of winter and breeding distribution maps. The location of the center of the range was determined based on the average latitude of CBC circles in 1975 using 36º north latitude as the demarcation. In addition, the number of CBC circles is indicated where each species was designated as common (COM), common and extirpated (EXT), common and colonized (COL), and the number of CBC circles where the species was present but not identified in any of the three common categories (NCOM). These values were estimated based on an analysis of temporal turnover dynamics (see the text for details on the analysis and description of categories). Finally, slope coefficients from individual linear models are presented for each species that estimate latitudinal trends for the northern boundary (NB), center of occurrence (CO), and center of abundance (CA) of geographic ranges (see text for description of linear models and range components).

# spp_of_interest <- lasorte %>% 
#   filter(`N/S`=="S", # focus on southerly species so they don't run out of the study region
#          COM > 100, # and common species
#          NB > 0 # with a northward shift in 2007
#   )

# looked up some range maps
# let's focus on the black vulture--it definitively has a poleward edge in the US 

focal_bird <- c('Coragyps atratus')
bird_edgetype <- "pol"

# download data from https://netapp.audubon.org/CBCObservation/Historical/ResultsBySpecies.aspx?1
# the historical data download has some bugs so I used the "current year" tab and then went back and manually downloaded every count from 1975 to present for black vulture (as per pers comm w CBC)

file_names <- dir(here("data","cbc_black_vulture_manual_download"), full.names=TRUE)

birddat <- read_csv(file_names, id = "name", skip=3) %>% # first three rows have no data except saying "black vulture"
  mutate(year = as.numeric(str_sub(sub(".csv","",name), start=-4)),
         num_cpue = Number_By_Party_Hours, 
         latitude = Latitude) %>% # extract year value from file name
  select(latitude, year, num_cpue)

birdedgetidy <- calc_all_edges(birddat, bird_edgetype, focal_bird)

write_csv(birdedgetidy, file=here("results",paste0(focal_bird, "_", bird_edgetype, "_results.csv")))
