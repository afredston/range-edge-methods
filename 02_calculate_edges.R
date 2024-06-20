library(here)
library(tidyverse)
#library(ggnewscale)

source(here("functions","calc_all_edges.R"))
load(here("data","fishdat.RData"))
# hauldat <- read_csv(here("data","haul_data.csv"), col_types = cols(haul_id = col_character())) 

fishdat <- dat 

focal_spp <- c('Urophycis tenuis')
edgetype <- "eq"

dat <- as_tibble(fishdat) %>% 
  filter(accepted_name==focal_spp) 

# rm(dat_zeros)

# read in all functions
# funs <- list.files("functions")
# sapply(funs, function(x)
#   source(file.path("functions", x)))

dat <- dat %>% 
  filter(year>1971)

edgetidy <- calc_all_edges(dat, edgetype, focal_spp)

write_csv(edgetidy, file=here("results",paste0(focal_spp, "_", edgetype, "_results.csv")))

# also note I'm not adding in flags for doing this with lots of species
# for example, no flag to drop years without enough data points to calculate these metrics
# since this is for illustration only 

# choosing a bird

lasorte <- read_csv(here("data","la_sorte_thompson_2007_supp.csv")) #https://doi.org/10.6084/m9.figshare.c.3299831.v1

# TABLE A1. A table summarizing the associations and findings for all 254 avian species considered in the analysis. For each species, the migratory status (Migratory) and the center of each species winter range is indicated (N/S). Migratory status was determined based on examinations of winter and breeding distribution maps. The location of the center of the range was determined based on the average latitude of CBC circles in 1975 using 36º north latitude as the demarcation. In addition, the number of CBC circles is indicated where each species was designated as common (COM), common and extirpated (EXT), common and colonized (COL), and the number of CBC circles where the species was present but not identified in any of the three common categories (NCOM). These values were estimated based on an analysis of temporal turnover dynamics (see the text for details on the analysis and description of categories). Finally, slope coefficients from individual linear models are presented for each species that estimate latitudinal trends for the northern boundary (NB), center of occurrence (CO), and center of abundance (CA) of geographic ranges (see text for description of linear models and range components).

spp_of_interest <- lasorte %>% 
  filter(`N/S`=="S", # focus on southerly species so they don't run out of the study region
         COM > 100, # and common species
         NB > 0 # with a northward shift in 2007
  )

# looked up some range maps
# let's focus on the black vulture--it definitively has a poleward edge in the US 

# download data from https://netapp.audubon.org/CBCObservation/Historical/ResultsBySpecies.aspx?1
# as of personal communication with CBC team in june 2024, the data portal has a bug that doesn't allow data download later than the 115th CBC (2015), so I downloaded 1975 (the start year of La Sorte and Jetz) - 2014



