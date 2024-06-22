# download from FISHGLOB: https://github.com/AquaAuma/FishGlob_data/blob/main/outputs/Cleaned_data/NEUS_clean.RData
# too big to track on GH (added to gitignore)

# choosing a bird

# lasorte <- read_csv(here("data","la_sorte_thompson_2007_supp.csv")) #https://doi.org/10.6084/m9.figshare.c.3299831.v1

# TABLE A1. A table summarizing the associations and findings for all 254 avian species considered in the analysis. For each species, the migratory status (Migratory) and the center of each species winter range is indicated (N/S). Migratory status was determined based on examinations of winter and breeding distribution maps. The location of the center of the range was determined based on the average latitude of CBC circles in 1975 using 36º north latitude as the demarcation. In addition, the number of CBC circles is indicated where each species was designated as common (COM), common and extirpated (EXT), common and colonized (COL), and the number of CBC circles where the species was present but not identified in any of the three common categories (NCOM). These values were estimated based on an analysis of temporal turnover dynamics (see the text for details on the analysis and description of categories). Finally, slope coefficients from individual linear models are presented for each species that estimate latitudinal trends for the northern boundary (NB), center of occurrence (CO), and center of abundance (CA) of geographic ranges (see text for description of linear models and range components).

# spp_of_interest <- lasorte %>% 
#   filter(`N/S`=="S", # focus on southerly species so they don't run out of the study region
#          COM > 100, # and common species
#          NB > 0 # with a northward shift in 2007
#   )

# looked up some range maps
# let's focus on the black vulture--it definitively has a poleward edge in the US 

library(data.table)
library(tidyverse)
library(here)

#########
# BIRD DATA
#########

# download data from https://netapp.audubon.org/CBCObservation/Historical/ResultsBySpecies.aspx?1
# the historical data download has some bugs so I used the "current year" tab and then went back and manually downloaded every count from 1975 to present for black vulture (as per pers comm w CBC)

file_names <- dir(here("data","cbc_black_vulture_manual_download"), full.names=TRUE)

birddat <- read_csv(file_names, id = "name", skip=3) %>% # first three rows have no data except saying "black vulture"
  mutate(year = as.numeric(str_sub(sub(".csv","",name), start=-4)),
         num_cpue = Number_By_Party_Hours, 
         latitude = Latitude) %>% # extract year value from file name
  select(latitude, year, num_cpue)

save(birddat, file=here("data","birddat.Rdata"))
#########
# FISH DATA
#########

# pull in data

load(here("data","NEUS_clean.Rdata"))
# contains two objects, 'data' and 'readme'
# be sure to read the readme to understand the column units and values 

data <- data.table(data) 

dat <- data[season == 'Fall']

# note that the standardized catch per unit effort / area columns are NA in this dataset

# this is because NEUS doesn't report these, and they are complicated to calculate; see https://github.com/AquaAuma/FishGlob_data/tree/main/metadata_docs

# we approach this as follows...

# STEP 1: get rid of hauls that are not close to the NOAA standard (complicating things, this was 30 min before 2009 and 20 min after)

neus_bad_hauls <- unique(dat[(year < 2009 & (haul_dur < 0.42 | haul_dur > 0.58)) | (year >= 2009 & (haul_dur < 0.25  | haul_dur > 0.42)),haul_id]) # pulls out haul IDs of bad hauls 

dat <- dat[!haul_id %in% neus_bad_hauls]

# STEP 2: calculate CPUE. as per NOAA staff, the reported biomass is calibrated to the pre-2009 30-minute tow duration (which is why we divide all wgt by 0.5 regardless of year). the average trawl swept area value is also provided by NOAA staff. 

dat <- dat[,wgt_cpue := wgt/0.5][,wgt_cpua := wgt/0.0384][,num_cpue := num/0.5][,num_cpua := num/0.0384]

# now stopping here because the analysis doesn't end up needing zeros 

save(dat, file=here("data","fishdat.Rdata"))

# you can now use any of these indices of biomass -- kg / hr or kg / km^2, or num / hr or num / km^2 -- for EDM

# STEP 3: impute zeros for hauls where species were not found 

# first split out most of the haul information so the resulting object isn't too huge
# cheat code: dput(colnames(dat))

# hauldat <- unique(dat[,c("survey", "source", "timestamp", "haul_id", "country", "sub_area", 
#                          "continent", "stat_rec", "station", "stratum", "year", "month", 
#                          "day", "quarter", "season", "latitude", "longitude", "haul_dur", 
#                          "area_swept", "gear", "depth", "sbt", "sst",  "survey_unit")])
# 
# sppdat <- unique(dat[, c("verbatim_name", "verbatim_aphia_id", 
#                      "accepted_name", "aphia_id", "SpecCode", "kingdom", "phylum", 
#                      "class", "order", "family", "genus", "rank")])
# 
# # base write.csv introduced weird errors into the haul_id column 
# readr::write_csv(sppdat, file=here("data","species_data.csv"))
# readr::write_csv(hauldat, file=here("data","haul_data.csv"))
# 
# # now we can cut those columns out of dat and only keep the columns that vary in each row and are used in the analysis 
# dat <- dat[,c("haul_id","num_cpue", 
#               "num_cpua", "wgt_cpue", "wgt_cpua", 
#               "accepted_name")] 

# # get expanded grid 
# dat_zeros <- as.data.table(expand.grid(haul_id=unique(dat[,haul_id]), accepted_name=unique(dat[,accepted_name]))) # every combination of taxon and haul; 10 million rows for NEUS alone! 

# # expand raw to have true absences
# dat_zeros <- merge(dat_zeros, dat[,], all.x=TRUE, by=c("haul_id", "accepted_name")) 
# dat_zeros <- copy(dat_zeros)[is.na(wgt_cpue), wgt_cpue := 0][wgt_cpue<Inf] # replace NAs with 0s; these occur after merging in taxon*haul combos where no individuals were recorded 
# dat_zeros <- copy(dat_zeros)[is.na(num_cpue), num_cpue := 0][num_cpue<Inf]
# dat_zeros <- copy(dat_zeros)[is.na(num_cpua), num_cpua := 0][num_cpua<Inf]
# dat_zeros <- copy(dat_zeros)[is.na(wgt_cpua), wgt_cpua := 0][wgt_cpua<Inf]
# 
# save(dat_zeros, file=here("data","dat_zeros.RData"))
