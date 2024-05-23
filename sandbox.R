# download from FISHGLOB: https://github.com/AquaAuma/FishGlob_data/blob/main/outputs/Cleaned_data/NEUS_clean.RData
# too big to track on GH (added to gitignore)

# pull in data

library(data.table)
library(here)

load(here("data","NEUS_clean.Rdata"))
# contains two objects, 'data' and 'readme'
# be sure to read the readme to understand the column units and values 

focal_spp <- 'Paralichthys dentatus' # can change later

data <- data.table(data) 

dat <- data[accepted_name == focal_spp & season == 'Fall']

hist(dat$wgt) # note that the data are already zero-inflated--most of the records are zeros 

# note that the standardized catch per unit effort / area columns are NA in this dataset

# this is because NEUS doesn't report these, and they are complicated to calculate; see https://github.com/AquaAuma/FishGlob_data/tree/main/metadata_docs

# we approach this as follows...

# STEP 1: get rid of hauls that are not close to the NOAA standard (complicating things, this was 30 min before 2009 and 20 min after)

neus_bad_hauls <- unique(dat[(year < 2009 & (haul_dur < 0.42 | haul_dur > 0.58)) | (year >= 2009 & (haul_dur < 0.25  | haul_dur > 0.42)),haul_id]) # pulls out haul IDs of bad hauls 

dat <- dat[!haul_id %in% neus_bad_hauls]

# STEP 2: calculate CPUE. as per NOAA staff, the reported biomass is calibrated to the pre-2009 30-minute tow duration (which is why we divide all wgt by 0.5 regardless of year). the average trawl swept area value is also provided by NOAA staff. 

dat <- dat[,wgt_cpue := wgt/0.5][,wgt_cpua := wgt/0.0384][,num_cpue := num/0.5][,num_cpua := num/0.0384]

# you can now use any of these indices of biomass -- kg / hr or kg / km^2, or num / hr or num / km^2 -- for EDM

save(dat, file=here("data","summer_flounder.RData"))

# calculate annual edge with different methods 

# latitudinal distal

# latitudinal quantile of presence

# latitudinal quantile of abundance

# 2D contour distal

# 2D contour quantile of presence

# 2D contour quantile of abundance

# SDM outputs 
