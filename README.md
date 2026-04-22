# Measuring the edges of species' geographic ranges 

## Alexa Fredston 

### Description 

This repository contains all of the code and most of the data to reproduce analyses exploring methods for measuring range edge positions and edge shifts. Please cite [the manuscript](https://doi.org/10.32942/X2QP69) if you draw any data, code, or ideas from this repository. The workflow is as follows:

1. `01_prep_data.R` reshapes and filters the raw data. It uses spatial masks to do so, which are available in this repository as well. Not all the raw data are hosted here, because some files are too big (see "Data" below). 
2. `02_calculate_edges.R` applies range edge metrics to the data (see functions/calc_edge_quantile.R for more information on how they're calculated).
3. `03_analyze_edges.R` calculates statistics about those range edge metrics, such as whether they shifted and how far. It also generates Figure 1 in the text. 
4. `04_calculate_power.R` runs the simulation + non-random resampling described in the manuscript, and calculates power for different parameter combinations. 
5. `05_analyze_power.R` calculates summary statistics and makes plots based on the simulation in the previous step. 

### Data 

This project used data on an example fish and bird species. Processed datasets are available in this repository, so you don't need to run the script to regenerate them (`01_prep_data.R`) in order to reproduce the results. 

The fish species data comes from [FISHGLOB](https://github.com/fishglob/FishGlob_data) v2.0.1. I am quite familiar with these data so I chose a species that I knew was conducive to this analysis, white hake. 

To choose a bird species, I leveraged [La Sorte and Thomspon III 2007](https://doi.org/10.1890/06-1072.1), specifically their supplemental table A1. I filtered those data for southern species that were common and had a demonstrated northward shift in the analysis. Then I explored some range maps and settled on the Black Vulture. 

To download Christmas Bird Count data on the Black Vulture, I used the [Audubon data portal](https://netapp.audubon.org/CBCObservation/Historical/ResultsBySpecies.aspx?1). It had some bugs at the time so after emailing with CBC folks I manually downloaded every count from 1975 to 2023 (the most recent available at that time) for Black Vulture. 

To generate the spatial mask used to crop the fish data, I downloaded data on global EEZs from [marineregions.org](https://www.marineregions.org/). 

