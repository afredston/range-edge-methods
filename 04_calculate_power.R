set.seed(42)
library(plyr)
library(doParallel)
library(here)
here <- here::here
detectCores()
registerDoParallel(cores=6)

source(here("functions","create_subsamples.R"))
source(here("functions","calculate_slope.R"))

# three nested for loops

# inner two for loops (in the function) say:

# for a given true rate of range shift j and time-series length k, combined with a random error term (based on literature SD values), what is the position of a simulated range shift each year? and, if we fit a regression to those simulated data (position ~ time), what is the p-value? 

# I combine this with code from Easton White to extract every possible time-series of length k, not just the first. so to explore the power of a five-year time-series, we test every five-year window in the full time series, not just years 1 to 5. 

# the outer for loop (parallelized) repeats the above 1000 times, and each time it records how long the time-series needs to be (for a given rate of range shift j) to always get significant shifts. if this value is equal to the upper value of k, it means we never got consistently significant shifts for that value of j. 

# get "true" parameters 
shiftrate = seq(0.1, 10, 0.1) / 111 # "true" shift rate -- convert from km/yr to lat/yr 
yrs = 100 
sampleyrs <- seq(1, 100, 1)
error = 0.65 # from mean(fish_edgetidy$conditional_sd) and mean(bird_edgetidy$conditional_sd)
out_true <- array(dim=c(length(shiftrate), length(sampleyrs)))
mod_stats <- NULL
sum_stats <- NULL
iters <- 100
ts_lengths <- seq(3, 100, 1) # need to run a regression so must have >2 points 
subset_times <- create_subsamples(sampleyrs)
alpha = 0.05
power=matrix(ncol=length(shiftrate),nrow=(length(ts_lengths)))


calculate_power <- function(shiftrate, sampleyrs, error, subset_times, ts_lengths, alpha) {
# simulate data 
for(j in 1:length(shiftrate)){
  for(k in 1:length(sampleyrs)){
    # simulate edge position using the true rate of shift, number of years passed, and a random error term based on the standard deviation of the actual data 
    out_true[j,k] <- shiftrate[j] * sampleyrs[k] + rnorm(n = 1, mean = 0, sd = error)
  }}

# calculate power for subsamples, sensu White 2019
# could rewrite with apply to speed up ... 
for(j in 1:length(shiftrate)) {
  ts <- out_true[j,] # get the true positions for this value of j 
  for(n in 3:100){ 
    ts_yrs <- na.omit(subset_times[[n-1]]) 
    ts_data <- matrix(nrow=nrow(ts_yrs),ncol=ncol(ts_yrs)) # set up an object to hold the true data (since ts_yrs just has year values, e.g., 1,2,3, not edge position values)
    for(l in 1:nrow(ts_yrs)){
      ts_data[l,] <- ts[min(ts_yrs[l,]):max(ts_yrs[l,])] # pull out the true data (edge positions) for each of those subset time-series   
    }
    power[n-2,j]=sum(apply(ts_data,1,calculate_p_value)<alpha  & sign(apply(ts_data,1,calculate_slope))==sign(calculate_slope(ts))  )/(nrow(ts_data)) # for each time-series length and shift rate, calculate what fraction of time-series reveal the "true trend", i.e., significant results in the correct direction 
  }
}
return(power)
} # close function 

power_out <- foreach(i = seq(1, iters, 1)) %dopar% {
  calculate_power(shiftrate=shiftrate, sampleyrs=sampleyrs, error=error, subset_times = subset_times, ts_lengths=ts_lengths, alpha=alpha)
  }

saveRDS(power_out, file=here("results","simulated_time_series_summary.rds"))
