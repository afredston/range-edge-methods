set.seed(42)
library(plyr)
library(doParallel)
library(here)
here <- here::here
detectCores()
registerDoParallel(cores=2)

source(here("functions","create_subsamples.R"))
source(here("functions","calculate_slope.R"))
resid_sd_dat <- read.csv(here("results","residual_sd_after_detrending.csv")) 

errors <- quantile(resid_sd_dat$residual_sd, probs=seq(0, 1, 0.5))

# four nested for loops

# inner three for loops (in the function) say:

# for a given true rate of range shift j (in deg lat/yr), time-series length k, and standard deviation m, what is the position of a simulated range shift each year? and, if we fit a regression to those simulated data (position ~ time), what is the p-value? 

# I combine this with code from Easton White to extract every possible time-series of length k, not just the first. so to explore the power of a five-year time-series, we test every five-year window in the full time series, not just years 1 to 5. 

# the outer for loop (parallelized) repeats the above 100 times, and each time it records the power of the combination of shift rate and time-series length, defined as the proportion of times the different time-series subsets are significantly different from zero and in the correct direction. 

# get "true" parameters 
shiftrate_prep = seq(0.1, 10, 0.1) / 111 # "true" shift rate -- convert from km/yr to lat/yr  -- but these numbers are ugly so let's slightly shift this to ...
shiftrate <- seq(0.001, 0.1, 0.001)
sampleyrs <- seq(1, 100, 1)
# error = 0.65 # from mean(fish_edgetidy$conditional_sd) and mean(bird_edgetidy$conditional_sd)
out_true <- array(dim=c(length(shiftrate), length(sampleyrs), length(errors))) 
iters <- 10
ts_lengths <- seq(3, 100, 1) # need to run a regression so must have >2 points (in contrast to sampleyrs) 
subset_times <- create_subsamples(sampleyrs)
alpha = 0.05
#power=matrix(ncol=length(shiftrate),nrow=(length(ts_lengths)))
power <- array(dim=c(length(shiftrate), length(ts_lengths), length(errors)))

calculate_power <- function(shiftrate, sampleyrs, errors, subset_times, ts_lengths, alpha) {
# simulate data 
for(j in 1:length(shiftrate)){
  for(k in 1:length(sampleyrs)){
    for(m in 1:length(errors)){
    # simulate edge position using the true rate of shift, number of years passed, and a random error term based on the standard deviation of the actual data 
    out_true[j,k,m] <- shiftrate[j] * sampleyrs[k] + rnorm(n = 1, mean = 0, sd = errors[m])
  }
  }
}

# calculate power for subsamples, sensu White 2019
# could rewrite with apply to speed up ... 
for(j in 1:length(shiftrate)) {
  for(m in 1:length(errors)) {
  ts <- out_true[j,,m] # get the true positions for this value of j and m 
  for(n in 3:100){ 
    ts_yrs <- na.omit(subset_times[[n-1]]) 
    ts_data <- matrix(nrow=nrow(ts_yrs),ncol=ncol(ts_yrs)) # set up an object to hold the true data (since ts_yrs just has year values, e.g., 1,2,3, not edge position values)
    for(l in 1:nrow(ts_yrs)){
      ts_data[l,] <- ts[min(ts_yrs[l,]):max(ts_yrs[l,])] # pull out the true data (edge positions) for each of those subset time-series   
    }
    power[j, n-2, m]=sum(apply(ts_data,1,calculate_p_value)<alpha  & sign(apply(ts_data,1,calculate_slope))==sign(calculate_slope(ts))  )/(nrow(ts_data)) # for each time-series length, standard deviation, and shift rate, calculate what fraction of time-series reveal the "true trend", i.e., significant results in the correct direction 
  }
} 
}
return(power)
} # close function 

power_out <- foreach(i = seq(1, iters, 1)) %dopar% {
  calculate_power(shiftrate=shiftrate, sampleyrs=sampleyrs, error=errors, subset_times = subset_times, ts_lengths=ts_lengths, alpha=alpha)
  }

saveRDS(power_out, file=here("results","simulated_time_series_summary.rds"))

save(shiftrate,
     sampleyrs,
     errors,
     iters,
     ts_lengths,
     alpha,
     file=here("results","parameters.Rdata"))
