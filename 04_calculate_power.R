set.seed(42)
library(tidyverse)
library(doParallel)
detectCores()
registerDoParallel(cores=4)
# three nested for loops

# inner two for loops (in the function) say:

# for a given true rate of range shift j and time-series length k, combined with a random error term (based on literature SD values), what is the position of a simulated range shift each year? and, if we fit a regression to those simulated data (position ~ time), what is the p-value? 

# the outer for loop (parallelized) repeats the above 1000 times, and each time it records how long the time-series needs to be (for a given rate of range shift j) to always get significant shifts. if this value is equal to the upper value of k, it means we never got consistently significant shifts for that value of j. 

# get "true" parameters 
shiftrate = seq(0.1, 10, 0.1) / 111 # "true" shift rate -- convert from km/yr to lat/yr 
yrs = 100 
sampleyrs <- seq(1, 100, 1)
error = 0.75 # from mean(edgetidy$conditional_sd)
out_true <- array(dim=c(length(shiftrate), length(sampleyrs)))
mod_stats <- NULL
sum_stats <- NULL
iters <- 2

simulate_edge_shifts <- function(shiftrate, sampleyrs, error) {
  for(j in 1:length(shiftrate)){
    for(k in 1:length(sampleyrs)){
      # simulate edge position using the true rate of shift, number of years passed, and a random error term based on the standard deviation of the actual data 
      out_true[j,k] <- shiftrate[j] * sampleyrs[k] + rnorm(n = 1, mean = 0, sd = error)
      
      if(k > 2){
        # now run a regression of position ~ time, the way people often do 
        tmpdat <- data.frame(y=out_true[j, 1:k], x=seq(1, k, 1))
        mod <- summary(lm(formula="y ~ x", data=tmpdat))
        
        # save results of regression 
        tmp <- tibble(rate=shiftrate[j], n_yrs=sampleyrs[k], coef=mod$coefficients[2,1], se = mod$coefficients[2,2], p = mod$coefficients[2,4])
        
        mod_stats <- rbind(mod_stats, tmp)
      } # close k
    } # close j 
  }
  #after how many years is p permanently < 0.05?
  sum_stats_tmp <- mod_stats %>% 
    mutate(sig = ifelse(p < 0.05, 1, 0)) %>% 
    group_by(rate) %>% 
    arrange(n_yrs) %>% 
    # need to find the last year in which sig = 0 
    filter(sig==0) %>% 
    mutate(sig_time_series_length = max(n_yrs)+1) %>% 
    ungroup() %>% 
    select(rate, sig_time_series_length) %>% 
    distinct()
  
  rm(mod_stats) # gets large when we run the outer for loop 
  
  sum_stats <- rbind(sum_stats, sum_stats_tmp)
  return(sum_stats)
} # close function 

power_out <- foreach(i = seq(1, iters, 1), .combine='rbind') %dopar% {
  #  library(dplyr)
  simulate_edge_shifts(shiftrate=shiftrate, sampleyrs=sampleyrs, error=error)     
}
