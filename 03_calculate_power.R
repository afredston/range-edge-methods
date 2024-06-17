set.seed(42)
library(tidyverse)
# step 1: simulate data with true value from literature and SD from literature or my work
# y = mx + e, where e and m are from literature, and x is year. 

# step 2: sample from this N times, where N is seq(1, 100, 1) -- need to slice so it is time-series length. 

# run a regression to estimate the coefficient and calculate a p-value

# do this many times

# what proportion of times is p < 0.05? 

# this is the probability of rejecting the null if the alternative is true, ie, power 

# get "true" parameters 
shiftrate = seq(0.1, 10, 0.1) / 111 # "true" shift rate -- convert from km/yr to lat/yr 
yrs = 100 
sampleyrs <- seq(1, 100, 1)
e = 0.75 # from mean(edgetidy$conditional_sd)
out_true <- array(dim=c(length(shiftrate), length(sampleyrs)))
sum_stats <- NULL
  
for(j in 1:length(shiftrate)){
  for(k in 1:length(sampleyrs)){
    # simulate edge position using the true rate of shift, number of years passed, and a random error term based on the standard deviation of the actual data 
    out_true[j,k] <- shiftrate[j] * sampleyrs[k] + rnorm(n = 1, mean = 0, sd = e)
    
    if(k > 1){
    # now run a regression of position ~ time, the way people often do 
    tmpdat <- data.frame(y=out_true[j, 1:k], x=seq(1, k, 1))
    mod <- summary(lm(formula="y ~ x", data=tmpdat))
    
    # save results of regression 
    tmp <- tibble(shiftrate=j, sampleyrs=k, coef=mod$coefficients[2,1], se = mod$coefficients[2,2], p = mod$coefficients[2,4])
    
    sum_stats <- rbind(sum_stats, tmp)
    }
  }
}


