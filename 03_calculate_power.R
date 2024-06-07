# step 1: simulate data with true value from literature and SD from literature or my work
# y = mx + e, where e and m are from literature, and x is year. 

# step 2: sample from this N times, where N is seq(1, 100, 1) -- need to slice so it is time-series length. 

# run a regression to estimate the coefficient and calculate a p-value

# do this many times

# what proportion of times is p < 0.05? 

# this is the probability of rejecting the null if the alternative is true, ie, power 

# get "true" parameters 
m = seq(0.1, 10, 0.1) / 111 # convert from km/yr to lat/yr 
yrs = 100
sampleyrs <- seq(1, 100, 1)
e = 0.75 # from mean(edgetidy$conditional_sd)
out <- c()

m = 2
for(j in 1:5){
#  for(j in 1:length(sampleyrs)){
    out[j] = m * j + rnorm(n = 1, mean = 0, sd = e)
    # now need to regress out against 1:j 
  }


fn_sim_yr <- function(powerdat, surv, trialyrs, a){
  
  Data = powerdat %>% filter(survey==surv)
  Gompertz = lm( log(wt) ~ 1 + log(lagwt) + mhw_yes_no, data=Data )
  
  # Gompertz parameters
  alpha = Gompertz$coef['(Intercept)']
  rho = Gompertz$coef['log(lagwt)']
  conditional_sd = sqrt(mean(Gompertz$residuals^2))
  
  # MHW frequency and intensity
  prob_mhw = mean( ifelse(Data[,'mhw_yes_no']=="yes",1,0), na.rm=TRUE )
  #gamma = Gompertz$coef['mhw_yes_noyes']
  gamma = log(0.94) # from Cheung et al. 2021
  
  # dial back perfect autocorrelation so that the marginal standard deviation doesn't go to infinity 
  rho <- ifelse( abs(rho)>0.95, sign(rho)*0.95, rho )
  
  # Stationary properties (for initial condition)
  marginal_sd = conditional_sd / sqrt(1-rho^2)
  marginal_mean = alpha / (1-rho)
  
  sim_yrs <- NULL
  
  for(j in trialyrs){
    n_years = j
    logB_t = rep(NA,n_years)
    MHW_t = rbinom(n_years, size=1, prob=prob_mhw)
    
    # Initialize
    logB_t[1] = rnorm( n=1, mean=marginal_mean, sd=marginal_sd )
    #  logB_t[1] = marginal_mean # variance was too big for initializing 
    
    # Project every year 
    #  Gompertz:  log(N(t+1)) = alpha + rho * log(N(t)) + effects + error
    for( tI in 2:n_years){
      logB_t[tI] = alpha + rho*logB_t[tI-1] + gamma*MHW_t[tI] + rnorm( n=1, mean=0, sd=conditional_sd )
    }
    
    # set up dataframe to write out the results 
    tmp <- tibble(wt = exp(logB_t), year = seq(1, n_years, 1), mhw_yes_no = MHW_t, n_years = n_years, gamma = gamma, survey = unique(Data$survey), iter = a) %>% 
      arrange(year) %>% 
      mutate(wt_mt_log = log(wt / lag(wt))) 
    
    sim_yrs <- rbind(sim_yrs, tmp)
  } # close j loop (nyears)
  
  return(sim_yrs)
}

