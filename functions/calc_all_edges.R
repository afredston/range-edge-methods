calc_all_edges <- function(dat, edgetype, focal_spp) {
  
  library(purrr)
  library(broom) 
  source("functions/calc_edge_quantile.R")
  
  check <- dat %>% 
    select(year, latitude, num_cpue) %>% 
    filter(num_cpue > 0) %>% 
    group_by(year) %>% 
    summarise(n=n())
  
  # most distal point (and set up dataframe)
  if(edgetype=="pol"){
    edgedat <- dat %>% 
      select(year, latitude, num_cpue) %>% 
      filter(num_cpue > 0) %>% 
      group_by(year) %>% 
      summarise(most_distal_lat_pol = max(latitude)) 
  }
  
  if(edgetype=="eq"){
    edgedat <- dat %>% 
      select(year, latitude, num_cpue) %>% 
      filter(num_cpue > 0) %>% 
      group_by(year) %>% 
      summarise(most_distal_lat_eq = min(latitude)) 
  }
  
  # mean of 3 most distal points
  if(edgetype=="pol") {
    tmp <- dat %>%  
      select(year, latitude, num_cpue) %>% 
      filter(num_cpue > 0) %>%   
      group_by(year) %>% 
      arrange(-latitude) %>% # starting from highest 
      slice(1:3) %>% 
      summarise(
        n_most_distal_lat_pol = mean(latitude[1:3]),
        n_most_distal_lat_pol_wgt = weighted.mean(latitude, num_cpue)) %>% 
      select(year, n_most_distal_lat_pol, n_most_distal_lat_pol_wgt) %>% 
      distinct()
  }
  
  if(edgetype=="eq"){
    tmp <- dat %>%  
      select(year, latitude, num_cpue) %>% 
      filter(num_cpue > 0) %>%   
      group_by(year) %>% 
      arrange(latitude) %>% # starting from lowest 
      slice(1:3) %>% 
      summarise(
        n_most_distal_lat_eq = mean(latitude[1:3]),
        n_most_distal_lat_eq_wgt = weighted.mean(latitude, num_cpue)) %>% 
      select(year, n_most_distal_lat_eq, n_most_distal_lat_eq_wgt) %>% 
      distinct()
  }
  edgedat <- left_join(edgedat, tmp)
  
  # quantiles
  if(edgetype=="pol"){
    tmp2 <- dat %>% 
      add_count(year) %>% # get nrows by year 
      group_by(year) %>% 
      summarise(
        quant_90 = calc_edge_quantile(nobs=unique(n), weights=num_cpue, ruler=latitude, quantile=0.9),
        quant_95 = calc_edge_quantile(nobs=unique(n), weights=num_cpue, ruler=latitude, quantile=0.95), 
        quant_99 = calc_edge_quantile(nobs=unique(n), weights=num_cpue, ruler=latitude, quantile=0.99))        
  }
  
  if(edgetype=="eq"){
    tmp2 <- dat %>% 
      add_count(year) %>% # get nrows by year 
      group_by(year) %>% 
      summarise(
        quant_10 = calc_edge_quantile(nobs=unique(n), weights=num_cpue, ruler=latitude, quantile=0.1),
        quant_05 = calc_edge_quantile(nobs=unique(n), weights=num_cpue, ruler=latitude, quantile=0.05), 
        quant_01 = calc_edge_quantile(nobs=unique(n), weights=num_cpue, ruler=latitude, quantile=0.01))        
  }
  
  edgedat <- edgedat %>% 
    left_join(tmp2)
  
  edgetidy <- edgedat %>% 
    pivot_longer(!year, names_to="Method", values_to="lat_position")
  
  edgeresid <- edgetidy %>% 
    group_by(Method) %>%
    nest() %>%
    mutate(
      model = purrr::map(data, ~lm(lat_position ~ year, data=.x)),
      tidymodel = purrr::map(model, augment)
    ) %>%
    unnest(tidymodel) %>%
    select(-data, -model) %>% 
    group_by(Method) %>% 
    summarise(conditional_sd = sqrt(mean(.resid^2)))
  # I GOT THIS FROM THE MHW POWER ANALYSIS BUT THAT WAS AN AUTOREGRESSIVE MODEL
  # NOT SURE THIS IS THE RIGHT SD TO USE
  # CHECK IN TEXTBOOKS LATER 
  
  edgelm <- edgetidy %>% 
    group_by(Method) %>%
    nest() %>%
    mutate(
      model = purrr::map(data, ~lm(lat_position ~ year, data=.x)),
      tidymodel = purrr::map(model, tidy)
    ) %>%
    unnest(tidymodel) %>%
    select(-data, -model) %>% 
    filter(term=="year") %>% 
    left_join(edgeresid)
  
  if(edgetype=="eq"){
    edgetidy <- left_join(edgetidy, edgelm) %>% 
    mutate(sig = ifelse(p.value<=0.05, "yes", "no"),
           sig = factor(sig, levels=c("yes","no")), 
           Method = case_match(
             Method, 
             "most_distal_lat_eq" ~ "Most Distal Point",
             "n_most_distal_lat_eq" ~ "Mean of Most Distal Points", 
             "n_most_distal_lat_eq_wgt" ~ "Weighted Mean of Most Distal Points", 
             "quant_10" ~ "0.10 Quantile",
             "quant_05" ~ "0.05 Quantile", 
             "quant_01" ~ "0.01 Quantile"
           ))
    
    edgetidy$Edge <- "Equatorward"
    
  }
  
  if(edgetype=="pol"){
    edgetidy <- left_join(edgetidy, edgelm) %>% 
      mutate(sig = ifelse(p.value<=0.05, "yes", "no"),
             sig = factor(sig, levels=c("yes","no")), 
             Method = case_match(
               Method, 
               "most_distal_lat_pol" ~ "Most Distal Point",
               "n_most_distal_lat_pol" ~ "Mean of Most Distal Points", 
               "n_most_distal_lat_pol_wgt" ~ "Weighted Mean of Most Distal Points", 
               "quant_90" ~ "0.90 Quantile",
               "quant_95" ~ "0.95 Quantile", 
               "quant_99" ~ "0.99 Quantile"
             ))
    
    edgetidy$Edge <- "Poleward"
    
  }
  
  edgetidy$Species <- focal_spp
  
  
  if(min(check$n) < 3){
    edgetidy <- "Some years have less than three observations, so I didn't calculate range edges for you." 
  }
  return(edgetidy)
  
} # close function 
