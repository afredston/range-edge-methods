calc_all_edges <- function(dat, edgetype, focal_spp) {
  
  library(purrr)
  library(broom) 
  source("functions/calc_edge_quantile.R")
  
  check <- dat %>% 
    select(year, latitude, num_cpue) %>% 
    filter(num_cpue > 0) %>% 
    group_by(year) %>% 
    summarise(n=n())
  
  n_most_distal_points <- 5 # how many distal points to average? 
  n_most_distal_cells <- 10 # how many grid cells to average?
  edgedat <- NULL
  
  ##########
  # calculate edge positions with different methods 
  ##########
  
  # mean of n most distal points
  if(edgetype=="pol") {
    tmp <- dat %>%  
      select(year, latitude, num_cpue) %>% 
      filter(num_cpue > 0) %>%   
      group_by(year) %>% 
      arrange(-latitude) %>% # starting from highest 
      slice(1:n_most_distal_points) %>% 
      summarise(
        n_most_distal_points_lat_pol = mean(latitude[1:n_most_distal_points])
     #   n_most_distal_points_lat_pol_wgt = weighted.mean(latitude, num_cpue)
        ) %>% 
      select(year, n_most_distal_points_lat_pol) %>% 
      distinct()
  }
  
  if(edgetype=="eq"){
    tmp <- dat %>%  
      select(year, latitude, num_cpue) %>% 
      filter(num_cpue > 0) %>%   
      group_by(year) %>% 
      arrange(latitude) %>% # starting from lowest 
      slice(1:n_most_distal_points) %>% 
      summarise(
        n_most_distal_points_lat_eq = mean(latitude[1:n_most_distal_points])
     #   n_most_distal_points_lat_eq_wgt = weighted.mean(latitude, num_cpue)
     ) %>% 
      select(year, n_most_distal_points_lat_eq) %>% 
      distinct()
  }
  edgedat <- rbind(edgedat, tmp)
  
  # abundance-weighted quantiles 
  if(edgetype=="pol"){
    tmp2 <- dat %>% 
      add_count(year) %>% # get nrows by year 
      group_by(year) %>% 
      summarise(
        wt_quant_99 = calc_edge_quantile(nobs=unique(n), weights=num_cpue, ruler=latitude, quantile=0.99))        
  }
  
  if(edgetype=="eq"){
    tmp2 <- dat %>% 
      add_count(year) %>% # get nrows by year 
      group_by(year) %>% 
      summarise(
        wt_quant_01 = calc_edge_quantile(nobs=unique(n), weights=num_cpue, ruler=latitude, quantile=0.01))        
  }
  
  edgedat <- edgedat %>% 
    left_join(tmp2)
  
  # presence-based quantiles
  if(edgetype=="pol"){
    tmp3 <- dat %>% 
      add_count(year) %>% # get nrows by year 
      group_by(year) %>% 
      summarise(
        quant_95 = quantile(latitude, probs = 0.95)
      )
  }
  
  if(edgetype=="eq"){
    tmp3 <- dat %>% 
      add_count(year) %>% # get nrows by year 
      group_by(year) %>% 
      summarise(
        quant_05 = quantile(latitude, probs = 0.05)
      )
  }
  edgedat <- edgedat %>% 
    left_join(tmp3)
  
  # mean of n most distal grid cells 
  grid <- expand.grid(
    latitude_12 = seq(floor(min(dat$latitude)), ceiling(max(dat$latitude)), 1/12),
    longitude_12 = seq(floor(min(dat$longitude)), ceiling(max(dat$longitude)), 1/12)) |> 
    mutate(lonlat = paste0(longitude_12, "_", latitude_12))
  
  if(edgetype=="pol"){
  tmp4 <- dat |> 
    select(latitude, longitude, year) |> 
    mutate(latitude_12 = round(latitude * 12) / 12, 
           longitude_12 = round(longitude * 12) / 12, 
           lonlat = paste0(longitude_12, "_", latitude_12)) |> 
    select(year, lonlat) |> 
    distinct() |> # this drops repeats of the same cell 
    left_join(grid) |> 
    group_by(year) |> 
    arrange(-latitude_12) |> # sort from high to low, giving highest latitudes 
    slice(1:n_most_distal_cells) |> 
    summarise(n_most_distal_grid_cells = mean(latitude_12))
  }
  
  if(edgetype=="eq"){
    tmp4 <- dat |> 
      select(latitude, longitude, year) |> 
      mutate(latitude_12 = round(latitude * 12) / 12, 
             longitude_12 = round(longitude * 12) / 12, 
             lonlat = paste0(longitude_12, "_", latitude_12)) |> 
      select(year, lonlat) |> 
      distinct() |> # this drops repeats of the same cell 
      left_join(grid) |> 
      group_by(year) |> 
      arrange(latitude_12) |> # sort from low to high, giving lowest latitudes 
      slice(1:n_most_distal_cells) |> 
      summarise(n_most_distal_grid_cells = mean(latitude_12))
  }
  
  edgedat <- edgedat %>% 
    left_join(tmp4)
  
  edgetidy <- edgedat %>% 
    pivot_longer(!year, names_to="Method", values_to="lat_position")
  
  ##########
  # calculate summary statistics for edge methods
  ##########
  
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
             "n_most_distal_points_lat_eq" ~ "Mean of Most Distal Points", 
             "quant_05" ~ "Presence-Based 0.05 Quantile",
             "wt_quant_01" ~ "Abundance-Weighted 0.01 Quantile", 
             "n_most_distal_grid_cells" ~ "Mean of Most Distal Occupied Cells"
           ))
    
    edgetidy$Edge <- "Equatorward"
    
  }
  
  if(edgetype=="pol"){
    edgetidy <- left_join(edgetidy, edgelm) %>% 
      mutate(sig = ifelse(p.value<=0.05, "yes", "no"),
             sig = factor(sig, levels=c("yes","no")), 
             Method = case_match(
               Method, 
               "n_most_distal_points_lat_pol" ~ "Mean of Most Distal Points", 
               "quant_95" ~ "Presence-Based 0.95 Quantile",
               "wt_quant_99" ~ "Abundance-Weighted 0.99 Quantile", 
               "n_most_distal_grid_cells" ~ "Mean of Most Distal Occupied Cells"
             ))
    
    edgetidy$Edge <- "Poleward"
    
  }
  
  edgetidy$Species <- focal_spp
  
  
  if(min(check$n) < n_most_distal_points){
    edgetidy <- paste0("Some years have less than ", n_most_distal_points, "  observations, so I didn't calculate range edges for you.") 
  }
  return(edgetidy)
  
} # close function 
