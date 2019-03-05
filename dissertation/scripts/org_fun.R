# FUNCTIONS FOR SCORING (examining organization/relationships between conceptual units)

# function for constructing scales from EFA solutions
scale_fun <- function(efa, factor_names = NA, 
                      # params set in org_param.R
                      count = chosen_count, 
                      trim = chosen_trim, 
                      n_keep = chosen_n_keep,
                      min_loading = chosen_min_loading, 
                      max_cross = chosen_max_cross){ 
  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
  }
  
  # decide whether to count raw or absolute loadings
  count_fun <- function(num){
    res <- if(count == "absolute"){abs(num)} else (num)
    return(res)
  }
  
  # get loadings
  loadings <- loadings_fun(efa)
  
  # filter out capacities iwth high cross-loadings
  if(!is.na(max_cross)){
    cross_loadings <- loadings %>%
      group_by(capacity) %>%
      top_n(-2, count_fun(loading)) %>%
      ungroup() %>%
      filter(loading >= count_fun(max_cross))
    
    loadings <- loadings %>%
      filter(!capacity %in% cross_loadings$capacity)
  }
  
  # filter out capaciites with low loadings
  if(!is.na(min_loading)){
    loadings <- loadings %>%
      filter(loading >= count_fun(min_loading))
  }
  
  # get dominant factor for each capacity
  loadings <- loadings %>%
    group_by(capacity) %>%
    top_n(1, count_fun(loading)) %>%
    ungroup()
  
  # trim to some number of capacities per factor
  if(trim){
    # decide how many to keep per factor
    if(is.na(n_keep)){
      grouped <- loadings %>% count(factor)
      n_fact <- nrow(grouped)
      how_many <- min(grouped$n)
    } else {how_many <- n_keep}
    
    # trim
    loadings <- loadings %>%
      group_by(factor) %>%
      top_n(how_many, count_fun(loading)) %>%
      ungroup()
  }
  
  loadings <- loadings  %>%
    arrange(factor, desc(count_fun(loading))) %>%
    mutate(order = 1:nrow(.))
  
  return(loadings)
  
}
