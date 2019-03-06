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
  
  # put factors in a standard order when applicable
  body_factors <- factor_names[grepl("BODY", factor_names)]
  
  leftovers <- factor_names[!factor_names %in% body_factors]
  heart_factors <- leftovers[grepl("HEART", leftovers)]
  
  leftovers <- leftovers[!leftovers %in% heart_factors]
  mind_factors <- leftovers[grepl("MIND", leftovers)]
  
  other_factors <- leftovers[!leftovers %in% mind_factors]
  
  factor_levels <- c(body_factors, heart_factors, mind_factors, other_factors)
  
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
    grouped <- loadings %>% count(factor)
    n_fact <- nrow(grouped)
    how_many <- min(n_keep, min(grouped$n), na.rm = T)

    # trim
    loadings <- loadings %>%
      group_by(factor) %>%
      top_n(how_many, count_fun(loading)) %>%
      ungroup()
  }
  
  loadings <- loadings  %>%
    arrange(factor, desc(count_fun(loading))) %>%
    mutate(order = 1:nrow(.),
           factor = factor(factor, labels = factor_names),
           factor = factor(as.character(factor), levels = factor_levels))
  
  return(loadings)
  
}

# function for scoring participants
score_fun <- function(df, scales,
                      grouping = c("study", "age_group", "subid",
                                   "character", "factor")){
  scores <- df %>%
    full_join(scales %>% select(capacity, factor)) %>%
    filter(!is.na(factor)) %>%
    group_by_at(vars(one_of(grouping))) %>%
    summarise(score = mean(response_num, na.rm = T)) %>%
    ungroup()
  return(scores)
}
