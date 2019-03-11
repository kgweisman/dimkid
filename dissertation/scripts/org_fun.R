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
                                   "character", "factor"),
                      scale01 = T){
  scores <- df %>%
    full_join(scales %>% select(capacity, factor)) %>%
    filter(!is.na(factor)) %>%
    group_by_at(vars(one_of(grouping))) %>%
    summarise(score = mean(response_num, na.rm = T)) %>%
    ungroup()
  
  if(scale01){
    scores <- scores %>%
      mutate(score = scales::rescale(score, to = c(0, 1)))
  }
  
  if(is.null(levels(df$character))){
    df <- df %>% mutate(character = factor(character))
  }
  
  scores <- scores %>%
    mutate(character = factor(character,
                              levels = levels(df$character)))
  
  return(scores)
}

# function for getting difference scores across factors
diff_fun <- function(d_scored){
  
  # put factors in a standard order when applicable
  factor_names <- levels(d_scored$factor)
  
  body_factors <- factor_names[grepl("BODY", factor_names)]
  leftovers <- factor_names[!factor_names %in% body_factors]
  
  heart_factors <- leftovers[grepl("HEART", leftovers)]
  leftovers <- leftovers[!leftovers %in% heart_factors]
  
  mind_factors <- leftovers[grepl("MIND", leftovers)]
  
  other_factors <- leftovers[!leftovers %in% mind_factors]
  
  # save good factor names for plot
  factor_names_plot <- c(body_factors, heart_factors,
                         mind_factors, other_factors)
  
  # get factor names
  factor_names <- gsub("\\*", "", 
                       gsub("\\-", "_", 
                            gsub("\\(", "", 
                                 gsub("\\)", "", 
                                      gsub(" ", "_", factor_names_plot)))))
  n_fact <- length(factor_names)
  
  d_diff <- d_scored %>%
    mutate(factor = factor(factor, labels = factor_names)) %>%
    spread(factor, score)
  
  d_diff$diff1 <- unlist(d_diff[factor_names[1]] - 
                           d_diff[factor_names[2]]) %>% unname()
  
  diff_names <- c()
  diff_names[1] <- paste(factor_names_plot[1], "-", factor_names_plot[2])
  
  if(n_fact > 2){
    d_diff$diff2 <- unlist(d_diff[factor_names[1]] - 
                             d_diff[factor_names[3]]) %>% unname()
    diff_names[2] <- paste(factor_names_plot[1], "-", factor_names_plot[3])
    
    d_diff$diff3 <- unlist(d_diff[factor_names[2]] - 
                             d_diff[factor_names[3]]) %>% unname()
    diff_names[3] <- paste(factor_names_plot[2], "-", factor_names_plot[3])
  }
  
  if(n_fact > 3){
    d_diff$diff4 <- unlist(d_diff[factor_names[1]] -
                             d_diff[factor_names[4]]) %>% unname()
    diff_names[4] <- paste(factor_names_plot[1], "-", factor_names_plot[4])
    
    d_diff$diff5 <- unlist(d_diff[factor_names[2]] -
                             d_diff[factor_names[4]]) %>% unname()
    diff_names[5] <- paste(factor_names_plot[2], "-", factor_names_plot[4])
    
    d_diff$diff6 <- unlist(d_diff[factor_names[3]] -
                             d_diff[factor_names[4]]) %>% unname()
    diff_names[6] <- paste(factor_names_plot[3], "-", factor_names_plot[4])
  }
  
  d_diff <- d_diff %>%
    select(-one_of(factor_names)) %>%
    gather(pair, diff, -c(study, age_group, subid, character)) %>%
    mutate(pair = factor(pair, labels = diff_names))
  
  return(d_diff)
}

# function for getting regressions on difference scores into one dataframe
diff_reg_table_fun <- function(reg_list, pair_list, study_name,
                               char_label = NA, agegp_label = NA){
  
  n_reg <- length(reg_list)
  
  table <- fixef(reg_list[[1]]) %>%
    data.frame() %>%
    rownames_to_column("param") %>%
    mutate(pair = pair_list[[1]])
  
  if(n_reg > 1){
    table <- table %>%
      full_join(fixef(reg_list[[2]]) %>%
                  data.frame() %>%
                  rownames_to_column("param") %>%
                  mutate(pair = pair_list[[2]])) %>%
      full_join(fixef(reg_list[[3]]) %>%
                  data.frame() %>%
                  rownames_to_column("param") %>%
                  mutate(pair = pair_list[[3]]))
  }
  
  if(n_reg > 3){
    table <- table %>%
      full_join(fixef(reg_list[[4]]) %>%
                  data.frame() %>%
                  rownames_to_column("param") %>%
                  mutate(pair = pair_list[[4]])) %>%
      full_join(fixef(reg_list[[5]]) %>%
                  data.frame() %>%
                  rownames_to_column("param") %>%
                  mutate(pair = pair_list[[5]])) %>%
      full_join(fixef(reg_list[[6]]) %>%
                  data.frame() %>%
                  rownames_to_column("param") %>%
                  mutate(pair = pair_list[[6]]))
  }
  
  table <- table %>%
    mutate(study = study_name,
           CI95 = paste0("[", format(round(Q2.5, 2), nsmall = 2),
                         ", ", format(round(Q97.5, 2), nsmall = 2), "]"),
           nonzero = ifelse(Q2.5 * Q97.5 >= 0, "*", "")) %>%
    rename(b = Estimate, s.e. = Est.Error)
  
  params_all <- levels(factor(table$param))
  params_interaction <- params_all[grepl(":", params_all)]
  params_char <- params_all[grepl("char", params_all) &
                              !(params_all %in% params_interaction)]
  if(length(params_char) > 1){
    params_char <- paste0("character", 1:length(params_char))
  }
  params_agegp <- params_all[grepl("age_group", params_all) &
                               !(params_all %in% params_interaction)]
  params_all_ord <- c("Intercept", params_char, params_agegp,
                      params_interaction)
  
  if(is.na(params_interaction) && is.na(agegp_label)){
    table <- table %>%
      mutate(param = factor(param,
                            levels = params_all_ord,
                            labels = c("Intercept", char_label)))
  } else if(is.na(params_interaction)) {
    table <- table %>%
      mutate(param = factor(param, 
                            levels = params_all_ord,
                            labels = c("Intercept", char_label, agegp_label)))
  } else {
    table <- table %>%
      mutate(param = factor(param, 
                            levels = params_all_ord,
                            labels = c("Intercept", char_label, 
                                       agegp_label, "Interaction")))
  }
  
  table <- table %>% select(study, pair, param, b, s.e., CI95, nonzero) %>%
    arrange(study, pair, param)
  
  return(table)
}
