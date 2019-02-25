# 'ceiling' and 'floor' functions for n decimal places
# from https://stackoverflow.com/questions/35807523/r-decimal-ceiling
floor_dec <- function(x, level=1){round(x - 5*10^(-level-1), level)}
ceiling_dec <- function(x, level=1){round(x + 5*10^(-level-1), level)}

# functions for describing missing data
missing_percent_fun <- function(df, round_n = NA){
  res <- sum(is.na(df))/sum(!is.na(df))*100
  if(!is.na(round_n)){
    res <- round(res, round_n)
  }
  return(res)
}

missing_print_fun <- function(df, round_n = NA){
  ifelse(missing_percent_fun(df) == 0, 
         "none", 
         paste0(missing_percent_fun(df, round_n = 2), "%"))
}


# function for quickly getting counts and proportions for categorical demo variables
demo_fun <- function(df, var, n_round = NA){
  new_df <- df %>%
    distinct_("subid", var) %>%
    mutate_at(.vars = var,
              .funs = . %>% as.character() %>% replace_na(., "MISSING")) %>%
    count_(var) %>%
    mutate(prop = n/sum(n)) %>%
    arrange(desc(n))
  if(!is.na(n_round)){
    new_df <- new_df %>% mutate(prop = round(prop, n_round))
  }
  return(new_df)
}

# function for running factor retention protocols across multiple datasets
reten_report_fun <- function(df_list){
  
  res <- data.frame(par = NULL, bic = NULL, wdm = NULL)
  
  for(i in 1:length(df_list)){
    nfact_par <- fa.parallel(df_list[[i]],
                             cor = chosen_cor, fm = chosen_fm,
                             plot = F)$nfact
    
    nfact_bic <- vss(df_list[[i]], 
                     cor = chosen_cor, rotate = chosen_rot, fm = chosen_fm,
                     plot = F)$vss.stats$BIC %>% which.min()
    
    nfact_wdm <- reten_fun(df_list[[i]], rot_type = chosen_rot)
    
    res[names(df_list)[i], "par"] <- nfact_par
    res[names(df_list)[i], "bic"] <- nfact_bic
    res[names(df_list)[i], "wdm"] <- nfact_wdm
  }
  
  return(res)
}

# functions for summarizing factor loadings
loadings_count_dom_fun <- function(efa){
  loadings_fun(efa) %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    count(factor)
}

loadings_summarize_fun <- function(efa, abs = TRUE){
  loadings <- loadings_fun(efa)
  
  if(abs){
    loadings <- loadings %>%
      group_by(factor) %>%
      summarise(min_abs = min(abs(loading)),
                max_abs = max(abs(loading))) %>%
      ungroup()
  } else {
    loadings <- loadings %>%
      group_by(factor) %>%
      summarise(min = min(loading),
                max = max(abs(loading))) %>%
      ungroup()
  }
  return(loadings)
}

# function for getting write-up of brms model results
write_b_95CI_fun <- function(model, param, round_n = 2){
  fixef <- fixef(model) %>% round(round_n) %>% format(nsmall = round_n)
  b <- fixef[param, "Estimate"]
  lower <- fixef[param, "Q2.5"]
  upper <- fixef[param, "Q97.5"]
  text <- paste0("_b_ = ", b, 
                 ", 95% credible interval: [", lower, ", ", upper, "]")
  text <- gsub("\\[ ", "\\[", text)
  return(text)
}

# functions for plotting
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

ital_by_cap_fun <- function(str) {
  paste0("_", paste(str, collapse = "_, _"), "_")
}

