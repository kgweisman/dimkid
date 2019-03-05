# FUNCTIONS FOR EFA (identifying conceptual units)

# general custom efa function
fa_fun <- function(df, n){ # params set in org_param.R
  efa <- fa(df, nfactors = n, missing = T, impute = "median",
            cor = chosen_cor, rotate = chosen_rot,
            fm = chosen_fm, scores = chosen_scores)
  colnames(efa$r.scores) <- paste0("F", 1:n)
  rownames(efa$r.scores) <- paste0("F", 1:n)
  names(efa$R2) <- paste0("F", 1:n)
  colnames(efa$weights) <- paste0("F", 1:n)
  colnames(efa$loadings) <- paste0("F", 1:n)
  colnames(efa$scores) <- paste0("F", 1:n)
  colnames(efa$Vaccounted) <- paste0("F", 1:n)
  return(efa)
}

# functions for implementing weisman et al.'s (2017) factor retention criteria
s_moments <- function(p) {p*(p+1)/2}
param_est <- function(p, k) {p*k + p - (k*(k-1)/2)}

check_ok <- function(p, k) {
  a <- (p-k)^2
  b <- p+k
  return(ifelse(a>b, TRUE, FALSE))
}

max_ok <- function(p) {
  df_check <- data.frame()
  for(i in 1:p){
    df_check[i,"check"] <- check_ok(p,i)
  }
  max <- df_check %>% filter(check) %>% nrow()
  return(max)
}

reten_fun <- function(df, rot_type = c("oblimin", "varimax", "none")){
  
  # figure out max number of factors to retain
  n_var <- length(names(df))
  max_k <- max_ok(n_var)
  
  # run efa with max factors, unrotated
  fa_unrot <- fa(df, nfactors = max_k, rotate = "none", 
                 scores = "tenBerge", impute = "median")
  eigen <- fa_unrot$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("param") %>%
    gather(factor, value, -param) %>%
    spread(param, value) %>%
    filter(`SS loadings` > 1, `Proportion Explained` > 0.05)
  retain_k <- nrow(eigen)
  
  fa_rot <- fa(df, nfactors = retain_k, rotate = rot_type,
               scores = "tenBerge", impute = "median")
  
  loadings <- fa_rot$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    count(factor)
  retain_k_final <- nrow(loadings)
  
  return(retain_k_final)
}

# function for extracting factor loadings
loadings_fun <- function(efa, long_wide = "long"){
  loadings_df <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity")
  
  if(long_wide == "long"){
    loadings_df <- loadings_df %>%
      gather(factor, loading, -capacity)
  }
  
  return(loadings_df)
}

# function for grabbing top n mental capacities for which a factor was dominant
top_n_domCap <- function(efa, n, factor, abs_pos = "abs"){
  
  loadings_df <- loadings_fun(efa)
  
  if(abs_pos == "abs"){
    dom_df <- loadings_df %>%
      group_by(capacity) %>%
      top_n(1, abs(loading)) %>%
      ungroup() %>%
      group_by(factor) %>%
      top_n(n, abs(loading)) %>%
      ungroup() %>%
      arrange(desc(abs(loading)))
  } else if(abs_pos == "pos"){
    dom_df <- loadings_df %>%
      group_by(capacity) %>%
      top_n(1, loading) %>%
      ungroup() %>%
      group_by(factor) %>%
      top_n(n, loading) %>%
      ungroup() %>%
      arrange(desc(loading))
  }
  
  wordings <- dom_df$capacity[dom_df$factor == factor] %>% 
    paste(collapse = "_, _")
  
  wordings <- paste0("_", wordings, "_")
  wordings <- stri_replace_last_regex(wordings, ",", ", and")
  wordings <- gsub("sense...far away", 
                   "sense whether something is close by or far away", wordings)
  wordings <- gsub("understand how someone...feeling", 
                   "understand how someone else is feeling", wordings)
  wordings <- gsub("\\.\\.\\.", "", wordings)
  
  return(wordings)
}
