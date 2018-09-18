# custom functions for efa

fa_fun <- function(df, n){
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

top_n_domCap <- function(efa, n, factor, abs_pos = "abs"){
  
  loadings_df <- loadings_fun(efa)
  
  if(abs_pos == "abs"){
    dom_df <- loadings_df %>%
      group_by(capacity) %>%
      top_n(1, abs(loading)) %>%
      ungroup() %>%
      group_by(factor) %>%
      top_n(n, abs(loading))
  } else if(abs_pos == "pos"){
    dom_df <- loadings_df %>%
      group_by(capacity) %>%
      top_n(1, loading) %>%
      ungroup() %>%
      group_by(factor) %>%
      top_n(n, loading)
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
