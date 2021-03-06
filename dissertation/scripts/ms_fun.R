# FUNCTIONS FOR MANUSCRIPT

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

# function for making factor congruence tables
cong_table_fun <- function(efa_ad, efa_ch, 
                           factor_names_ad = c("Factor 1", "Factor 2", 
                                               "Factor 3")){
  cong <- fa.congruence(efa_ch$loadings, efa_ad$loadings)
  colnames(cong) <- factor_names_ad
  return(cong)
}

# function for writing up factor congruence
cong_report_fun <- function(efa_ad, efa_ch, factor, 
                            factor_names_ad = c("Factor 1", "Factor 2", 
                                                "Factor 3")){
  cong <- fa.congruence(efa_ch$loadings, efa_ad$loadings, digits = 4)
  cong <- cong[factor,]
  names(cong) <- factor_names_ad
  
  max_factor <- which(cong == max(cong)) %>% names()
  mid_factor <- which(cong != max(cong) & cong != min(cong)) %>% names()
  min_factor <- which(cong == min(cong)) %>% names()
  
  max_val <- cong[max_factor] %>% as.numeric() %>% 
    round(2) %>% format(nsmall = 2)
  mid_val <- cong[mid_factor] %>% as.numeric() %>%
    round(2) %>% format(nsmall = 2)
  min_val <- cong[min_factor] %>% as.numeric() %>%
    round(2) %>% format(nsmall = 2)
  
  string <- paste0("cosine similarity with ", max_factor, ": ", max_val, 
                   "; with ", mid_factor, ": ", mid_val,
                   "; with ", min_factor, ": ", min_val)
  
  return(string)
}

# function for getting % shared variance explained
vac_fun <- function(efa, factor_names, which_stat = "Proportion Explained"){
  efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == which_stat) %>%
    gather(factor, value, -stat) %>%
    mutate(factor = factor(factor, labels = factor_names))
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

# function for getting inter-factor correlations
IFcor_fun <- function(efa, factor_names = NA, remove_dup = F){
  
  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
  }
  
  # get correlations in longform
  d0 <- efa$Phi
  
  if(remove_dup){d0[lower.tri(d0, diag = T)] <- NA}
  rownames(d0) <- factor_names
  colnames(d0) <- factor_names
  
  d1 <- d0 %>%
    data.frame() %>%
    rownames_to_column("factor1") %>%
    gather(factor2, cor, -factor1)
  
  d2 <- d1 %>%
    filter(!is.na(cor)) %>%
    arrange(factor1, factor2)

  return(d2)
}

# function for italicizing items
ital_by_cap_fun <- function(str) {
  paste0("_", paste(str, collapse = "_, _"), "_")
}

# function for spelling out factor names
fact_name_fun <- function(names) {
  
  # put factors in a standard order when applicable
  body_factors <- names[grepl("BODY", names)]
  
  leftovers <- names[!names %in% body_factors]
  heart_factors <- leftovers[grepl("HEART", leftovers)]
  
  leftovers <- leftovers[!leftovers %in% heart_factors]
  mind_factors <- leftovers[grepl("MIND", leftovers)]
  
  other_factors <- leftovers[!leftovers %in% mind_factors]
  
  list0 <- c(body_factors, heart_factors, mind_factors, other_factors)

  # make a pretty prose list
  list1 <- paste0('_', paste(list0, collapse = '_, _'), '_')
  list2 <- gsub("\\*", "\\\\\\*", list1)
  if(length(names) > 2){
    list3 <- stri_replace_last_regex(list2, ",", ", and")
  } else {
    list3 <- stri_replace_last_regex(list2, ",", " and")
  }
  return(list3)
}

# function for getting nonzero effects
nonzero_fun <- function(regtab, which_pair, 
                        inc_intercept = F, pos_neg = "both"){
  newtab <- regtab %>%
    filter(pair == which_pair, nonzero == "*")
  
  if(!inc_intercept){
    newtab <- newtab %>% filter(tolower(param) != "intercept")
  }
  
  if(pos_neg == "pos"){
    newtab <- newtab %>%
      filter(b > 0)
  } else if(pos_neg == "neg"){
    newtab <- newtab %>%
      filter(b < 0)
  }
  
  newtab <- newtab %>%
    mutate(param = gsub(" vs.*$", "", tolower(param)),
           param = gsub("pvs", "person in a persistent vegetative state (PVS)", 
                        param))
  
  output <- paste(newtab$param, collapse = ", ")
  
  return(output)
}

# function for printing correlations between scores
score_cor_print_fun <- function(df_scored, which_pair, which_res = "both"){
  cor_tab <- score_cor_fun(df_scored) %>%
    filter(pair == which_pair) %>%
    mutate_at(vars(ci_lower, r, ci_upper),
              funs(format(round(., 2), nsmall = 2))) %>%
    mutate(p = format(round(p, 3), nsmall = 3))

  p_res <- ifelse(cor_tab$p < 0.001, "p < 0.001",
                  paste0("p = ", cor_tab$p))
  
  if(which_res == "CI"){
    res <- paste0("r = ", cor_tab$r, "; 95% CI: [", 
                  cor_tab$ci_lower, ", ", cor_tab$ci_upper, "]")
  } else if(which_res == "p"){
    res <- paste0("r = ", cor_tab$r, "; ", p_res)
  } else if(which_res == "both"){
    res <- paste0("r = ", cor_tab$r, "; ", p_res, "; 95% CI: [", 
                  cor_tab$ci_lower, ", ", cor_tab$ci_upper, "]")
  }
  
  return(res)
}

# function for getting % modal responding for diffscores
modal_percent_fun <- function(table = diffscores_tab, which_pair, which_age_group){
  sum_tab <- table %>%
    filter(pair %in% which_pair, age_group == which_age_group) %>%
    summarise(min = min(modal), max = max(modal))
  
  min <- round(sum_tab$min, 2) * 100
  max <- round(sum_tab$max, 2) * 100
  str <- paste0(min, "-", max, "%")
  
  return(str)
}

# function for printing mean + CI for scores
score_mean_print_fun <- function(table, which_factor, which_age_group,
                                 which_character = NA, which_anim = NA){
  
  if(!is.na(which_character)){
    sum_tab <- table %>%
      filter(factor == which_factor, age_group == which_age_group,
             character == which_character)
  } else {
    sum_tab <- table %>%
      filter(factor == which_factor, age_group == which_age_group,
             anim_inan == which_anim)
  }
  
  sum_tab <- sum_tab %>%
    mutate_at(vars(ci_lower, ci_upper, mean), 
             funs(format(round(., 2), nsmall = 2)))
  
  str <- paste0(sum_tab$mean, ", 95% CI: [", 
                sum_tab$ci_lower, "-", sum_tab$ci_upper, "]")
  
  return(str)
  
}

# another function for printing mean + CI for correlations among scores
score_cor_by_anim_print_fun <- function(df_scores, which_pair){
  r <- df_scores$r[df_scores$pair == which_pair]
  CI <- df_scores$CI95[df_scores$pair == which_pair]
  str <- paste0("r = ", r, " ", CI)
  return(str)
}

# function for printing summary stats for scores
stat_range_print_fun <- function(which_age_group, which_anim, which_stat,
                                 scores_sum_df = scores_sum){
  df <- scores_sum %>% 
    filter(!grepl("Study 1", study),
           age_group == which_age_group,
           anim_inan == which_anim) %>% 
    gather(key, value, -c(study, anim_inan, age_group)) %>%
    filter(key == which_stat)
  
  stat_min <- min(df$value) %>% round(., 2) %>% format(., nsmall = 2)
  stat_max <- max(df$value) %>% round(., 2) %>% format(., nsmall = 2)
  str <- paste0(stat_min, "-", stat_max)
  
  return(str)
}