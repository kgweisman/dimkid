# table for continuous development regressions
regtab_devscore_fun <- function(reg_body, reg_heart, reg_mind){
  regtab <- bind_rows(
    fixef(reg_body) %>%
      data.frame() %>%
      rownames_to_column("param") %>%
      mutate(factor = "BODY"),
    fixef(reg_heart) %>%
      data.frame() %>%
      rownames_to_column("param") %>%
      mutate(factor = "HEART"),
    fixef(reg_mind) %>%
      data.frame() %>%
      rownames_to_column("param") %>%
      mutate(factor = "MIND")) %>%
    rename(b = Estimate, s.e. = Est.Error) %>%
    mutate(param = factor(
      param,
      levels = c("Intercept", "anim_inan_anim_GM", "age_centered", 
                 "anim_inan_anim_GM:age_centered"),
      labels = c("Intercept", "Animate characters vs. GM", 
                 "Exact age (centered)", "Interaction")),
      b = format(round(b, 2), nsmall = 2),
      s.e. = format(round(s.e., 2), nsmall = 2),
      CI95 = paste0("[", format(round(Q2.5, 2), nsmall = 2),
                    ", ", format(round(Q97.5, 2), nsmall = 2), "]"),
      nonzero = ifelse(Q2.5 * Q97.5 >= 0, "*", "")) %>%
    select(-starts_with("Q")) %>%
    select(-starts_with("s.e.")) %>%
    gather(key, value, -c(factor, param)) %>%
    mutate(key = case_when(factor == "BODY" ~ paste0("B_", key),
                           factor == "HEART" ~ paste0("H_", key),
                           factor == "MIND" ~ paste0("M_", key))) %>%
    select(-factor) %>%
    spread(key, value) %>%
    arrange(param)
  
  return(regtab)
}

devscore_table_fun <- function(regtab_devscore, n_characters,
                               table_name, study_name,
                               age_range, mean_age,
                               char_compare_label = "Animate characters vs. GM",
                               ranef_subid = F){
  
  if(char_compare_label == "Animate characters vs. GM"){
    ranef_text <- paste0(" In addition to the fixed effects listed here, these regressions included random intercepts for individual target characters (n=", n_characters, "). ")
  } else if(ranef_subid){ # hacky, works because there are no studies with >2 char and within-subjects design...
    ranef_text <- " In addition to the fixed effects listed here, these regressions included random intercepts for participants to account for the within-subjects design of this study. "
  } else {
    ranef_text <- " "
  }
  
  tab <- regtab_devscore %>%
    mutate(param = factor(
      param,
      levels = levels(regtab_devscore$param),
      labels = gsub("Animate characters vs. GM",
                    char_compare_label,
                    levels(regtab_devscore$param)))) %>%
    select(-contains("s.e.")) %>%
    rename(Parameter = param,
           b = B_b, b = H_b, b = M_b,
           `95% CI` = B_CI95, `95% CI` = H_CI95, `95% CI` = M_CI95,
           " " = B_nonzero, " " = H_nonzero, " " = M_nonzero) %>%
    kable(format = "html", align = c("l", rep(c(rep("r", 2), "l"), 3)), 
          caption = paste0(
            table_name,
            ": Regression analyses of age-related differences in BODY, HEART, and MIND scores among the ",
            age_range, 
            " in ", 
            study_name,
            " (scored using adults' scales, as presented in Chapter IV). For each conceptual unit, the table presents a Bayesian regression with 4 fixed effect parameters: (1) the intercept, which is an index of attributions of that conceptual unit, collapsing across target characters, at the mean age for this sample (",
            format(round(mean_age, 2), nsmall = 2),
            "y); (2) the overall difference in scores for the ", 
            gsub("vs. gm", "compared to the grand mean ('GM')",
                 tolower(char_compare_label)),
            ", at the mean age for this sample (",
            format(round(mean_age, 2), nsmall = 2),
            "y); (3) the overall effect of age on scores, collapsing across target characters; and (4) the interactive effect of age and ",
            ifelse(grepl("animate", tolower(char_compare_label)),
                   "animacy status", "target character"),
            ". The last two effects are highlighted in bold, because they are the primary parameters of interest for these analyses.",
            ranef_text,
            "For each parameter, the table includes the estimate (b) and a 95% credible interval for that estimate. Asterisks indicate 95% credible intervals that do not include 0.")) %>%  
    kable_styling() %>%
    row_spec(3:4, bold = T) %>%
    add_header_above(c(" " = 1,
                       "BODY" = 3, "HEART" = 3, "MIND" = 3))
  
  return(tab)
}

# table for age group comparisons
regtab_devgp_fun <- function(reg_body, reg_heart, reg_mind,
                             age_levels, age_labels){
  
  
  if(length(age_levels) == 1){
    param_levels <- c("Intercept", "anim_inan_anim_GM", age_levels,
                      as.character(interaction("anim_inan_anim_GM", 
                                               age_levels, sep = ":")))
    param_labels <-  c("Intercept (adults)", "Animate characters vs. GM (adults)",
                       age_labels, "Interaction")
  } else {
    param_levels <- c("Intercept", "anim_inan_anim_GM", age_levels,
                      as.character(interaction("anim_inan_anim_GM", 
                                               age_levels, sep = ":")))
    param_labels <- c("Intercept (adults)", "Animate characters vs. GM (adults)",
                      age_labels, as.character(interaction("Interaction",
                                                           age_labels, 
                                                           sep = ": ")))
  }
  
  regtab <- bind_rows(
    fixef(reg_body) %>%
      data.frame() %>%
      rownames_to_column("param") %>%
      mutate(factor = "BODY"),
    fixef(reg_heart) %>%
      data.frame() %>%
      rownames_to_column("param") %>%
      mutate(factor = "HEART"),
    fixef(reg_mind) %>%
      data.frame() %>%
      rownames_to_column("param") %>%
      mutate(factor = "MIND")) %>%
    rename(b = Estimate, s.e. = Est.Error)
  
  regtab <- regtab %>%
    mutate(param = factor(param,
                          levels = param_levels,
                          labels = param_labels))
  
  regtab <- regtab %>%
    mutate(b = format(round(b, 2), nsmall = 2),
           s.e. = format(round(s.e., 2), nsmall = 2),
           CI95 = paste0("[", format(round(Q2.5, 2), nsmall = 2),
                         ", ", format(round(Q97.5, 2), nsmall = 2), "]"),
           nonzero = ifelse(Q2.5 * Q97.5 >= 0, "*", "")) %>%
    select(-starts_with("Q")) %>%
    select(-starts_with("s.e.")) %>%
    gather(key, value, -c(factor, param)) %>%
    mutate(key = case_when(factor == "BODY" ~ paste0("B_", key),
                           factor == "HEART" ~ paste0("H_", key),
                           factor == "MIND" ~ paste0("M_", key))) %>%
    select(-factor) %>%
    spread(key, value) %>%
    arrange(param)
  
  return(regtab)
}

devgp_table_fun <- function(regtab_devgp, n_characters,
                            table_name, study_name,
                            age_groups, n_age_groups = 1,
                            char_compare_label = "Animate characters vs. GM",
                            ranef_subid = F){
  
  if(char_compare_label == "Animate characters vs. GM"){
    ranef_text <- paste0("In addition to the fixed effects listed here, these regressions included random intercepts for individual target characters (n=", n_characters, "). ")
  } else if(ranef_subid){ # hacky, works because there are no studies with >2 char and within-subjects design...
    ranef_text <- "In addition to the fixed effects listed here, these regressions included random intercepts for participants to account for the within-subjects design of this study. "
  } else {
    ranef_text <- " "
  }
  
  tab <- regtab_devgp %>%
    mutate(param = factor(
      param,
      levels = levels(regtab_devgp$param),
      labels = gsub("Animate characters vs. GM",
                    char_compare_label,
                    levels(regtab_devgp$param)))) %>%
    select(-contains("s.e.")) %>%
    rename(Parameter = param,
           b = B_b, b = H_b, b = M_b,
           `95% CI` = B_CI95, `95% CI` = H_CI95, `95% CI` = M_CI95,
           " " = B_nonzero, " " = H_nonzero, " " = M_nonzero) %>%
    kable(format = "html", align = c("l", rep(c(rep("r", 2), "l"), 3)), 
          caption = paste0(
            table_name,
            ": Regression analyses of age-related differences in BODY, HEART, and MIND scores among the ",
            age_groups, 
            " in ", 
            study_name,
            " (scored using adults' scales, as presented in Chapter IV). For each conceptual unit, the table presents a Bayesian regression with 4 fixed effect parameters: (1) the intercept, which is an index of attributions of that conceptual unit among adults; (2) the overall difference in scores for the ", 
            gsub("vs. gm", "compared to the grand mean ('GM')",
                 tolower(char_compare_label)),
            " among adults; (3) the difference between children's and adults' scores, collapsing across target characters; and (4) the interactive effect of age group and ",
            ifelse(grepl("animate", tolower(char_compare_label)),
                   "animacy status", "target character"),
            ". Age effects are highlighted in bold, because they are the primary parameters of interest for these analyses.",
            ranef_text,
            "For each parameter, the table includes the estimate (b) and a 95% credible interval for that estimate. Asterisks indicate 95% credible intervals that do not include 0.")) %>%  
    kable_styling() %>%
    row_spec(3:(3 + n_age_groups * 2 - 1), bold = T) %>%
    add_header_above(c(" " = 1,
                       "BODY" = 3, "HEART" = 3, "MIND" = 3))
  
  return(tab)
}
