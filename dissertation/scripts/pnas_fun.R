# functions from Weisman et al., 2017

# make cleanup functions
cleanup <- function(datasource) {
  if(datasource %in% c("study 1", "study 2")) {
    
    # set target dataset
    if(datasource == "study 1"){d <- d_raw_study1}
    if(datasource == "study 2"){d <- d_raw_study2}
    
    # enact exclusionary criteria
    d_clean_1 <- d %>%
      mutate(finished_mod = ifelse(is.na(CATCH), 0,
                                   ifelse(finished == 1, 1,
                                          0.5))) %>%
      filter(CATCH == 1, # exclude Ps who fail catch trials 
             finished_mod != 0) %>% # exclude Ps who did not complete task
      mutate(yob_correct = as.numeric(
        ifelse(as.numeric(as.character(yob)) > 1900 & 
                 as.numeric(as.character(yob)) < 2000, 
               as.numeric(as.character(yob)), NA)), # correct formatting in yob
        age_approx = 2016 - yob_correct) %>% # calculate approximate age
      mutate(gender = factor(gender, levels = c(1, 2, 0), 
                             labels = c("m", "f", "other"))) %>%
      filter(age_approx >= 18) # exclude Ps who are younger than 18 years
    
    # recode background and demographic variables
    d_clean <- d_clean_1 %>%
      mutate( # deal with study number
        study = factor(study)) %>%
      mutate( # deal with study duration
        duration = as.numeric(difftime(strptime(end_time, "%I:%M:%S"),
                                       strptime(start_time, "%I:%M:%S"),
                                       units = "mins"))) %>%
      mutate( # deal with race
        race_asian_east = 
          factor(ifelse(is.na(race_asian_east), "", "asian_east ")),
        race_asian_south = 
          factor(ifelse(is.na(race_asian_south), "", "asian_south ")),
        race_asian_other = 
          factor(ifelse(is.na(race_asian_other), "", "asian_other ")),
        race_black = 
          factor(ifelse(is.na(race_black), "", "black ")),
        race_hispanic = 
          factor(ifelse(is.na(race_hispanic), "", "hispanic ")),
        race_middle_eastern = 
          factor(ifelse(is.na(race_middle_eastern), "", "middle_eastern ")),
        race_native_american = 
          factor(ifelse(is.na(race_native_american), "", "native_american ")),
        race_pac_islander = 
          factor(ifelse(is.na(race_pac_islander), "", "pac_islander ")),
        race_white = 
          factor(ifelse(is.na(race_white), "", "white ")),
        race_other_prefno = 
          factor(ifelse(is.na(race_other_prefno), "", "other_prefno ")),
        race_cat = paste0(race_asian_east, race_asian_south, race_asian_other,
                          race_black, race_hispanic, race_middle_eastern,
                          race_native_american, race_pac_islander, race_white,
                          race_other_prefno),
        race_cat2 = factor(sub(" +$", "", race_cat)),
        race_cat3 = factor(ifelse(grepl(" ", race_cat2) == T, "multiracial",
                                  as.character(race_cat2)))) %>%
      dplyr::select(study, subid:end_time, duration, finished:gender, 
                    religion_buddhism:age_approx, race_cat3) %>%
      rename(race_cat = race_cat3) %>%
      mutate( # deal with religion
        religion_buddhism = 
          factor(ifelse(is.na(religion_buddhism), "", "buddhism ")),
        religion_christianity = 
          factor(ifelse(is.na(religion_christianity), "", "christianity ")),
        religion_hinduism = 
          factor(ifelse(is.na(religion_hinduism), "", "hinduism ")),
        religion_islam = 
          factor(ifelse(is.na(religion_islam), "", "islam ")),
        religion_jainism = 
          factor(ifelse(is.na(religion_jainism), "", "jainism ")),
        religion_judaism = 
          factor(ifelse(is.na(religion_judaism), "", "judaism ")),
        religion_sikhism = 
          factor(ifelse(is.na(religion_sikhism), "", "sikhism ")),
        religion_other = 
          factor(ifelse(is.na(religion_other), "", "other ")),
        religion_none = 
          factor(ifelse(is.na(religion_none), "", "none ")),
        religion_prefno = 
          factor(ifelse(is.na(religion_prefno), "", "other_prefno ")),
        religion_cat = paste0(religion_buddhism, religion_christianity, 
                              religion_hinduism, religion_islam, 
                              religion_jainism, religion_judaism, 
                              religion_sikhism, religion_other, 
                              religion_none, religion_prefno),
        religion_cat2 = factor(sub(" +$", "", religion_cat)),
        religion_cat3 = factor(ifelse(grepl(" ", religion_cat2) == T, 
                                      "multireligious",
                                      as.character(religion_cat2)))) %>%
      dplyr::select(study:gender, feedback:race_cat, religion_cat3) %>%
      rename(religion_cat = religion_cat3)
    
    # remove extraneous dfs and variables
    rm(d, d_clean_1)
  }
  
  if(datasource == "study 3") {
    
    # set target dataset
    d <- d_raw_study3
    
    # enact exclusionary criteria
    d_clean_1 <- d %>%
      mutate(finished_mod = ifelse(is.na(CATCH..characterL) | 
                                     is.na(CATCH..characterR), 0,
                                   ifelse(finished == 1, 1,
                                          0.5))) %>%
      filter(CATCH..characterL == 5, # exclude Ps who fail catch trials 
             CATCH..characterR == 5,
             finished_mod != 0) %>% # exclude Ps who did not complete task
      mutate(yob_correct = as.numeric(
        ifelse(as.numeric(as.character(yob)) > 1900 & 
                 as.numeric(as.character(yob)) < 2000, 
               as.numeric(as.character(yob)), NA)), # correct formatting in yob
        age_approx = 2016 - yob_correct) %>% # calculate approximate age
      mutate(gender = factor(gender, levels = c(1, 2, 0), 
                             labels = c("m", "f", "other"))) %>%
      filter(age_approx >= 18) # exclude Ps who are younger than 18 years
    
    # recode background and demographic variables
    d_clean_2 <- d_clean_1 %>%
      mutate( # deal with study number
        study = factor(study)) %>%
      mutate( # deal with study duration
        duration = as.numeric(difftime(strptime(end_time, "%I:%M:%S"),
                                       strptime(start_time, "%I:%M:%S"),
                                       units = "mins"))) %>%
      mutate( # deal with race
        race_asian_east = 
          factor(ifelse(is.na(race_asian_east), "", "asian_east ")),
        race_asian_south = 
          factor(ifelse(is.na(race_asian_south), "", "asian_south ")),
        race_asian_other = 
          factor(ifelse(is.na(race_asian_other), "", "asian_other ")),
        race_black = 
          factor(ifelse(is.na(race_black), "", "black ")),
        race_hispanic = 
          factor(ifelse(is.na(race_hispanic), "", "hispanic ")),
        race_middle_eastern = 
          factor(ifelse(is.na(race_middle_eastern), "", "middle_eastern ")),
        race_native_american = 
          factor(ifelse(is.na(race_native_american), "", "native_american ")),
        race_pac_islander = 
          factor(ifelse(is.na(race_pac_islander), "", "pac_islander ")),
        race_white = 
          factor(ifelse(is.na(race_white), "", "white ")),
        race_other_prefno = 
          factor(ifelse(is.na(race_other_prefno), "", "other_prefno ")),
        race_cat = paste0(race_asian_east, race_asian_south, race_asian_other,
                          race_black, race_hispanic, race_middle_eastern,
                          race_native_american, race_pac_islander, race_white,
                          race_other_prefno),
        race_cat2 = factor(sub(" +$", "", race_cat)),
        race_cat3 = factor(ifelse(grepl(" ", race_cat2) == T, "multiracial",
                                  as.character(race_cat2)))) %>%
      dplyr::select(study, subid:end_time, duration, finished:gender, 
                    religion_buddhism:age_approx, race_cat3) %>%
      rename(race_cat = race_cat3) %>%
      mutate( # deal with religion
        religion_buddhism = 
          factor(ifelse(is.na(religion_buddhism), "", "buddhism ")),
        religion_christianity = 
          factor(ifelse(is.na(religion_christianity), "", "christianity ")),
        religion_hinduism = 
          factor(ifelse(is.na(religion_hinduism), "", "hinduism ")),
        religion_islam = 
          factor(ifelse(is.na(religion_islam), "", "islam ")),
        religion_jainism = 
          factor(ifelse(is.na(religion_jainism), "", "jainism ")),
        religion_judaism = 
          factor(ifelse(is.na(religion_judaism), "", "judaism ")),
        religion_sikhism = 
          factor(ifelse(is.na(religion_sikhism), "", "sikhism ")),
        religion_other = 
          factor(ifelse(is.na(religion_other), "", "other ")),
        religion_none = 
          factor(ifelse(is.na(religion_none), "", "none ")),
        religion_prefno = 
          factor(ifelse(is.na(religion_prefno), "", "other_prefno ")),
        religion_cat = paste0(religion_buddhism, religion_christianity, 
                              religion_hinduism, religion_islam, 
                              religion_jainism, religion_judaism, 
                              religion_sikhism, religion_other, 
                              religion_none, religion_prefno),
        religion_cat2 = factor(sub(" +$", "", religion_cat)),
        religion_cat3 = factor(ifelse(grepl(" ", religion_cat2) == T, 
                                      "multireligious",
                                      as.character(religion_cat2)))) %>%
      dplyr::select(study:gender, feedback:race_cat, religion_cat3) %>%
      rename(religion_cat = religion_cat3)
    
    # rename response variables
    d_clean_3 <- d_clean_2
    names(d_clean_3) <- gsub("get", "", names(d_clean_3))
    names(d_clean_3) <- gsub("\\.", "", names(d_clean_3))
    names(d_clean_3) <- gsub("char", "_char", names(d_clean_3))
    names(d_clean_3)[names(d_clean_3) %in% c("_characterL", "_characterR")] <- 
      c("characterL", "characterR")
    
    # recode response variables (center)
    d_clean_4 <- d_clean_3
    for(i in 11:92) {
      d_clean_4[,i] <- d_clean_4[,i] - 4 # transform from 1 to 7 --> -3 to 3
    }
    
    # recode characterL vs. characterR as beetle vs. robot
    d_clean_5_demo <- d_clean_4 %>%
      dplyr::select(study:condition, yob:religion_cat)
    
    d_clean_5_characterL <- d_clean_4 %>%
      mutate(target = characterL) %>%
      dplyr::select(study, subid, target, happy_characterL:CATCH_characterL)
    names(d_clean_5_characterL) <- gsub("_characterL", "", 
                                        names(d_clean_5_characterL))
    
    d_clean_5_characterR <- d_clean_4 %>%
      mutate(target = characterR) %>%
      dplyr::select(study, subid, target, happy_characterR:CATCH_characterR)
    names(d_clean_5_characterR) <- gsub("_characterR", "", 
                                        names(d_clean_5_characterR))
    
    d_clean <- d_clean_5_characterL %>%
      full_join(d_clean_5_characterR) %>%
      full_join(d_clean_5_demo) %>%
      dplyr::select(study, subid, date:religion_cat, target:CATCH)
    
    # remove extraneous dfs and variables
    rm(d, d_clean_1, d_clean_2, d_clean_3, d_clean_4, d_clean_5_characterL, 
       d_clean_5_characterR, d_clean_5_demo, i)
  }
  
  if(datasource == "study 4") {
    
    # set target dataset
    d <- d_raw_study4
    
    # enact exclusionary criteria
    d_clean_1 <- d %>%
      mutate(finished_mod = ifelse(is.na(CATCH), 0,
                                   ifelse(finished == 1, 1,
                                          0.5))) %>%
      filter(CATCH == 1, # exclude Ps who fail catch trials 
             finished_mod != 0) %>% # exclude Ps who did not complete task
      mutate(yob_correct = as.numeric(
        ifelse(as.numeric(as.character(yob)) > 1900 & 
                 as.numeric(as.character(yob)) < 2000, 
               as.numeric(as.character(yob)), NA)), # correct formatting in yob
        age_approx = 2016 - yob_correct) %>% # calculate approximate age
      mutate(gender = factor(gender, levels = c(1, 2, 0), 
                             labels = c("m", "f", "other"))) %>%
      filter(age_approx >= 18) # exclude Ps who are younger than 18 years
    
    # recode one character
    d_clean_2 <- d_clean_1 %>%
      mutate(condition = factor(ifelse(
        grepl("vegetative", as.character(condition)), "pvs",
        ifelse(grepl("blue", as.character(condition)), "bluejay",
               ifelse(grepl("chimp", as.character(condition)), "chimp",
                      as.character(condition))))))
    
    # recode background and demographic variables
    d_clean <- d_clean_2 %>%
      mutate( # deal with study number
        study = factor(study)) %>%
      mutate( # deal with study duration
        duration = as.numeric(difftime(strptime(end_time, "%I:%M:%S"),
                                       strptime(start_time, "%I:%M:%S"),
                                       units = "mins"))) %>%
      mutate( # deal with race
        race_asian_east = 
          factor(ifelse(is.na(race_asian_east), "", "asian_east ")),
        race_asian_south = 
          factor(ifelse(is.na(race_asian_south), "", "asian_south ")),
        race_asian_other = 
          factor(ifelse(is.na(race_asian_other), "", "asian_other ")),
        race_black = 
          factor(ifelse(is.na(race_black), "", "black ")),
        race_hispanic = 
          factor(ifelse(is.na(race_hispanic), "", "hispanic ")),
        race_middle_eastern = 
          factor(ifelse(is.na(race_middle_eastern), "", "middle_eastern ")),
        race_native_american = 
          factor(ifelse(is.na(race_native_american), "", "native_american ")),
        race_pac_islander = 
          factor(ifelse(is.na(race_pac_islander), "", "pac_islander ")),
        race_white = 
          factor(ifelse(is.na(race_white), "", "white ")),
        race_other_prefno = 
          factor(ifelse(is.na(race_other_prefno), "", "other_prefno ")),
        race_cat = paste0(race_asian_east, race_asian_south, race_asian_other,
                          race_black, race_hispanic, race_middle_eastern,
                          race_native_american, race_pac_islander, race_white,
                          race_other_prefno),
        race_cat2 = factor(sub(" +$", "", race_cat)),
        race_cat3 = factor(ifelse(grepl(" ", race_cat2) == T, "multiracial",
                                  as.character(race_cat2)))) %>%
      dplyr::select(study, subid:end_time, duration, finished:gender, 
                    education:age_approx, race_cat3) %>%
      rename(race_cat = race_cat3)
    
    # filter conditions if desired
    if(is.element("none", chosenExclude)) {} else {
      d_clean <- d_clean %>%
        filter(!condition %in% chosenExclude)
    }
    
    # remove extraneous dfs and variables
    rm(d, d_clean_1, d_clean_2)
  }
  
  #   # transform to 0 to 6 scale
  #   d_clean <- d_clean %>%
  #     gather(mc, score, happy:pride) %>%
  #     mutate(score = score + 3) %>% # transform from -3 to 3 --> 0 to 6
  #     spread(mc, score)
  
  # remove outliers
  if(chosenOutlierHandling == "remove") {
    
    if(datasource %in% c("study 1", "study 2", "study 4")) {
      d_clean <- d_clean %>%
        gather(mc, score, happy:pride) %>%
        group_by(condition, mc) %>%
        filter(!score %in% boxplot.stats(score, 2.5)$out) %>%
        spread(mc, score) %>%
        arrange(condition, subid)
    }
    
    if(datasource == "study 3") {
      d_clean <- d_clean %>%
        gather(mc, score, happy:pride) %>%
        group_by(target, mc) %>%
        filter(!score %in% boxplot.stats(score, 2.5)$out) %>%
        spread(mc, score) %>%
        arrange(target, subid)
    }
    
  }
  
  # filter items if desired
  if(is.element("none", chosenExcludeItem)) {} else {
    d_clean <- d_clean %>%
      dplyr::select(-contains(chosenExcludeItem))
  }
  
  # return cleaned dataset
  return(d_clean)
}
makeDRDF <- function(datasource, chosenCondition) {
  
  # set target dataset
  if(datasource == "study 1"){d <- d1}
  if(datasource == "study 2"){d <- d2}
  if(datasource == "study 3"){
    # rename variables for ease of function applpication
    d <- d3 %>%
      rename(order = condition,
             condition = target)
    
    # rename subids by condition if collapses across conditions
    d <- d %>%
      mutate(subid = paste(condition, subid, sep = "_"))
  }
  if(datasource == "study 4"){d <- d4}
  
  # filter by condition if specified
  if(chosenCondition %in% c("beetle", "robot")) {
    d <- d %>% filter(condition == chosenCondition)
  }
  
  # make stripped dataframe for dimension reducation analyses
  d_strip <- d %>%
    dplyr::select(subid, happy:pride)
  d_strip <- data.frame(d_strip[,-1], row.names = d_strip$subid)
  
  # return stripped dataframe
  return(d_strip)
}

# make renaming function
rename_fun <- function(df){
  df_renamed <- df %>%
    rename(`getting hungry` = hungry, 
           `experiencing pain` = pain,
           `feeling tired` = tired, 
           `experiencing fear` = fear,
           `doing computations` = computations, 
           `experiencing pleasure` = pleasure,
           `being conscious` = conscious, 
           `having free will` = free_will,
           `feeling safe` = safe, 
           `having desires` = desires,
           `feeling calm` = calm, 
           `feeling nauseated` = nauseated,
           `getting angry` = angry, 
           `having intentions` = intentions,
           `being self-aware` = self_aware, 
           `detecting odors` = odors,
           `feeling embarrassed` = embarrassed, 
           `experiencing pride` = pride,
           `feeling love` = love, 
           `experiencing guilt` = guilt,
           `feeling depressed` = depressed, 
           `feeling disrespected` = disrespected,
           `holding beliefs` = beliefs, 
           `understanding... feeling` = emo_recog,
           `experiencing joy` = joy, 
           `having a personality` = personality,
           `feeling happy` = happy, 
           `telling right from wrong` = morality,
           `having thoughts` = thoughts, 
           `exercising self-restraint` = self_restraint,
           `remembering things` = remembering, 
           `recognizing others` = recognizing,
           `sensing temperatures` = temperature, 
           `communicating...` = communicating,
           `working toward a goal` = goal, 
           `perceiving depth` = depth,
           `detecting sounds` = sounds, 
           `seeing things` = seeing,
           `making choices` = choices, 
           `reasoning about things` = reasoning)
  
  return(df_renamed)
}

# make hierarchy functions (specific to these studies)
catscore_fun_pnas <- function(df, which_efa){
  
  how_many_cap <- loadings_fun(which_efa) %>%
    group_by(capacity) %>%
    top_n(1, loading) %>%
    ungroup() %>%
    count(factor)
  how_many_cap <- min(how_many_cap$n)
  
  loadings <- loadings_fun(which_efa) %>%
    group_by(capacity) %>%
    top_n(1, loading) %>%
    ungroup() %>%
    group_by(factor) %>%
    top_n(how_many_cap, loading) %>%
    ungroup()
  
  bypart <-  df %>%
    # select(-c(study, date, start_time, end_time, duration, finished, mturkcode,
    #           CATCH, yob, gender, feedback, display_order, finished_mod, 
    #           yob_correct, age_approx, race_cat, religion_cat)) %>%
    select(subid, condition, `feeling happy`:`experiencing pride`) %>%
    distinct() %>%
    gather(capacity, response_num, -c(subid, condition)) %>%
    filter(capacity %in% loadings$capacity) %>%
    left_join(loadings) %>%
    group_by(subid, condition, factor) %>%
    mutate(score = mean((response_num + 3), na.rm = T)) %>% # make 0-6 scale
    ungroup() %>%
    distinct(subid, condition, factor, score)

  return(bypart)
}

hier_plot_fun_pnas <- function(df, factor1, factor2, which_efa){
  
  bypart <- catscore_fun_pnas(df, which_efa)
  
  means <- bypart %>%
    distinct(subid, condition, factor, score) %>%
    group_by(condition, factor) %>%
    multi_boot_standard(col = "score") %>%
    ungroup()
  
  means_x <- means %>%
    filter(factor == factor1) %>%
    select(-factor) %>%
    rename(ci_lower_x = ci_lower,
           ci_upper_x = ci_upper,
           mean_x = mean)
  
  means_y <- means %>%
    filter(factor == factor2) %>%
    select(-factor) %>%
    rename(ci_lower_y = ci_lower,
           ci_upper_y = ci_upper,
           mean_y = mean)
  
  means_xy <- full_join(means_x, means_y)
  
  plot <- means_xy %>%
    ggplot(aes(x = mean_x, y = mean_y,
               color = condition, fill = condition, shape = condition)) +
    geom_abline(lty = 2) +
    geom_jitter(data = bypart %>% spread(factor, score),
                aes_string(x = factor1, y = factor2),
                alpha = 0.4, width = 0.3, height = 0.3) +
    geom_errorbarh(aes(xmin = ci_lower_x, xmax = ci_upper_x), 
                   color = "black", height = 0, size = 0.6) +
    geom_errorbar(aes(ymin = ci_lower_y, ymax = ci_upper_y),
                  color = "black", width = 0, size = 0.6) +
    geom_point(color = "black", shape = 21, size = 2, stroke = 1) +
    ggrepel::geom_text_repel(aes(label = condition), color = "black", 
                             box.padding = 0.5) +
    scale_x_continuous(limits = c(-0.3, 6.3), breaks = seq(0, 6, 2)) +
    scale_y_continuous(limits = c(-0.3, 6.3), breaks = seq(0, 6, 2)) +
    theme_bw() +
    theme(legend.position = "none")

  return(plot)
}

hier_plot_agg_pnas <- function(df, which_efa, colors, shapes, title, lab_letter){
  plot_a <- hier_plot_fun_pnas(df, factor1 = "F1", factor2 = "F2", 
                               which_efa = which_efa) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    labs(#title = "BODY vs. HEART",
         x = "Mean BODY endorsement", 
         y = "Mean HEART endorsement")
  
  plot_b <- hier_plot_fun_pnas(df, factor1 = "F1", factor2 = "F3", 
                               which_efa = which_efa) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    labs(#title = "BODY vs. MIND",
         x = "Mean BODY endorsement", 
         y = "Mean MIND endorsement")
  
  plot_c <- hier_plot_fun_pnas(df, factor1 = "F2", factor2 = "F3", 
                               which_efa = which_efa) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    labs(#title = "HEART vs. MIND",
         x = "Mean HEART endorsement", 
         y = "Mean MIND endorsement")
  
  plot_title <- ggdraw() +
    draw_label(title)
  
  plots <- plot_grid(plot_a, plot_b, plot_c, nrow = 3,
                     labels = paste0(lab_letter, 1:3), scale = 0.95)
  
  fig_with_cap <- plot_grid(plot_title, plots, ncol = 1, 
                            rel_heights = c(0.025, 1))
  
  return(fig_with_cap)
}

app_plot_fun_pnas <- function(df, which_efa, 
                              factor_names = c("BODY", "HEART", "MIND"),
                              colors, shapes){
  
  plot <- catscore_fun_pnas(df, which_efa) %>%
    ggplot(aes(x = factor(factor, labels = factor_names), 
               y = score, color = condition)) +
    facet_wrap(~ condition, ncol = 7) +
    geom_jitter(aes(shape = condition), width = 0.25, height = 0, alpha = 0.4) +
    geom_pointrange(data = . %>% 
                      distinct(subid, condition, factor, score) %>%
                      group_by(condition, factor) %>%
                      multi_boot_standard(col = "score") %>%
                      ungroup(),
                    aes(y = mean, ymin = ci_lower, ymax = ci_upper),
                    color = "black", fatten = 1.5) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme(legend.position = "none") +
    labs(x = "Conceptual unit", y = "Mean endorsement")
  
  return(plot)
  
}
