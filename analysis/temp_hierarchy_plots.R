# comment on:
# - strength of relationship (correlation)
# - asymmetry around the x = y line
# - which characters are placed where

hier_plot_fun <- function(df, factor1, factor2, which_efa, kinda = 1){
  
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
  
  plot <- df %>%
    filter(capacity %in% loadings$capacity) %>%
    select(-factor) %>%
    left_join(loadings) %>%
    group_by(age_group, subid, character, factor) %>%
    mutate(response_num = case_when(response == "kinda" ~ kinda,
                                    TRUE ~ response_num)) %>%
    summarise(score = sum(response_num, na.rm = T)) %>% #/n()) %>%
    filter(factor %in% c(factor1, factor2)) %>%
    spread(factor, score) %>%
    ggplot(aes_string(x = factor1, y = factor2, color = "character", 
                      fill = "character", shape = "character")) +
    facet_grid(~ age_group) +
    geom_abline(lty = 2) +
    geom_jitter(width = 0.25, height = 0.25, alpha = 0.75) +
    scale_x_continuous(limits = c(-0.26, how_many_cap + 0.26),
                       breaks = seq(0, how_many_cap, 2)) +
    scale_y_continuous(limits = c(-0.26, how_many_cap + 0.26),
                       breaks = seq(0, how_many_cap, 2)) +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))
  
  return(plot)
}

hier_plot_fun2 <- function(df, factor1, factor2, which_efa, kinda = 1){
  
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
  
  means <- df %>%
    filter(capacity %in% loadings$capacity) %>%
    select(-factor) %>%
    left_join(loadings) %>%
    group_by(age_group, subid, character, factor) %>%
    mutate(response_num = case_when(response == "kinda" ~ kinda,
                                    TRUE ~ response_num),
           score = sum(response_num, na.rm = T)) %>%
    ungroup() %>%
    distinct(subid, age_group, character, factor, score) %>%
    group_by(age_group, character, factor) %>%
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
               color = character, fill = character, shape = character)) +
    facet_grid(~age_group) +
    geom_abline(lty = 2) +
    geom_errorbarh(aes(xmin = ci_lower_x, xmax = ci_upper_x), height = 0) +
    geom_pointrange(aes(ymin = ci_lower_y, ymax = ci_upper_y)) +
    scale_x_continuous(limits = c(0, how_many_cap),
                       breaks = seq(0, how_many_cap, 2)) +
    scale_y_continuous(limits = c(0, how_many_cap),
                       breaks = seq(0, how_many_cap, 2)) +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))
  
  return(plot)
}

# study 1 -----

## adults' factor space -----

# to determine labels for factors
# fa.sort(efa_3_d1_ad)

### 1 point per participant -----

hier_plot_fun(d1_all, "F2", "F1", efa_3_d1_ad) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of BODY vs. HEART capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "BODY (Factor 2)", y = "HEART (Factor 1)")

hier_plot_fun(d1_all, "F2", "F3", efa_3_d1_ad) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of BODY vs. MIND capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "BODY (Factor 2)", y = "MIND (Factor 3)")

hier_plot_fun(d1_all, "F1", "F3", efa_3_d1_ad) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of HEART vs. MIND capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "HEART (Factor 1)", y = "MIND (Factor 3)")

### 1 point per character -----

hier_plot_fun2(d1_all, "F2", "F1", efa_3_d1_ad) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of BODY vs. HEART capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "BODY (Factor 2)", y = "HEART (Factor 1)")

hier_plot_fun2(d1_all, "F2", "F3", efa_3_d1_ad) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of BODY vs. MIND capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "BODY (Factor 2)", y = "MIND (Factor 3)")

hier_plot_fun2(d1_all, "F1", "F3", efa_3_d1_ad) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of HEART vs. MIND capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "HEART (Factor 1)", y = "MIND (Factor 3)")

## 7-9yo children's factor space -----

# to determine labels for factors
# fa.sort(efa_3_d1_79)

### 1 point per participant -----

hier_plot_fun(d1_all, "F2", "F1", efa_3_d1_79) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of BODY vs. HEART capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "BODY (Factor 2)", y = "HEART (Factor 1)")

hier_plot_fun(d1_all, "F2", "F3", efa_3_d1_79) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of BODY vs. MIND capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "BODY (Factor 2)", y = "MIND (Factor 3)")

hier_plot_fun(d1_all, "F1", "F3", efa_3_d1_79) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of HEART vs. MIND capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "HEART (Factor 1)", y = "MIND (Factor 3)")

### 1 point per character -----

hier_plot_fun2(d1_all, "F2", "F1", efa_3_d1_79) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of BODY vs. HEART capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "BODY (Factor 2)", y = "HEART (Factor 1)")

hier_plot_fun2(d1_all, "F2", "F3", efa_3_d1_79) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of BODY vs. MIND capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "BODY (Factor 2)", y = "MIND (Factor 3)")

hier_plot_fun2(d1_all, "F1", "F3", efa_3_d1_79) +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
  scale_shape_manual(values = c(21, 22)) +
  labs(title = "Study 1: Endorsements of HEART vs. MIND capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "HEART (Factor 1)", y = "MIND (Factor 3)")

## adults for adults, children for children

### 1 point per particpiant -----

plot_grid(hier_plot_fun(d1_all %>% filter(age_group == "children"),
                        "F2", "F1", efa_3_d1_79) +
            geom_smooth(method = "lm", aes(group = age_group), 
                        color = "black", show.legend = F) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            theme(legend.position = "none") +
            labs(#title = "Study 1: Endorsements of BODY vs. HEART capacities",
                 #subtitle = "Defining factors by age group",
                 x = "BODY (Factor 2)", y = "HEART (Factor 1)"),
          hier_plot_fun(d1_all %>% filter(age_group == "adults"),
                        "F2", "F1", efa_3_d1_ad) +
            geom_smooth(method = "lm", aes(group = age_group), 
                        color = "black", show.legend = F) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            labs(#title = "Study 1: Endorsements of BODY vs. HEART capacities",
                 #subtitle = "Defining factors by age group",
                 x = "BODY (Factor 2)", y = "HEART (Factor 1)"),
          rel_widths = c(1, 1.4))

plot_grid(hier_plot_fun(d1_all %>% filter(age_group == "children"),
                        "F2", "F3", efa_3_d1_79) +
            geom_smooth(method = "lm", aes(group = age_group), 
                        color = "black", show.legend = F) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            theme(legend.position = "none") +
            labs(#title = "Study 1: Endorsements of BODY vs. MIND capacities",
              #subtitle = "Defining factors by age group",
              x = "BODY (Factor 2)", y = "MIND (Factor 3)"),
          hier_plot_fun(d1_all %>% filter(age_group == "adults"),
                        "F2", "F3", efa_3_d1_ad) +
            geom_smooth(method = "lm", aes(group = age_group), 
                        color = "black", show.legend = F) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            labs(#title = "Study 1: Endorsements of BODY vs. MIND capacities",
              #subtitle = "Defining factors by age group",
              x = "BODY (Factor 2)", y = "MIND (Factor 3)"),
          rel_widths = c(1, 1.4))

plot_grid(hier_plot_fun(d1_all %>% filter(age_group == "children"),
                        "F1", "F3", efa_3_d1_79) +
            geom_smooth(method = "lm", aes(group = age_group), 
                        color = "black", show.legend = F) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            theme(legend.position = "none") +
            labs(#title = "Study 1: Endorsements of HEART vs. MIND capacities",
              #subtitle = "Defining factors by age group",
              x = "HEART (Factor 1)", y = "MIND (Factor 3)"),
          hier_plot_fun(d1_all %>% filter(age_group == "adults"),
                        "F1", "F3", efa_3_d1_ad) +
            geom_smooth(method = "lm", aes(group = age_group), 
                        color = "black", show.legend = F) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            labs(#title = "Study 1: Endorsements of HEART vs. MIND capacities",
              #subtitle = "Defining factors by age group",
              x = "HEART (Factor 1)", y = "MIND (Factor 3)"),
          rel_widths = c(1, 1.4))

### 1 point per character -----

plot_grid(hier_plot_fun2(d1_all %>% filter(age_group == "children"),
                        "F2", "F1", efa_3_d1_79) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            theme(legend.position = "none") +
            labs(#title = "Study 1: Endorsements of BODY vs. HEART capacities",
              #subtitle = "Defining factors by age group",
              x = "BODY (Factor 2)", y = "HEART (Factor 1)"),
          hier_plot_fun2(d1_all %>% filter(age_group == "adults"),
                        "F2", "F1", efa_3_d1_ad) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            labs(#title = "Study 1: Endorsements of BODY vs. HEART capacities",
              #subtitle = "Defining factors by age group",
              x = "BODY (Factor 2)", y = "HEART (Factor 1)"),
          rel_widths = c(1, 1.4))

plot_grid(hier_plot_fun2(d1_all %>% filter(age_group == "children"),
                        "F2", "F3", efa_3_d1_79) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            theme(legend.position = "none") +
            labs(#title = "Study 1: Endorsements of BODY vs. MIND capacities",
              #subtitle = "Defining factors by age group",
              x = "BODY (Factor 2)", y = "MIND (Factor 3)"),
          hier_plot_fun2(d1_all %>% filter(age_group == "adults"),
                        "F2", "F3", efa_3_d1_ad) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            labs(#title = "Study 1: Endorsements of BODY vs. MIND capacities",
              #subtitle = "Defining factors by age group",
              x = "BODY (Factor 2)", y = "MIND (Factor 3)"),
          rel_widths = c(1, 1.4))

plot_grid(hier_plot_fun2(d1_all %>% filter(age_group == "children"),
                        "F1", "F3", efa_3_d1_79) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            theme(legend.position = "none") +
            labs(#title = "Study 1: Endorsements of HEART vs. MIND capacities",
              #subtitle = "Defining factors by age group",
              x = "HEART (Factor 1)", y = "MIND (Factor 3)"),
          hier_plot_fun2(d1_all %>% filter(age_group == "adults"),
                        "F1", "F3", efa_3_d1_ad) +
            scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_fill_manual(values = c("#fb9a99", "#1f78b4")) +
            scale_shape_manual(values = c(21, 22)) +
            labs(#title = "Study 1: Endorsements of HEART vs. MIND capacities",
              #subtitle = "Defining factors by age group",
              x = "HEART (Factor 1)", y = "MIND (Factor 3)"),
          rel_widths = c(1, 1.4))



# study 2 -----

## adults' factor space -----

# to determine labels for factors
# fa.sort(efa_3_d2_ad)

### 1 point per participant -----

hier_plot_fun(d2_all, "F1", "F2", efa_3_d2_ad) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY vs. HEART capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "BODY (Factor 1)", y = "HEART (Factor 2)")

hier_plot_fun(d2_all, "F1", "F3", efa_3_d2_ad) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY vs. MIND capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "BODY (Factor 1)", y = "MIND (Factor 3)")

hier_plot_fun(d2_all, "F2", "F3", efa_3_d2_ad) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of HEART vs. MIND capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "HEART (Factor 2)", y = "MIND (Factor 3)")

### 1 point per character -----

hier_plot_fun2(d2_all, "F1", "F2", efa_3_d2_ad) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY vs. HEART capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "BODY (Factor 1)", y = "HEART (Factor 2)")

hier_plot_fun2(d2_all, "F1", "F3", efa_3_d2_ad) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY vs. MIND capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "BODY (Factor 1)", y = "MIND (Factor 3)")

hier_plot_fun2(d2_all, "F2", "F3", efa_3_d2_ad) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of HEART vs. MIND capacities",
       subtitle = "Defining factors by adults' 3-factor EFA solution",
       x = "HEART (Factor 2)", y = "MIND (Factor 3)")

## 7-9yo children's factor space -----

# to determine labels for factors
# fa.sort(efa_3_d2_79)

### 1 point per participant -----

hier_plot_fun(d2_all, "F1", "F2", efa_3_d2_79) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY vs. HEART capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "BODY (Factor 1)", y = "HEART (Factor 2)")

hier_plot_fun(d2_all, "F1", "F3", efa_3_d2_79) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY vs. MIND capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "BODY (Factor 1)", y = "MIND (Factor 3)")

hier_plot_fun(d2_all, "F2", "F3", efa_3_d2_79) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of HEART vs. MIND capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "HEART (Factor 2)", y = "MIND (Factor 3)")

### 1 point per character -----

hier_plot_fun2(d2_all, "F1", "F2", efa_3_d2_79) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY vs. HEART capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "BODY (Factor 1)", y = "HEART (Factor 2)")

hier_plot_fun2(d2_all, "F1", "F3", efa_3_d2_79) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY vs. MIND capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "BODY (Factor 1)", y = "MIND (Factor 3)")

hier_plot_fun2(d2_all, "F2", "F3", efa_3_d2_79) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of HEART vs. MIND capacities",
       subtitle = "Defining factors by 7- to 9-y-old children's 3-factor EFA solution",
       x = "HEART (Factor 2)", y = "MIND (Factor 3)")

## 4-6yo children's factor space (3-factor solution) -----

# to determine labels for factors
# fa.sort(efa_3_d2_46)

### 1 point per participant -----

hier_plot_fun(d2_all, "F1", "F3", efa_3_d2_46) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of NEGATIVE vs. POSITIVE capacities",
       subtitle = "Defining factors by 4- to 6-y-old children's 3-factor EFA solution",
       x = "NEGATIVE (Factor 1)", y = "POSITIVE (Factor 3)")

hier_plot_fun(d2_all, "F1", "F2", efa_3_d2_46) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of NEGATIVE vs. MIND capacities",
       subtitle = "Defining factors by 4- to 6-y-old children's 3-factor EFA solution",
       x = "NEGATIVE (Factor 1)", y = "MIND (Factor 2)")

hier_plot_fun(d2_all, "F3", "F2", efa_3_d2_46) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of POSITIVE vs. MIND capacities",
       subtitle = "Defining factors by 4- to 6-y-old children's 3-factor EFA solution",
       x = "POSITIVE (Factor 3)", y = "MIND (Factor 2)")

### 1 point per character -----

hier_plot_fun2(d2_all, "F1", "F3", efa_3_d2_46) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of NEGATIVE vs. POSITIVE capacities",
       subtitle = "Defining factors by 4- to 6-y-old children's 3-factor EFA solution",
       x = "NEGATIVE (Factor 1)", y = "POSITIVE (Factor 3)")

hier_plot_fun2(d2_all, "F1", "F2", efa_3_d2_46) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of NEGATIVE vs. MIND capacities",
       subtitle = "Defining factors by 4- to 6-y-old children's 3-factor EFA solution",
       x = "NEGATIVE (Factor 1)", y = "MIND (Factor 2)")

hier_plot_fun2(d2_all, "F3", "F2", efa_3_d2_46) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of POSITIVE vs. MIND capacities",
       subtitle = "Defining factors by 4- to 6-y-old children's 3-factor EFA solution",
       x = "POSITIVE (Factor 3)", y = "MIND (Factor 2)")

## 4-6yo children's factor space (2-factor solution) -----

# to determine labels for factors
# fa.sort(efa_2_d2_46)

### 1 point per participant -----

hier_plot_fun(d2_all, "F1", "F2", efa_2_d2_46) +
  geom_smooth(method = "lm", aes(group = age_group), 
              color = "black", show.legend = F) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY-HEART vs. MIND capacities",
       subtitle = "Defining factors by 4- to 6-y-old children's 2-factor EFA solution",
       x = "BODY-HEART (Factor 1)", y = "MIND (Factor 2)")

### 1 point per character -----

hier_plot_fun2(d2_all, "F1", "F2", efa_2_d2_46) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = rep(19, 9)) +
  labs(title = "Study 2: Endorsements of BODY-HEART vs. MIND capacities",
       subtitle = "Defining factors by 4- to 6-y-old children's 2-factor EFA solution",
       x = "BODY-HEART (Factor 1)", y = "MIND (Factor 2)")
