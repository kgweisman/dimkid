# FUNCTIONS FOR PLOTTING

# colors for plotting target characters
colors02 <- c("#e41a1c", "#377eb8")
# from colorbrewer2.org "Paired"
colors09 <- colorRampPalette(c("#e31a1c", "#ff7f00", "#33a02c", 
                               "#1f78b4", "#6a3d9a"), 
                             space = "Lab")(9)
colors21 <- colorRampPalette(c("#e31a1c", "#ff7f00", "#33a02c", 
                               "#1f78b4", "#6a3d9a"),
                             space = "Lab")(21)
colorsAI <- c("#EE4D16", "#2a4166")

# function for generating heatmap of factor loadings
heatmap_fun <- function(efa, factor_names = NA){
  
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
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels))
  
  # get fa.sort() order
  order <- loadings %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    arrange(desc(factor), abs(loading)) %>%
    mutate(order = 1:length(levels(factor(loadings$capacity)))) %>%
    select(capacity, order)
  
  # get percent shared variance explained
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels)) %>%
    mutate(var_shared = paste0(factor, "\n", round(var, 2)*100, "% shared var.,"))
  
  # get percent total variance explained
  total_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Var") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels)) %>%
    mutate(var_total = paste0(round(var, 2)*100, "% total var."))
  
  # make plot
  plot <- ggplot(loadings %>% 
                   left_join(order) %>%
                   left_join(shared_var %>% select(-var)) %>%
                   left_join(total_var %>% select(-var)) %>%
                   mutate(capacity = gsub("_", " ", capacity),
                          factor = factor(factor, levels = factor_levels),
                          xlab = paste(var_shared, var_total, sep = "\n")),
                 aes(x = reorder(xlab, as.numeric(factor)), 
                     y = reorder(capacity, order), 
                     fill = loading, 
                     label = format(round(loading, 2), nsmall = 2))) +
    geom_tile(color = "black") +
    geom_text(size = 3) +
    scale_fill_distiller(limits = c(-1, 1), 
                         palette = "RdYlBu",
                         guide = guide_colorbar(barheight = 10)) +
    theme_minimal() +
    scale_x_discrete(position = "top") +
    theme(axis.title = element_blank())
  
  return(plot)
  
}

# functions for visualizing relationships between factors
relviz_fun <- function(d_scored, jit = 0.05, add_means = T, add_cor = T,
                       colors = c("#e41a1c", "#377eb8")){
  
  factor_names_plot <- levels(d_scored$factor)
  factor_names <- gsub("\\*", "", 
                       gsub("\\-", "_", 
                            gsub("\\(", "", 
                                 gsub("\\)", "", 
                                      gsub(" ", "_", factor_names_plot)))))
  n_fact <- length(factor_names)
  n_plots <- choose(n_fact, 2)
  
  d_scored <- d_scored %>%
    mutate(factor = factor(factor, labels = factor_names))
  
  add_means_fun <- function(plot, d_scored, facnames){
    
    char_means <- d_scored %>%
      mutate(factor = as.character(factor)) %>%
      filter(factor %in% facnames) %>%
      group_by(character, factor) %>%
      multi_boot_standard(col = "score") %>%
      ungroup()
    
    df_means <- full_join(char_means %>%
                            filter(factor == facnames[1]) %>%
                            rename(factor_x = factor,
                                   ci_lower_x = ci_lower,
                                   ci_upper_x = ci_upper,
                                   mean_x = mean),
                          char_means %>%
                            filter(factor == facnames[2]) %>%
                            rename(factor_y = factor,
                                   ci_lower_y = ci_lower,
                                   ci_upper_y = ci_upper,
                                   mean_y = mean))
    
    newplot <- plot +
      geom_errorbar(data = df_means,
                    aes(x = mean_x, y = NULL, 
                        ymin = ci_lower_y, ymax = ci_upper_y),
                    width = 0, color = "black", show.legend = F) +
      geom_errorbarh(data = df_means,
                     aes(x = NULL, y = mean_y, 
                         xmin = ci_lower_x, xmax = ci_upper_x),
                     height = 0, color = "black", show.legend = F) +
      geom_point(data = df_means,
                 aes(x = mean_x, y = mean_y, fill = character),
                 size = 3, stroke = 0.8, shape = 23, color = "black") +
      scale_fill_manual("Target character", values = colors)
    
    return(newplot)
  }
  
  add_cor_fun <- function(plot, d_scored, facnames){
    df_temp <- d_scored %>%
      unite(subid_char, subid, character) %>%
      select(subid_char, factor, score) %>%
      filter(factor %in% facnames) %>%
      spread(factor, score) %>%
      data.frame() %>%
      column_to_rownames("subid_char")
    
    corres <- corr.test(df_temp)
    
    r <- corres$r[1,2]
    p <- corres$p[1,2]
    p_str <- ifelse(p < 0.001, "p < 0.001",
                    paste0("p = ", format(round(p, 3), 3)))
    
    cor_str <- paste0("r = ", format(round(r, 2), nsmall = 2),
                      ", ", p_str)
    newplot <- plot +
      annotate("text", x = 0, y = 1, color = "black",
               label = cor_str, hjust = 0)
    
    return(newplot)
  }
  
  plot_fun <- function(facnames, facnames_plot){
    
    newplot <- d_scored %>%
      filter(factor %in% facnames) %>%
      spread(factor, score) %>%
      ggplot(aes_string(x = facnames[1], y = facnames[2],
                        color = "character")) +
      geom_abline(slope = 1, intercept = 0, lty = 2) +
      geom_jitter(width = jit, height = jit, alpha = 0.25) +
      scale_x_continuous(name = paste(facnames_plot[1], "score"), 
                         limits = c(0-jit, 1+jit), breaks = seq(0, 1, 0.2)) +
      scale_y_continuous(name = paste(facnames_plot[2], "score"), 
                         limits = c(0-jit, 1+jit), breaks = seq(0, 1, 0.2)) +
      scale_color_manual(name = "Target character", values = colors)
    
    if(add_means){newplot <- add_means_fun(newplot, d_scored, facnames)}
    if(add_cor){newplot <- add_cor_fun(newplot, d_scored, facnames)}
    
    return(newplot)
  }
  
  plots <- vector("list", n_plots)
  
  plot_12 <- plot_fun(facnames = factor_names[c(1, 2)],
                      facnames_plot = factor_names_plot[c(1, 2)])
  plots[[1]] <- plot_12
  
  if(n_fact > 2){
    plot_13 <- plot_fun(facnames = factor_names[c(1, 3)],
                        facnames_plot = factor_names_plot[c(1, 3)])
    plots[[2]] <- plot_13
    
    plot_23 <- plot_fun(facnames = factor_names[c(2, 3)],
                        facnames_plot = factor_names_plot[c(2, 3)])
    plots[[3]] <- plot_23
  }
  
  if(n_fact > 3){
    plot_14 <- plot_fun(facnames = factor_names[c(1, 4)],
                        facnames_plot = factor_names_plot[c(1, 4)])
    plots[[4]] <- plot_14
    
    plot_24 <- plot_fun(facnames = factor_names[c(2, 4)],
                        facnames_plot = factor_names_plot[c(2, 4)])
    plots[[5]] <- plot_24
    
    plot_34 <- plot_fun(facnames = factor_names[c(3, 4)],
                        facnames_plot = factor_names_plot[c(3, 4)])
    plots[[6]] <- plot_34
  }
  
  return(plots)
}

relviz_agegp_fun <- function(d_scored, age_groups, age_group_labels,
                             colors = c("#e41a1c", "#377eb8")){
  
  d_scored_means_BODY <- d_scored %>%
    filter(factor == "BODY") %>%
    group_by(age_group, character) %>%
    multi_boot_standard(col = "score") %>%
    ungroup() %>%
    rename(ci_lower_BODY = ci_lower, ci_upper_BODY = ci_upper, mean_BODY = mean)
  
  d_scored_means_HEART <- d_scored %>%
    filter(factor == "HEART") %>%
    group_by(age_group, character) %>%
    multi_boot_standard(col = "score") %>%
    ungroup() %>%
    rename(ci_lower_HEART = ci_lower, ci_upper_HEART = ci_upper, mean_HEART = mean)
  
  d_scored_means_MIND <- d_scored %>%
    filter(factor == "MIND") %>%
    group_by(age_group, character) %>%
    multi_boot_standard(col = "score") %>%
    ungroup() %>%
    rename(ci_lower_MIND = ci_lower, ci_upper_MIND = ci_upper, mean_MIND = mean)
  
  d_scored_means_all <- d_scored_means_BODY %>%
    full_join(d_scored_means_HEART) %>%
    full_join(d_scored_means_MIND) %>%
    mutate(age_group = factor(age_group,
                              levels = age_groups,
                              labels = age_group_labels)) %>%
    arrange(age_group) %>%
    filter(!is.na(character))
  
  plot_BH <- ggplot(d_scored_means_all,
                    aes(x = mean_BODY, y = mean_HEART, color = character)) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    ggforce::geom_link2(arrow = arrow(type = "closed", length = unit(0.5, "lines")),
                        show.legend = F) +
    geom_point(data = . %>% filter(age_group != "Adults"),
               aes(size = age_group)) +
    scale_color_manual("Target character", values = colors, na.translate = F) +
    scale_size_manual("Age group", values = c(2, 3), guide = "none") +
    scale_x_continuous("BODY score", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous("HEART score", limits = c(0, 1), breaks = seq(0, 1, 0.2))
  
  plot_BM <- ggplot(d_scored_means_all,
                    aes(x = mean_BODY, y = mean_MIND, color = character)) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    ggforce::geom_link2(arrow = arrow(type = "closed", length = unit(0.5, "lines")),
                        show.legend = F) +
    geom_point(data = . %>% filter(age_group != "Adults"),
               aes(size = age_group)) +
    scale_color_manual("Target character", values = colors, na.translate = F) +
    scale_size_manual("Age group", values = c(1, 1.75), guide = "none") +
    scale_x_continuous("BODY score", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous("MIND score", limits = c(0, 1), breaks = seq(0, 1, 0.2))
  
  plot_HM <- ggplot(d_scored_means_all,
                    aes(x = mean_HEART, y = mean_MIND, color = character)) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    ggforce::geom_link2(arrow = arrow(type = "closed", length = unit(0.5, "lines")),
                        show.legend = F) +
    geom_point(data = . %>% filter(age_group != "Adults"),
               aes(size = age_group)) +
    scale_color_manual("Target character", values = colors, na.translate = F) +
    scale_size_manual("Age group", values = c(1, 1.75), guide = "none") +
    scale_x_continuous("HEART score", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous("MIND score", limits = c(0, 1), breaks = seq(0, 1, 0.2))
  
  plots <- list(plot_BH, plot_BM, plot_HM)
  return(plots)
  
}

# function for plotting difference scores between factors
diffplot_fun <- function(df_diff, colors = colors02, wid = 0.7){
  
  charnames <- levels(df_diff$character)
  
  means_bychar <- df_diff %>%
    group_by(pair, character) %>%
    multi_boot_standard(col = "diff", na.rm = T) %>%
    ungroup()
  
  means_nochar <- df_diff %>%
    group_by(pair) %>%
    multi_boot_standard(col = "diff", na.rm = T) %>%
    ungroup() %>%
    mutate(character = "OVERALL")
  
  means_all <- full_join(means_bychar, means_nochar) %>%
    mutate(character = factor(character,
                              levels = c(charnames, "OVERALL")))
  
  colors_plus <- c(colors, "black")
  sizes_plus <- c(rep(0.5, length(charnames)), 0.8)
  
  ncolumns <- ifelse(length(colors_plus) <= 10, length(colors_plus), 8)
  
  df_diff %>%
    bind_rows(data.frame(character = "OVERALL",
                         diff = NA_integer_,
                         pair = levels(df_diff$pair))) %>%
    mutate(character = factor(character, levels = c(charnames, "OVERALL"))) %>%
    ggplot(aes(x = pair, y = diff, color = character, fill = character)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_point(alpha = 0.25, show.legend = F,
               position = position_jitterdodge(jitter.width = wid - 0.1, 
                                               dodge.width = wid)) +
    geom_pointrange(data = means_all,
                    aes(y = mean, ymin = ci_lower, ymax = ci_upper,
                        size = character),
                    position = position_dodge(width = wid),
                    color = "black", shape = 23) +
    scale_x_discrete("Pair of conceptual units") +
    scale_y_continuous("Difference in scores", limits = c(-1, 1)) +
    scale_color_manual("Target character", values = colors_plus,
                       guide = guide_legend(direction = "horizontal", 
                                            ncol = ncolumns)) +
    scale_fill_manual("Target character", values = colors_plus,
                      guide = guide_legend(direction = "horizontal", 
                                           ncol = ncolumns)) +
    scale_size_manual("Target character", values = sizes_plus,
                      guide = guide_legend(direction = "horizontal", 
                                           ncol = ncolumns)) +
    theme(legend.position = "bottom")
}

# function for plotting factor scores by factor, target
scoresplot_fun <- function(efa, 
                           target = c("all", "beetle", "robot"), 
                           highlight = "none", factor_names = NA){
  
  # generate list of targets
  if(target == "all"){
    target_list <- c("beetle", "robot")
  } else {
    target_list <- target
  }
  
  # generate list of targets to highlight
  if(highlight == "none"){
    highlight_list <- c()
  } else {
    highlight_list <- highlight
  }
  
  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
  }
  
  # make usable dataframe
  df <- efa$scores[] %>%
    data.frame() %>%
    rownames_to_column("subid") %>%
    mutate(ResponseId = gsub("_.*$", "", subid),
           target = gsub("^.*_", "", subid)) %>%
    filter(target %in% target_list) %>%
    select(-subid) %>%
    gather(factor, score, -c(ResponseId, target)) %>%
    mutate(highlight = factor(ifelse(target %in% highlight_list,
                                     "highlight", "no_highlight"),
                              levels = c("no_highlight", "highlight")),
           factor = as.character(factor(factor, labels = factor_names)))
  
  # get bootstrapped means
  df_boot <- df %>%
    group_by(target, factor) %>%
    multi_boot_standard("score", na.rm = T) %>%
    ungroup() %>%
    mutate(highlight = factor(ifelse(target %in% highlight_list,
                                     "highlight", "no_highlight"),
                              levels = c("no_highlight", "highlight")))
  
  # get first items for subtitle
  first_items <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names))) %>%
    group_by(factor) %>%
    top_n(3, abs(loading)) %>%
    mutate(capacity = gsub("_", " ", capacity),
           cap_list = str_c(capacity, collapse = ", "),
           cap_list = paste0(cap_list, "...")) %>%
    ungroup() %>%
    select(-capacity, -loading) %>%
    distinct() %>%
    arrange(factor)
  
  # get percent shared variance explained
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           var = paste0(round(var, 2)*100, "% shared variance"))
  
  subtitle <- c()
  for(i in 1:nrow(first_items)){
    subtitle <- paste0(subtitle,
                       first_items[i,1], 
                       " (", shared_var[i,2], "): ",
                       first_items[i,2], 
                       "\n")
  }
  subtitle <- gsub("\\n$", "", subtitle)
  
  # get colors for lines
  line_colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854",
                   "#ffd92f", "#e5c494", "#b3b3b3") # Set2 from colorbrewer
  line_colors <- line_colors[1:nrow(shared_var)]
  
  # make plot
  plot <- ggplot(df, aes(x = target, y = score, fill = factor)) +
    facet_grid(cols = vars(factor)) +
    geom_hline(yintercept = 0, lty = 2, color = "darkgray") +
    geom_point(aes(color = factor), alpha = 0.25,
               position = position_jitter(width = 0.25, height = 0)) +
    # geom_path(aes(color = factor, group = ResponseId), alpha = 0.1) +
    # geom_path(data = df_boot, 
    #           aes(y = mean, group = factor), 
    #           lty = 2, color = "black") +
    geom_errorbar(data = df_boot,
                  aes(y = mean, ymin = ci_lower, ymax = ci_upper, 
                      color = highlight),
                  width = 0) +
    geom_point(data = df_boot,
               aes(y = mean, 
                   color = highlight, size = highlight)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_manual(values = c(line_colors, "black", "#984ea3")) +
    scale_size_manual(values = c(0.75, 2)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(x = "target character",
         y = "factor score",
         subtitle = subtitle,
         caption = "Error bars are bootstrapped 95% confidence intervals") +
    guides(fill = "none", color = "none", size = "none")
  # guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))
  
  return(plot)
  
}

# function for plotting individual item means by factor, target
itemsplot_fun <- function(efa,
                          target = c("all", "beetle", "robot")){
  
  # generate list of targets
  target_list <- case_when(
    target == "all" ~ c("beetle", "robot"),
    TRUE ~ target
  )
  
  # make usable dataframe
  df <- d_efa %>%
    rownames_to_column("subid") %>%
    mutate(ResponseId = gsub("_.*$", "", subid),
           target = gsub("^.*_", "", subid)) %>%
    filter(target %in% target_list) %>%
    select(-subid) %>%
    gather(capacity, response, -c(ResponseId, target))
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity)
  
  # get fa.sort() order
  order <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    arrange(desc(factor), abs(loading)) %>%
    mutate(order = 1:length(levels(factor(loadings$capacity)))) %>%
    select(factor, capacity, order)
  
  # add order to df
  df <- df %>% left_join(order)
  
  # get bootstrapped means
  df_boot <- df %>%
    group_by(target, factor, capacity, order) %>%
    multi_boot_standard("response", na.rm = T) %>%
    ungroup() %>%
    mutate(capacity = gsub("_", " ", capacity))
  
  # make plot
  plot <- ggplot(df %>% 
                   left_join(order) %>%
                   mutate(capacity = gsub("_", " ", capacity)),
                 aes(x = response, 
                     y = reorder(capacity, order), 
                     color = factor)) +
    facet_grid(factor ~ target, scales = "free", space = "free") +
    geom_point(alpha = 0.01) +
    geom_errorbarh(data = df_boot, 
                   aes(xmin = ci_lower, xmax = ci_upper, x = NULL),
                   color = "black", height = 0) +
    geom_point(data = df_boot,
               aes(x = mean),
               color = "black", size = 2) +
    theme_bw() +
    labs(x = "response", y = "",
         subtitle = "Error bars are bootstrapped 95% confidence intervals") +
    guides(color = "none")
  # guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
  
  return(plot)
  
}

# function for plotting interfactor correlations
IFcor_plot_fun <- function(efa, factor_names = NA, remove_dup = F){
  
  efa_cor <- IFcor_fun(efa = efa, factor_names = factor_names,
                       remove_dup = remove_dup) %>%
    mutate_at(vars(factor1, factor2), 
              funs(gsub("vs\\.", "vs", .))) %>%
    mutate_at(vars(factor1, factor2), 
              funs(gsub(".vs.", " vs ", .))) %>%
    mutate_at(vars(factor1, factor2), 
              funs(gsub("Factor\\.", "Factor ", .))) %>%
    mutate_at(vars(factor1, factor2), 
              funs(gsub("BODY.HEART", "BODY-HEART ", .))) %>%
    mutate_at(vars(factor1, factor2), 
              funs(gsub("MIND.HEART", "MIND-HEART ", .))) %>%
    mutate_at(vars(factor1, factor2),
              funs(gsub("\\.\\.", " (", .))) %>%
    mutate_at(vars(factor1, factor2),
              funs(gsub("\\.$", ")", .))) %>%
    mutate_at(vars(factor1, factor2),
              funs(gsub(" \\(", "\n\\(", .))) %>%
    mutate_at(vars(factor1, factor2), 
              funs(gsub("  ", " ", .))) %>%
    mutate(cor = ifelse(factor1 == factor2, NA, cor))
  
  plot <- ggplot(efa_cor,
                 aes(x = factor1, y = reorder(factor2, desc(factor2)), 
                     fill = cor,
                     label = ifelse(is.na(cor), "",
                                    format(round(cor, 2), nsmall = 2)))) +
    geom_tile(color = "black") +
    geom_text(size = 3) +
    scale_fill_distiller(limits = c(-1, 1),
                         palette = "PRGn",
                         guide = guide_colorbar(barheight = 6), 
                         na.value = "white") +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(plot)
  
}

# function for adding glm line to regresion plots
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

# function for making multi-part plots BY AGE of scores of target characters
character_multiplot_age <- function(df_scored, show_anim_by_subj = F,
                                    age_levels, age_labels,
                                    jitter_wid = 0.4,
                                    dodge_wid = 0.5,
                                    bin_width = 1/12,
                                    plot_marg_upper = NA,
                                    axis_height = NA,
                                    plot_labels = "AUTO"){
  
  # setup -----
  levels_characters <- levels(df_scored$character) %>% 
    as.character() %>%
    gsub("persistent.*$", "PVS", .)
  maxlength_characters <- max(nchar(levels_characters))
  levels_characters_pad <- str_pad(levels_characters,
                                   width = maxlength_characters,
                                   pad = " ")
  n_characters <- length(levels(df_scored$character))
  if(n_characters == 2){
    colors <- colors02
  } else if(n_characters == 9) {
    colors <- colors09
  } else {
    colors <- colors21
  }
  
  df_scored <- df_scored %>%
    mutate(age_group = factor(age_group, levels = age_levels, 
                              labels = age_labels))

  pos_jd <- position_jitterdodge(jitter.width = jitter_wid, 
                                 dodge.width = dodge_wid)
  pos_d <- position_dodge(width = dodge_wid)
  
  # target characters -----
  plotA <- df_scored %>%
    mutate(character = as.character(character),
           character = gsub("persistent.*$", "PVS", character),
           character = factor(character,
                              levels = levels_characters,
                              labels = levels_characters_pad)) %>%
    ggplot(aes(x = character, y = score, color = character)) +
    facet_grid(factor ~ age_group) +
    geom_point(position = pos_jd, alpha = 0.2, show.legend = F) +
    geom_pointrange(data = . %>%
                      group_by(age_group, character, factor) %>%
                      multi_boot_standard(col = "score", na.rm = T) %>%
                      ungroup(),
                    aes(y = mean, ymin = ci_lower, ymax = ci_upper),
                    color = "black", shape = "diamond", fatten = 6,
                    position = pos_d, show.legend = F) +
    scale_color_manual(values = colors, guide = "none") +
    labs(title = "Scores by age group and target character", 
         x = "Target character", y = "Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "none")
  
  # animacy status -----
  if(n_characters > 2){
    plotB <- df_scored %>%
      left_join(anim_lookup) %>%
      mutate(anim_inan = as.character(anim_inan),
             anim_inan = factor(
               anim_inan,
               levels = levels(anim_lookup$anim_inan),
               labels = str_pad(levels(anim_lookup$anim_inan),
                                width = maxlength_characters,
                                pad = " "))) %>%
      ggplot(aes(x = anim_inan, y = score, color = anim_inan)) +
      facet_grid(factor ~ age_group)
    
    if(show_anim_by_subj){
      plotB <- plotB + 
        geom_point(position = pos_jd, alpha = 0.25, show.legend = F) +
        geom_pointrange(data = . %>%
                          group_by(age_group, anim_inan, factor) %>%
                          multi_boot_standard(col = "score", na.rm = T) %>%
                          ungroup(),
                        aes(y = mean, ymin = ci_lower, ymax = ci_upper),
                        color = "black", shape = "diamond", 
                        position = pos_d, fatten = 6, show.legend = F)
    } else {
      plotB <- plotB +
        geom_pointrange(data = . %>%
                          group_by(age_group, anim_inan, factor) %>%
                          multi_boot_standard(col = "score", na.rm = T) %>%
                          ungroup(),
                        aes(y = mean, ymin = ci_lower, ymax = ci_upper),
                        shape = "diamond", position = pos_d, 
                        show.legend = F, fatten = 6)
    }
    
    plotB <- plotB +
      scale_color_manual(values = colorsAI) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = "Scores by animacy status", 
           x = "Animacy status", y = "Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  
  # histogram -----
  plotC <- df_scored %>%
    mutate(character = as.character(character),
           character = gsub("persistent.*$", "PVS", character),
           character = factor(character,
                              levels = levels_characters,
                              labels = levels_characters_pad)) %>%
    # group_by(age_group) %>%
    ggplot(aes(x = score, fill = character)) +
    facet_grid(factor ~ age_group) +
    geom_histogram(aes(y = stat(density) * bin_width/n_characters),
                   binwidth = bin_width, show.legend = F) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks = seq(0, 1, 0.25),
                       labels = str_pad(format(seq(0, 1, 0.25), nsmall = 2),
                                        width = maxlength_characters,
                                        pad = " ")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Distribution of scores",
         x = "Score", y = "Percent of responses (by age group)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  # put them all together -----
  if(n_characters > 2) {
    # hard-coded for fig.width = 6, fig.asp = 0.5
    if(is.na(plot_marg_upper)) {plot_marg_upper <- -32}
    if(is.na(axis_height)) {axis_height <- 0.17}
  }
  
  if(n_characters == 2){
    # hard-coded for fig.width = 3, fig.asp = 1
    if(is.na(plot_marg_upper)) {plot_marg_upper <- -32}
    if(is.na(axis_height)) {axis_height <- 0.085}
  }
  
  plot_margins <- unit(c(plot_marg_upper, 5.5, 5.5, 5.5), "points")
  if(n_characters == 21){
    relative_wid <- c(2, 2, 1)
  } else if(n_characters == 9) {
    relative_wid <- c(3, 2, 2)
  }
  
  if(n_characters > 2){
    allplots <- plot_grid(
      plotA + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.ticks.x = element_blank()), 
      plotB + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.ticks.x = element_blank()), 
      plotC + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.ticks.x = element_blank()),
      ncol = 3, rel_widths = relative_wid,
      labels = plot_labels)
    all_axis_xs <- plot_grid(
      gtable_filter(ggplotGrob(
        plotA + theme(plot.margin = plot_margins)), 
        'axis-b|xlab', trim=F),
      gtable_filter(ggplotGrob(
        plotB + theme(plot.margin = plot_margins)), 
        'axis-b|xlab', trim=F),
      gtable_filter(ggplotGrob(
        plotC + theme(plot.margin = plot_margins)), 
        'axis-b|xlab', trim=F),
      ncol = 3, rel_widths = relative_wid,
      align = "h", axis = "t")
    allplots_plus <- plot_grid(allplots, all_axis_xs,
                               ncol = 1, rel_heights = c(1, axis_height))
  } else {
    allplots <- plot_grid(
      plotA + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.ticks.x = element_blank()), 
      plotC + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.ticks.x = element_blank()),
      ncol = 2, rel_widths = c(1, 1),
      labels = plot_labels)
    all_axis_xs <- plot_grid(
      gtable_filter(ggplotGrob(
        plotA + theme(plot.margin = plot_margins)), 
        'axis-b|xlab', trim=F),
      gtable_filter(ggplotGrob(
        plotC + theme(plot.margin = plot_margins)), 
        'axis-b|xlab', trim=F),
      ncol = 2, rel_widths = c(1, 1),
      align = "h", axis = "t")
    allplots_plus <- plot_grid(allplots, all_axis_xs,
                               ncol = 1, rel_heights = c(1, axis_height))
  }
  
  return(allplots_plus)
  
}

# function for showing development in scores by target character
character_devplot <- function(df_scored_ad, df_scored_ch, df_age,
                              jit_height = 0.015,
                              dodge_width = NA){
  
  min_age <- floor(min(df_age$age, na.rm = T))
  max_age <- ceiling(max(df_age$age, na.rm = T))
  if(max_age < 7){
    adult_age <- 6
    max_age <- 5
  } else {
    adult_age <- max_age + 1
  }
  missing_age <- df_age %>%
    distinct(subid, age) %>%
    filter(is.na(age)) %>%
    nrow()
  
  n_characters <- length(levels(df_scored_ad$character))
  if(n_characters == 2){
    colors <- colors02
    dodge_width <- 0.5
  } else if(n_characters == 9){
    colors <- colors09
    dodge_width <- 1.5
  } else {
    colors <- colors21
    dodge_width <- 1.5
  }
  
  plot <- df_scored_ch %>%
    left_join(df_age %>% distinct(subid, age)) %>%
    arrange(desc(character)) %>%
    ggplot(aes(x = age, y = score, color = character, fill = character)) +
    facet_grid(. ~ factor) +
    geom_jitter(width = 0, height = jit_height, alpha = 0.2) +
    geom_pointrange(data = df_scored_ad %>%
                      group_by(factor, character) %>%
                      multi_boot_standard(col = "score") %>%
                      ungroup(),
                    aes(x = adult_age, 
                        y = mean, ymin = ci_lower, ymax = ci_upper),
                    position = position_dodge(width = dodge_width),
                    shape = "diamond", fatten = 6, show.legend = F) +
    geom_smooth(method = "lm", alpha = 0.2, show.legend = F) +
    scale_color_manual("Target character", values = colors,
                       guide = guide_legend(
                         direction = "horizontal",
                         ncol = length(colors),
                         override.aes = list(alpha = 1))) +
    scale_fill_manual("Target character", values = colors) +
    scale_x_continuous("Age", breaks = c(min_age:adult_age), 
                       labels = c(paste0(min_age:max_age, "y"), "Adults")) +
    scale_y_continuous("Score", limits = c(0 - jit_height, 1 + jit_height)) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  if(missing_age > 0){
    plot <- plot + 
      labs(subtitle = paste("Note: missing exact age for", 
                            missing_age, "children"))
  }
  
  return(plot)
}

# function for scatter plots of attributions
scatter_plot_fun <- function(df_scores = scores_all, 
                             df_scores_mb =  score_all_mb,
                             lookup = anim_lookup,
                             which_age_group, chosen_alpha = 0.4){
  plot <- df_scores %>%
    left_join(lookup) %>%
    filter(age_group == which_age_group) %>%
    spread(factor, score) %>%
    arrange(HEART) %>% # put highest HEART on top
    ggplot(aes(x = BODY, y = MIND)) +
    facet_grid(anim_inan ~ study) +
    geom_jitter(aes(fill = HEART),
                width = 0.05, height = 0.05, alpha = chosen_alpha, size = 3,
                color = "black", shape = 21) +
    geom_errorbarh(data = df_scores_mb %>% filter(age_group == which_age_group),
                   aes(xmin = BODY_ci_lower, xmax = BODY_ci_upper),
                   height = 0, size = 1) +
    geom_errorbar(data = df_scores_mb %>% filter(age_group == which_age_group),
                  aes(ymin = MIND_ci_lower, ymax = MIND_ci_upper),
                  width = 0, size = 1) +
    geom_point(data = df_scores_mb %>% filter(age_group == which_age_group),
               aes(fill = HEART), shape = 23, size = 3, stroke = 1.5) +
    geom_label(data = scatter_key %>% filter(age_group == which_age_group),
               aes(x = NULL, y = NULL, label = character_list),
               x = 1, y = 0, hjust = 1, size = 3, alpha = 0.7, label.size = 0) +
    scale_fill_distiller(palette = "Spectral", 
                         guide = guide_colorbar(barwidth = 20, barheight = 1)) +
    labs(title = which_age_group,
         x = "BODY score", y = "MIND score", fill = "HEART score") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  return(plot)
}
