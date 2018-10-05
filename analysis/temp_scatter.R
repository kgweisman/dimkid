char_plot_fun <- function(efa_scores, 
                          factor_names = NA,
                          group = "character"){

  if(is.na(factor_names)){
    df_boot <- efa_scores %>%
      gather(factor, score, starts_with("F")) %>%
      group_by_(.groups = group) %>%
      group_by(age_group, factor, add = TRUE) %>%
      multi_boot_standard(col = "score") %>%
      ungroup() %>%
      gather(key, val, c(ci_lower, ci_upper, mean)) %>%
      spread(factor, val)
    
    plotA <- ggplot(data = efa_scores, 
                    aes_string(x = "F1", y = "F2", 
                               color = group, fill = group, shape = "age_group"))
    plotB <- ggplot(data = efa_scores, 
                    aes_string(x = "F1", y = "F3", 
                               color = group, fill = group, shape = "age_group"))
    plotC <- ggplot(data = efa_scores, 
                    aes_string(x = "F2", y = "F3", 
                               color = group, fill = group, shape = "age_group"))
    
  } else {
    df <- efa_scores %>%
      select(age_group, subid, character, factor_name, score) %>%
      spread(factor_name, score)
    
    df_boot <- efa_scores %>%
      select(age_group, subid, character, factor_name, score) %>%
      group_by_(.groups = group) %>%
      group_by(age_group, factor_name, add = TRUE) %>%
      multi_boot_standard(col = "score") %>%
      ungroup() %>%
      gather(key, val, c(ci_lower, ci_upper, mean)) %>%
      spread(factor_name, val)
    
    plotA <- ggplot(data = df, 
                    aes_string(x = "BODY", y = "HEART",
                               color = group, fill = group, shape = "age_group"))
    plotB <- ggplot(data = df, 
                    aes_string(x = "BODY", y = "MIND",
                               color = group, fill = group, shape = "age_group"))
    plotC <- ggplot(data = df, 
                    aes_string(x = "HEART", y = "MIND",
                               color = group, fill = group, shape = "age_group"))
  }
  
  plotA <- plotA +
    # facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    theme(legend.position = "none") +
    guides(color = guide_legend(override.aes = list(shape = 21)))

  plotB <- plotB +
    # facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    theme(legend.position = "none") +
    guides(color = guide_legend(override.aes = list(shape = 21)))
  
  plotC <- plotC +
    # facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    guides(color = guide_legend(override.aes = list(shape = 21)))
  
  if(length(levels(efa_scores$character)) == 2){
    plotA <- plotA + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))

    plotB <- plotB + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))
  
    plotC <- plotC + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))
  } else {
    plotA <- plotA + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
    
    plotB <- plotB + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
    
    plotC <- plotC + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
  }
  
  plots <- plot_grid(plotA, plotB, plotC, nrow = 1, 
                     rel_widths = c(1, 1, 1.3), labels = "AUTO")
  
  return(plots)  
}

char_plot_fun2 <- function(efa_scores,
                           factor_names = NA,
                           group = "character"){
  
  if(is.na(factor_names)){
    df_boot <- efa_scores %>%
      gather(factor, score, starts_with("F")) %>%
      group_by_(.groups = group) %>%
      group_by(age_group, factor, add = TRUE) %>%
      multi_boot_standard(col = "score") %>%
      ungroup() %>%
      gather(key, val, c(ci_lower, ci_upper, mean)) %>%
      spread(factor, val)
    
    plotA <- ggplot(data = efa_scores, 
                    aes_string(x = "F1", y = "F2", 
                               color = group, fill = group, shape = "age_group"))
    plotB <- ggplot(data = efa_scores, 
                    aes_string(x = "F1", y = "F3", 
                               color = group, fill = group, shape = "age_group"))
    plotC <- ggplot(data = efa_scores, 
                    aes_string(x = "F2", y = "F3", 
                               color = group, fill = group, shape = "age_group"))
    
  } else {
    df <- efa_scores %>%
      select(age_group, subid, character, factor_name, score) %>%
      spread(factor_name, score)
    
    df_boot <- efa_scores %>%
      select(age_group, subid, character, factor_name, score) %>%
      group_by_(.groups = group) %>%
      group_by(age_group, factor_name, add = TRUE) %>%
      multi_boot_standard(col = "score") %>%
      ungroup() %>%
      gather(key, val, c(ci_lower, ci_upper, mean)) %>%
      spread(factor_name, val)
    
    plotA <- ggplot(data = df, 
                    aes_string(x = "BODY", y = "HEART",
                               color = group, fill = group, shape = "age_group"))
    plotB <- ggplot(data = df, 
                    aes_string(x = "BODY", y = "MIND",
                               color = group, fill = group, shape = "age_group"))
    plotC <- ggplot(data = df, 
                    aes_string(x = "HEART", y = "MIND",
                               color = group, fill = group, shape = "age_group"))
  }
  
  plotA <- plotA +
    facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    theme(legend.position = "none") +
    guides(color = guide_legend(override.aes = list(shape = 21)))
  
  plotB <- plotB +
    facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    theme(legend.position = "none") +
    guides(color = guide_legend(override.aes = list(shape = 21)))
  
  plotC <- plotC +
    facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    guides(color = guide_legend(override.aes = list(shape = 21)))
  
  if(length(levels(efa_scores$character)) == 2){
    plotA <- plotA + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))
    
    plotB <- plotB + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))
    
    plotC <- plotC + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))
  } else {
    plotA <- plotA + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
    
    plotB <- plotB + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
    
    plotC <- plotC + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
  }
  
  plots <- plot_grid(plotA, plotB, plotC, nrow = 1, 
                     rel_widths = c(1, 1, 1.3), labels = "AUTO")
  
  return(plots)  
}

char_plot_fun3 <- function(efa_scores,
                           factor_names = NA,
                           group = "character"){
  
  if(is.na(factor_names)){
    df_boot <- efa_scores %>%
      gather(factor, score, starts_with("F")) %>%
      group_by_(.groups = group) %>%
      group_by(age_group, factor, add = TRUE) %>%
      multi_boot_standard(col = "score") %>%
      ungroup() %>%
      gather(key, val, c(ci_lower, ci_upper, mean)) %>%
      spread(factor, val)
    
    plotA <- ggplot(data = efa_scores, 
                    aes_string(x = "F1", y = "F2", 
                               color = group, fill = group, shape = "age_group"))
    plotB <- ggplot(data = efa_scores, 
                    aes_string(x = "F1", y = "F3", 
                               color = group, fill = group, shape = "age_group"))
    plotC <- ggplot(data = efa_scores, 
                    aes_string(x = "F2", y = "F3", 
                               color = group, fill = group, shape = "age_group"))
    
  } else {
    df <- efa_scores %>%
      select(age_group, subid, character, factor_name, score) %>%
      spread(factor_name, score)
    
    df_boot <- efa_scores %>%
      select(age_group, subid, character, factor_name, score) %>%
      group_by_(.groups = group) %>%
      group_by(age_group, factor_name, add = TRUE) %>%
      multi_boot_standard(col = "score") %>%
      ungroup() %>%
      gather(key, val, c(ci_lower, ci_upper, mean)) %>%
      spread(factor_name, val)
    
    plotA <- ggplot(data = df, 
                    aes_string(x = "BODY", y = "HEART",
                               color = group, fill = group, shape = "age_group"))
    plotB <- ggplot(data = df, 
                    aes_string(x = "BODY", y = "MIND",
                               color = group, fill = group, shape = "age_group"))
    plotC <- ggplot(data = df, 
                    aes_string(x = "HEART", y = "MIND",
                               color = group, fill = group, shape = "age_group"))
  }
  
  plotA <- plotA +
    facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    # geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    theme(legend.position = "none") +
    guides(color = guide_legend(override.aes = list(shape = 21)))
  
  plotB <- plotB +
    facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    # geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    theme(legend.position = "none") +
    guides(color = guide_legend(override.aes = list(shape = 21)))
  
  plotC <- plotC +
    facet_grid(age_group ~ .) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    # geom_point(alpha = 0.5) + 
    geom_point(data = df_boot %>% filter(key == "mean"),
               aes(fill = .groups),
               color = "black", size = 3) +
    scale_shape_manual(values = c(23, 24, 22)) +
    guides(color = guide_legend(override.aes = list(shape = 21)))
  
  if(length(levels(efa_scores$character)) == 2){
    plotA <- plotA + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))
    
    plotB <- plotB + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))
    
    plotC <- plotC + 
      scale_color_manual(values = c("#fb9a99", "#1f78b4")) +
      scale_fill_manual(values = c("#fb9a99", "#1f78b4"))
  } else {
    plotA <- plotA + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
    
    plotB <- plotB + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
    
    plotC <- plotC + 
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired")
  }
  
  plots <- plot_grid(plotA, plotB, plotC, nrow = 1, 
                     rel_widths = c(1, 1, 1.3), labels = "AUTO")
  
  return(plots)  
}


char_plot_fun(efa_3_scores_d1_all,
              factor_names = c("HEART", "BODY", "MIND"))
  
char_plot_fun3(efa_3_scores_d2_all,
               factor_names = c("BODY", "HEART", "MIND"))

char_plot_fun(efa_3_scores_d3_all,
              factor_names = c("BODY", "HEART", "MIND"))



char_plot_fun(efa_3_scores_d2_all,
              factor_names = c("BODY", "HEART", "MIND"),
              group = "age_group")




