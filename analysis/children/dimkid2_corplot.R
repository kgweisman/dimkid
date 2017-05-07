factors_df <- fa.sort(fa(d4, nfactors = 3, rotate = "oblimin")$loadings[]) %>% 
  data.frame() %>%
  rownames_to_column(var = "capacity") %>%
  left_join(d2_bycond_mb %>% data.frame() %>% select(capacity, capWordingShort) %>% distinct()) %>%
  mutate(capWordingShort = gsub("sense whether something is close by or far away",
                                "sense whether... far away", capWordingShort)) %>%
  # mutate(capWordingShort = gsub("figure out how to do things", 
  #                               "figure out how...", capWordingShort)) %>%
  # mutate(capWordingShort = gsub("sense temperatures", 
  #                               "sense temp...", capWordingShort)) %>%
  select(-capacity) %>%
  # column_to_rownames(var = "capWordingShort") %>%
  rename(capacity = capWordingShort, Factor1 = MR1, Factor2 = MR2, Factor3 = MR3) %>%
  rownames_to_column(var = "order") %>%
  mutate(order = as.numeric(order))

factors_df_long <- factors_df %>%
  gather(factor, loading, -capacity, -order) %>%
  arrange(order, factor)

ggplot(factors_df_long, aes(x = factor(factor, labels = c("Factor 1", "Factor 2", "Factor 3")), 
                            y = reorder(capacity, desc(order)), fill = loading)) + 
  geom_tile(color = "black") +
  geom_text(aes(label = format(round(loading, 2), nsmall = 2))) +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), breaks = c(-1, 0, 1),
                       guide = guide_colorbar(title = element_blank(),
                                              barheight = 20)) +
  scale_x_discrete(position = "top") +
  # geom_rect(aes(xmin = 0.51, xmax = 1.49, ymin = 14.55, ymax = 20.45),
  #           alpha = 0, color = "black", size = .5) +
  # geom_rect(aes(xmin = 1.51, xmax = 2.49, ymin = 6.55, ymax = 14.45),
  #           alpha = 0, color = "black", size = .5) +
  # geom_rect(aes(xmin = 2.51, xmax = 3.49, ymin = 0.55, ymax = 6.45),
  #           alpha = 0, color = "black", size = .5) +
  # theme_bw() +
  theme_minimal() +
  theme(text = element_text(size = 24),
        axis.title = element_blank(),
        panel.grid = element_blank())
