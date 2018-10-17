# first run bodyheartmind_analysis.Rmd

order <- loadings(fa.sort(efa_d4_all_rotatedN))[] %>%
  data.frame() %>%
  rownames_to_column("mc") %>%
  left_join(char_plotting_wordings) %>%
  gather(factor, loading, -mc, -wording) %>%
  group_by(mc) %>%
  top_n(1, abs(loading)) %>%
  ungroup() %>%
  arrange(factor, desc(abs(loading))) %>%
  mutate(order = 1:40) %>%
  select(mc, order)

loadings(fa.sort(efa_d4_all_rotatedN))[] %>%
  data.frame() %>%
  rownames_to_column("mc") %>%
  left_join(char_plotting_wordings) %>%
  gather(factor, loading, -mc, -wording) %>%
  left_join(order) %>%
  ggplot(aes(x = factor, y = reorder(wording, desc(order)), fill = loading)) +
  geom_tile(color = "black") +
  geom_text(aes(label = format(round(loading, 2), nsmall = 2))) +
  scale_fill_distiller(palette = "RdYlBu",limits = c(-1, 1), 
                       guide = guide_colorbar(barheight = 20, barwidth = 0.5)) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  labs(fill = "")

