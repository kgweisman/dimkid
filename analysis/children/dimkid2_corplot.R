# SET PARAMETERS (overrides previous run)

# cor_type <- "cor"
# cor_type <- "poly"

# rot_type <- "varimax"
rot_type <- "oblimin"

# score_type <- "tenBerge"
score_type <- "Thurstone"

# data_set <- d3_adult # study 1, 2 characters, adults
# data_set <- d3_child # study 2, 2 characters, children
# data_set <- d3_combined # studies 1-2, 2 characters, adults/children
data_set <- d4 # study 3, 7 characters

# cap_key <- d1_bycond2_mb_factorsAll # studies 1-2, 2 characters, adults/children
cap_key <- d2_bycond_mb %>% rename(capWording = capWordingShort) # study 3, 7 characters

# MAKE DATAFRAME

factors_df <- fa.sort(fa(data_set, nfactors = 3, rotate = rot_type)$loadings[]) %>% 
  data.frame() %>%
  rownames_to_column(var = "capacity") %>%
  # left_join(d2_bycond_mb %>% data.frame() %>% select(capacity, capWordingShort) %>% distinct()) %>%
  left_join(cap_key %>% data.frame() %>% select(capacity, capWording) %>% distinct()) %>%
  mutate(capWordingShort = gsub("_", " ", capWording)) %>%
  # mutate(capWordingShort = gsub("sense whether something is close by or far away",
  #                               "sense whether... far away", cap_key)) %>%
  # mutate(capWordingShort = gsub("figure out how to do things", 
  #                               "figure out how...", cap_key)) %>%
  # mutate(capWordingShort = gsub("sense temperatures", 
  #                               "sense temp...", cap_key)) %>%
  select(capWordingShort, MR1, MR2, MR3) %>%
  # column_to_rownames(var = "capWordingShort") %>%
  rename(capacity = capWordingShort, Factor1 = MR1, Factor2 = MR2, Factor3 = MR3) %>%
  rownames_to_column(var = "order") %>%
  mutate(order = as.numeric(order))

factors_df_long <- factors_df %>%
  gather(factor, loading, -capacity, -order) %>%
  # mutate(factor = factor(gsub("Factor", "Factor ", factor))) %>%
  # mutate(factor = factor(gsub("Factor", "Factor ", factor),
  #                        levels = c("Factor 1", "Factor 3", "Factor 2"))) %>%
  mutate(factor = factor(gsub("Factor", "Factor ", factor),
                         levels = c("Factor 2", "Factor 1", "Factor 3"))) %>%
  arrange(order, factor)

ggplot(factors_df_long, aes(x = factor, 
                            y = reorder(capacity, desc(order)), fill = loading)) + 
  geom_tile(color = "black") +
  # geom_text(aes(label = format(round(loading, 2), nsmall = 2))) +
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
        panel.grid = element_blank()) # 1000 by 1000 

# correlations by participant
cor.ci(fa(data_set, nfactors = 3, rotate = rot_type, cor = cor_type, scores = score_type)$scores %>% data.frame())
