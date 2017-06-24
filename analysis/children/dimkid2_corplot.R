# SET PARAMETERS (overrides previous run)

# cor_type <- "cor"
# cor_type <- "poly"

# rot_type <- "varimax"
rot_type <- "oblimin"

# score_type <- "tenBerge"
score_type <- "Thurstone"

# data_set <- d3_adult # study 1, 2 characters, adults
# data_set <- d3_child # study 2, 2 characters, children
data_set <- d3_combined # studies 1-2, 2 characters, adults/children
# data_set <- d4 # study 3, 7 characters

cap_key <- d1_bycond2_mb_factorsAll %>% select(capacity, capWording) %>% distinct() # studies 1-2, 2 characters, adults/children
# cap_key <- d2_bycond_mb %>% rename(capWording = capWordingShort) # study 3, 7 characters

# MAKE DATAFRAMES

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
  mutate(factor = factor(gsub("Factor", "F", factor))) %>%
  # mutate(factor = factor(gsub("Factor", "F", factor),
  #                        levels = c("F1", "F3", "F2"))) %>%
  # mutate(factor = factor(gsub("Factor", "F", factor),
  #                        levels = c("F2", "F1", "F3"))) %>%
  arrange(order, factor)

factors_df_blank1 <- factors_df_long %>%
  mutate(loading = rep(100, length(factors_df_long$loading)))
factors_df_blank2 <- factors_df_long %>%
  mutate(loading = ifelse(factor == "F1", loading, rep(100, length(factors_df_long$loading)*2/3)))
factors_df_blank3 <- factors_df_long %>%
  mutate(loading = ifelse(factor != "F3", loading, rep(100, length(factors_df_long$loading)*1/3)))

ggplot(factors_df_blank1, aes(x = factor,
                            y = reorder(capacity, desc(order)), fill = loading)) +
  geom_tile(color = "black") +
  # geom_text(aes(label = format(round(loading, 2), nsmall = 2))) +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), breaks = c(-1, 0, 1),
                       guide = guide_colorbar(title = element_blank(),
                                              barheight = 20)) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(size = 28),
        axis.title = element_blank(),
        panel.grid = element_blank()) # 1000 by 1000

ggplot(factors_df_blank2, aes(x = factor,
                              y = reorder(capacity, desc(order)), fill = loading)) +
  geom_tile(color = "black") +
  # geom_text(aes(label = format(round(loading, 2), nsmall = 2))) +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), breaks = c(-1, 0, 1),
                       guide = guide_colorbar(title = element_blank(),
                                              barheight = 20)) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(size = 28),
        axis.title = element_blank(),
        panel.grid = element_blank()) # 1000 by 1000

ggplot(factors_df_blank3, aes(x = factor,
                              y = reorder(capacity, desc(order)), fill = loading)) +
  geom_tile(color = "black") +
  # geom_text(aes(label = format(round(loading, 2), nsmall = 2))) +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), breaks = c(-1, 0, 1),
                       guide = guide_colorbar(title = element_blank(),
                                              barheight = 20)) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(size = 28),
        axis.title = element_blank(),
        panel.grid = element_blank()) # 1000 by 1000

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
        axis.text.x = element_text(size = 28),
        axis.title = element_blank(),
        panel.grid = element_blank()) # 1000 by 1000

# # correlations by participant
# cor.ci(fa(data_set, nfactors = 3, rotate = rot_type, cor = cor_type, scores = score_type)$scores %>% data.frame())
# 
# 
# # NEW VERSION 2017-05-10
# 
# # make corplot functions
# make_corplot_df <- function(d, cor = c("cor", "poly"),
#                             rot = c("varimax", "oblimin")) {
#   
#   # set variables
#   data_set <- d
#   cor_type <- cor
#   rot_type <- rot
#   
#   # make dataframe
#   factors_df <- fa.sort(fa(data_set, nfactors = 3, rotate = rot_type)$loadings[]) %>% 
#     data.frame() %>%
#     rownames_to_column(var = "capacity") %>%
#     left_join(cap_key) %>%
#     mutate(short = gsub("_", " ", capWording)) %>%
#     select(short, MR1, MR2, MR3) %>%
#     rename(Factor1 = MR1, Factor2 = MR2, Factor3 = MR3) %>%
#     rownames_to_column(var = "order") %>%
#     mutate(order = as.numeric(order))
#   
#   factors_df_long <- factors_df %>%
#     gather(factor, loading, -short, -order) %>%
#     mutate(factor = factor(gsub("Factor", "Factor ", factor)),
#            face = ifelse(abs(loading) > 0.6, "bold", "plain")) %>%
#     arrange(order, factor)
#   
#   return(factors_df_long)
# }
# 
# make_corplot_plot <- function(d) {
#   
#   # make plot
#   p <- ggplot(d, aes(x = factor,
#                      y = reorder(short, desc(order)), fill = loading)) +
#     geom_tile(color = "black") +
#     geom_text(aes(label = format(round(loading, 2), nsmall = 2),
#                   fontface = face)) +
#     scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), breaks = c(-1, 0, 1),
#                          guide = guide_colorbar(title = element_blank(),
#                                                 barheight = 20)) +
#     scale_x_discrete(position = "top") +
#     # geom_rect(aes(xmin = 0.51, xmax = 1.49, ymin = 14.55, ymax = 20.45),
#     #           alpha = 0, color = "black", size = .5) +
#     # geom_rect(aes(xmin = 1.51, xmax = 2.49, ymin = 6.55, ymax = 14.45),
#     #           alpha = 0, color = "black", size = .5) +
#     # geom_rect(aes(xmin = 2.51, xmax = 3.49, ymin = 0.55, ymax = 6.45),
#     #           alpha = 0, color = "black", size = .5) +
#     # theme_bw() +
#     theme_minimal() +
#     theme(text = element_text(size = 18),
#           axis.text.x = element_text(size = 24),
#           axis.title = element_blank(),
#           panel.grid = element_blank()) # 1000 by 1000 
#   
#   # return plot 
#   return(p)
# }
# 
# # set capacity key
# cap_key <- d1_bycond2_mb_factorsAll %>% select(capacity, capWording) %>% distinct() # studies 1-2, 2 characters, adults/children
# # cap_key <- d2_bycond_mb %>% rename(capWording = capWordingShort) # study 3, 7 characters
# 
# # make individual plots per study
# corplot_d3_adult <- make_corplot_df(d3_adult, "cor", "oblimin")
# corplot_d3_child <- make_corplot_df(d3_child, "cor", "oblimin")
# corplot_d3_combined <- make_corplot_df(d3_combined, "cor", "oblimin")
# corplot_all_studies <- corplot_d3_combined %>%
#   select(short, loading, face, factor) %>%
#   mutate(study = "Combined") %>%
#   full_join(corplot_d3_child %>% select(short, loading, face, factor) %>% mutate(study = "Children")) %>%
#   full_join(corplot_d3_adult %>% select(short, loading, face, factor) %>% mutate(study = "Adults")) %>%
#   full_join(corplot_d3_combined %>% select(short, order) %>% distinct()) %>%
#   mutate(factor = paste0(factor, " (", study, ")"))
# 
# # make corplots... and one for whole study
# make_corplot_plot(corplot_d3_adult) +
#   scale_x_discrete(limits = c("Factor 1", "Factor 3", "Factor 2"), position = "top")
# make_corplot_plot(corplot_d3_child)
# make_corplot_plot(corplot_d3_combined)
# # make_corplot_plot(corplot_all_studies) +
# #   geom_rect(aes(xmin = 0.51, xmax = 3.49, ymin = 21.55, ymax = 40.45),
# #             alpha = 0, color = "black", size = .5) +
# #   geom_rect(aes(xmin = 3.51, xmax = 6.49, ymin = 13.55, ymax = 21.55),
# #             alpha = 0, color = "black", size = .5) +
# #   geom_rect(aes(xmin = 6.51, xmax = 9.49, ymin = 0.55, ymax = 13.45),
# #             alpha = 0, color = "black", size = .5) +
# #   scale_x_discrete(labels = rep(c("Adults", "Children", "Combined"), 3), 
# #                    limits = c("Factor 2 (Adults)", "Factor 1 (Children)", "Factor 1 (Combined)",
# #                               "Factor 1 (Adults)", "Factor 2 (Children)", "Factor 2 (Combined)",
# #                               "Factor 3 (Adults)", "Factor 3 (Children)", "Factor 3 (Combined)"),
# #                    position = "top")
