library(tidyverse)
library(factoextra)
# library(FactoMineR)
library(psych)

# temp <- PCA(d_us79_2char, ncp = 16)
# fviz_pca(temp)

fa(d_us79_2char, reten_fact_fun(d_us79_2char, "oblimin"), "oblimin")$loadings[] %>%
  data.frame() %>%
# d_us79_2char %>%
#   drop_na() %>%
#   t() %>%
  hkmeans(3) %>%
  fviz_cluster() +
  theme_minimal() +
  labs(title = "Hierarchical k-means clustering: 7-9yo US children",
       subtitle = "2 characters, 20 capacities, requesting 3 clusters") +
  scale_x_continuous(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(.1, .1)) # +
  # scale_color_manual(values = c("#377eb8", "#4daf4a", "#e41a1c")) +
  # scale_fill_manual(values = c("#377eb8", "#4daf4a", "#e41a1c"))

fa(d_us79_9char, reten_fact_fun(d_us79_9char, "oblimin"), "oblimin")$loadings[] %>%
  data.frame() %>%
# d_us79_9char %>%
#   drop_na() %>%
#   t() %>%
  hkmeans(3) %>%
  fviz_cluster() +
  theme_minimal() +
  labs(title = "Hierarchical k-means clustering: 7-9yo US children",
       subtitle = "9 characters, 20 capacities, requesting 3 clusters") +
  scale_x_continuous(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(.1, .1)) +
  scale_color_manual(values = c("#377eb8", "#4daf4a", "#e41a1c")) +
  scale_fill_manual(values = c("#377eb8", "#4daf4a", "#e41a1c"))

fa(d_us46_9char, reten_fact_fun(d_us46_9char, "oblimin"), "oblimin")$loadings[] %>%
  data.frame() %>%
  hkmeans(3) %>%
  fviz_cluster() +
  theme_minimal() +
  labs(title = "Hierarchical k-means clustering: 4-6yo US children",
       subtitle = "9 characters, 20 capacities, requesting 3 clusters") +
  scale_x_continuous(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(.1, .1)) +
  scale_color_manual(values = c("#e41a1c", "#4daf4a", "#377eb8")) +
  scale_fill_manual(values = c("#e41a1c", "#4daf4a", "#377eb8"))
