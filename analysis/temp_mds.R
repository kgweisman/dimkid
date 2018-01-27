library(tidyverse)
library(ggrepel)
library(directlabels)

# colors
colors <- fa(d_slide_all_complete[,-c(1,2)], 
             nfactors = 3, rotate = "oblimin")$loadings[] %>%
  data.frame() %>%
  rownames_to_column("mc") %>%
  gather(factor, loading, -mc) %>%
  group_by(mc) %>%
  top_n(1, loading) %>%
  ungroup() %>%
  mutate(mc = gsub(" ", ".", mc),
         mc = gsub("_", ".", mc))


# robot alone -----
robot_matrix <- d_slide_all_complete %>%
  arrange(age) %>%
  # mutate(age_rank = rank(age)) %>%
  filter(character == "robot") %>%
  remove_rownames() %>%
  data.frame() %>%
  column_to_rownames("age") %>%
  select(-character, -starts_with("age")) %>%
  as.matrix()

robot_mds <- cmdscale(dist(robot_matrix)) %>% 
  data.frame() %>%
  rownames_to_column("age") %>%
  mutate(age = as.numeric(as.character(age)))

ggplot(robot_mds, aes(x = X1, y = X2, color = age)) +
  geom_point() +
  geom_path() +
  scale_color_distiller(palette = "RdYlBu",
                        limits = c(4, 10), breaks = c(4, 10, 1)) +
  theme_minimal() +
  labs(title = "MDS: robot")


# all by character -----
all_matrix <- d_slide_all_complete %>%
  arrange(age) %>%
  # mutate(age_rank = rank(age)) %>%
  # filter(character == "all") %>%
  mutate(age_char = paste(age, gsub("_", ".", character), sep = "_")) %>%
  remove_rownames() %>%
  data.frame() %>%
  column_to_rownames("age_char") %>%
  select(-character, -starts_with("age")) %>%
  as.matrix()

all_mds <- cmdscale(dist(all_matrix)) %>% 
  data.frame() %>%
  rownames_to_column("age_char") %>%
  mutate(age = as.numeric(gsub("_.*$", "", age_char)),
         character = factor(gsub(".*_", "", age_char),
                            levels = c("computer", "robot",
                                       "doll", "teddy.bear",
                                       "beetle", "bird",
                                       "mouse", "goat", "elephant"))) %>%
  filter(!is.na(age), !is.na(character)) %>%
  distinct()

# ggplot(all_mds, aes(x = X1, y = X2, color = age)) +
#   facet_wrap(~ character) +
#   geom_point() +
#   geom_path() +
#   scale_color_distiller(palette = "RdYlBu",
#                         limits = c(4, 10), breaks = c(4, 10, 1)) +
#   theme_minimal() +
#   labs(title = "MDS: all")

g <- ggplot(all_mds,
            aes(x = X1, y = X2, 
                group = character,
                # color = character,
                color = age,
                frame = age)) +
  geom_point(aes(cumulative = TRUE)) +
  geom_path(aes(cumulative = TRUE)) +
  # scale_color_brewer(type = "div") +
    scale_color_distiller(palette = "RdYlBu",
                          limits = c(4, 10), breaks = seq(4, 10, 1)) +
  theme_minimal()
g

# g_anim <- gganimate::gganimate(g, interval = .1)

ggplot(all_mds,
       aes(x = X1, y = X2,
           group = character,
           color = character,
           alpha = age)) +
  geom_point(data = all_mds %>% filter(character == "computer")) +
  geom_path(data = all_mds %>% filter(character == "computer")) +
  geom_point(data = all_mds %>% filter(character == "robot")) +
  geom_path(data = all_mds %>% filter(character == "robot")) +
  geom_point(data = all_mds %>% filter(character == "doll")) +
  geom_path(data = all_mds %>% filter(character == "doll")) +
  geom_point(data = all_mds %>% filter(character == "teddy.bear")) +
  geom_path(data = all_mds %>% filter(character == "teddy.bear")) +
  geom_point(data = all_mds %>% filter(character == "beetle")) +
  geom_path(data = all_mds %>% filter(character == "beetle")) +
  geom_point(data = all_mds %>% filter(character == "bird")) +
  geom_path(data = all_mds %>% filter(character == "bird")) +
  geom_point(data = all_mds %>% filter(character == "mouse")) +
  geom_path(data = all_mds %>% filter(character == "mouse")) +
  geom_point(data = all_mds %>% filter(character == "goat")) +
  geom_path(data = all_mds %>% filter(character == "goat")) +
  geom_point(data = all_mds %>% filter(character == "elephant")) +
  geom_path(data = all_mds %>% filter(character == "elephant")) +
  scale_color_brewer(limits = levels(all_mds$character),
                     palette = "Spectral") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray30"),
        panel.grid = element_blank())

ggplot(all_mds,
       aes(x = X1, y = X2,
           group = character,
           color = character,
           alpha = age)) +
  facet_wrap(~ character) +
  geom_point() +
  geom_path() +
  scale_color_brewer(limits = levels(all_mds$character),
                     palette = "Spectral") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray30"),
        panel.grid = element_blank())

# transpose, to chart MCs instead of characters -------

all_MC_matrix <- d_slide_all_complete %>%
  mutate(age_quantile = ntile(age, 5),
         age_char = paste(age_quantile, gsub("_", ".", character), sep = "_")) %>%
  select(-age) %>%
  gather(mc, response, -character, -age_quantile, -age_char) %>%
  group_by(age_char, character, age_quantile, mc) %>%
  summarise_all(. %>% mean(na.rm = T)) %>%
  ungroup() %>%
  mutate(age_mc = paste(age_quantile, mc, sep = "_")) %>%
  select(character, age_mc, response) %>%
  spread(age_mc, response) %>%
  remove_rownames() %>%
  data.frame() %>%
  column_to_rownames("character") %>%
  as.matrix() %>%
  t()

# all_MC_matrix <- d_slide_all_complete %>%
#     arrange(age) %>%
#     # mutate(age_rank = rank(age)) %>%
#     # filter(character == "all") %>%
#     mutate(age_char = paste(age, gsub("_", ".", character), sep = "_")) %>%
#     remove_rownames() %>%
#     data.frame() %>%
#     column_to_rownames("age_char") %>%
#     select(-character, -starts_with("age")) %>%
#     as.matrix() %>%
#     t()

# all_MC_matrix <- d_slide_all_complete %>%
#   rownames_to_column("subid") %>%
#   arrange(age, subid) %>%
#   # mutate(age_rank = rank(age)) %>%
#   # filter(character == "all") %>%
#   select(-character) %>%
#   gather(mc, response, -subid, -age) %>%
#   mutate(age_subid_mc = paste(age, subid, gsub(" ", ".", mc), sep = "_")) %>%
#   remove_rownames() %>%
#   data.frame() %>%
#   column_to_rownames("age_subid_mc") %>%
#   select(-subid, -starts_with("age")) %>%
#   spread(mc, response) %>%
#   as.matrix()
all_MC_mds <- cmdscale(dist(all_MC_matrix)) %>% 
  data.frame() %>%
  rownames_to_column("age_mc") %>%
  mutate(mc = gsub(".+_", "", age_mc),
         age_quantile = gsub("_.+$", "", age_mc)) %>%
  filter(!is.na(mc)) %>%
  distinct()

# ggplot(all_MC_mds, aes(x = X1, y = X2, color = age)) +
#   facet_wrap(~ character) +
#   geom_point() +
#   geom_path() +
#   scale_color_distiller(palette = "RdYlBu",
#                         limits = c(4, 10), breaks = c(4, 10, 1)) +
#   theme_minimal() +
#   labs(title = "MDS: all")

g <- ggplot(all_MC_mds,
            aes(x = X1, y = X2, 
                group = mc,
                label = mc,
                color = mc,
                # color = age,
                frame = age_quantile)) +
  geom_point(aes(cumulative = TRUE)) +
  # geom_text_repel(aes(cumulative = TRUE)) +
  # geom_dl(method = "last.qp") +
  geom_path(aes(cumulative = TRUE)) +
  # scale_color_brewer(type = "div") +
  # scale_color_distiller(palette = "RdYlBu",
  #                       limits = c(4, 10), breaks = seq(4, 10, 1)) +
  theme_minimal()
g

# g + geom_dl(method = "first.qp")

# g_anim <- gganimate::gganimate(g, interval = .1)


ggplot(all_MC_mds,
       aes(x = X1, y = X2,
           group = mc,
           color = mc,
           alpha = age_quantile)) +
  geom_point(data = all_MC_mds %>% filter(mc == "anger")) +
  geom_path(data = all_MC_mds %>% filter(mc == "anger")) +
  geom_point(data = all_MC_mds %>% filter(mc == "awareness")) +
  geom_path(data = all_MC_mds %>% filter(mc == "awareness")) +
  geom_point(data = all_MC_mds %>% filter(mc == "choice")) +
  geom_path(data = all_MC_mds %>% filter(mc == "choice")) +
  geom_point(data = all_MC_mds %>% filter(mc == "depth")) +
  geom_path(data = all_MC_mds %>% filter(mc == "depth")) +
  geom_point(data = all_MC_mds %>% filter(mc == "embarrassment")) +
  geom_path(data = all_MC_mds %>% filter(mc == "embarrassment")) +
  geom_point(data = all_MC_mds %>% filter(mc == "fatigue")) +
  geom_path(data = all_MC_mds %>% filter(mc == "fatigue")) +
  geom_point(data = all_MC_mds %>% filter(mc == "fear")) +
  geom_path(data = all_MC_mds %>% filter(mc == "fear")) +
  geom_point(data = all_MC_mds %>% filter(mc == "figuring.out")) +
  geom_path(data = all_MC_mds %>% filter(mc == "figuring.out")) +
  geom_point(data = all_MC_mds %>% filter(mc == "guilt")) +
  geom_path(data = all_MC_mds %>% filter(mc == "guilt")) +
  geom_point(data = all_MC_mds %>% filter(mc == "happiness")) +
  geom_path(data = all_MC_mds %>% filter(mc == "happiness")) +
  geom_point(data = all_MC_mds %>% filter(mc == "hunger")) +
  geom_path(data = all_MC_mds %>% filter(mc == "hunger")) +
  geom_point(data = all_MC_mds %>% filter(mc == "hurt.feelings")) +
  geom_path(data = all_MC_mds %>% filter(mc == "hurt.feelings")) +
  geom_point(data = all_MC_mds %>% filter(mc == "love")) +
  geom_path(data = all_MC_mds %>% filter(mc == "love")) +
  geom_point(data = all_MC_mds %>% filter(mc == "memory")) +
  geom_path(data = all_MC_mds %>% filter(mc == "memory")) +
  geom_point(data = all_MC_mds %>% filter(mc == "nausea")) +
  geom_path(data = all_MC_mds %>% filter(mc == "nausea")) +
  geom_point(data = all_MC_mds %>% filter(mc == "pain")) +
  geom_path(data = all_MC_mds %>% filter(mc == "pain")) +
  geom_point(data = all_MC_mds %>% filter(mc == "pride")) +
  geom_path(data = all_MC_mds %>% filter(mc == "pride")) +
  geom_point(data = all_MC_mds %>% filter(mc == "sadness")) +
  geom_path(data = all_MC_mds %>% filter(mc == "sadness")) +
  geom_point(data = all_MC_mds %>% filter(mc == "smell")) +
  geom_path(data = all_MC_mds %>% filter(mc == "smell")) +
  geom_point(data = all_MC_mds %>% filter(mc == "temperature")) +
  geom_path(data = all_MC_mds %>% filter(mc == "temperature")) +
  # scale_color_brewer(limits = levels(all_MC_mds$mc),
  #                    palette = "Spectral") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray30"),
        panel.grid = element_blank())

ggplot(all_MC_mds,
       aes(x = X1, y = X2,
           group = mc,
           color = mc,
           alpha = age_quantile)) +
  facet_wrap(~ mc) +
  geom_point() +
  geom_path() +
  # scale_color_brewer(limits = levels(all_MC_mds$mc),
  #                    palette = "Spectral") +
  theme_minimal() # +
  # theme(panel.background = element_rect(fill = "gray30"),
  #       panel.grid = element_blank())


ggplot(all_MC_mds,
       aes(x = X1, y = X2,
           # group = mc,
           label = mc,
           color = mc)) +
  facet_wrap(~ age_quantile) +
  # geom_point() +
  # geom_text_repel() +
  geom_text() +
  geom_path() +
  theme_bw()


# mds by age quantile -----

mds_quant_fun <- function(df, which_quantile) {
  temp_age <- df %>%
    mutate(age_quantile = ntile(age, 5)) %>%
    filter(age_quantile == which_quantile) %>%
    summarise(min = min(age),
              max = max(age),
              n = n())
  
  temp_df <- df %>%
    mutate(age_quantile = ntile(age, 5),
           age_char = paste(age_quantile, gsub("_", ".", character), sep = "_")) %>%
    filter(age_quantile == which_quantile) %>%
    select(-age) %>%
    gather(mc, response, -character, -age_quantile, -age_char) %>%
    group_by(age_char, character, age_quantile, mc) %>%
    summarise_all(. %>% mean(na.rm = T)) %>%
    ungroup() %>%
    mutate(age_mc = paste(age_quantile, mc, sep = "_")) %>%
    select(character, age_mc, response) %>%
    spread(age_mc, response) %>%
    remove_rownames() %>%
    data.frame() %>%
    column_to_rownames("character") %>%
    as.matrix() %>%
    t()
  
  mds_temp <- cmdscale(dist(temp_df)) %>% 
    data.frame() %>%
    rownames_to_column("age_mc") %>%
    mutate(mc = gsub(".+_", "", age_mc),
           age_quantile = gsub("_.+$", "", age_mc)) %>%
    filter(!is.na(mc)) %>%
    distinct() %>%
    full_join(colors)
  
  bounding_min_x <- min(mds_temp$X1)
  bounding_max_x <- max(mds_temp$X1)
  bounding_min_y <- min(mds_temp$X2)
  bounding_max_y <- max(mds_temp$X2)
  
  g <- ggplot(mds_temp,
              aes(x = X1, y = X2,
                  color = factor,
                  label = mc)) +
    # geom_rect(aes(xmin = bounding_min_x, xmax = bounding_max_x,
    #              ymin = bounding_min_y, ymax = bounding_max_y),
    #           alpha = 0.005, fill = "yellow", color = "black",
    #           size = 0.1) +
    # geom_path() +
    geom_point() +
    geom_text_repel(size = 3, segment.alpha = 0.5) +
    # geom_text() +
    scale_color_manual(limits = c("MR3", "MR1", "MR2"),
                       labels = c("BODY", "HEART", "MIND"),
                       values = c("#e41a1c", "#377eb8", "#4daf4a")) +
    theme_bw() +
    xlim(-1.2, 1.2) +
    ylim(-1.2, 1.2) +
    labs(title = paste0(round(temp_age$min, 2), " - ",
                        round(temp_age$max, 2), " years ",
                       "(n = ", temp_age$n, ")"),
         x = "MDS dimension 1",
         y = "MDS dimension 2") +
    theme(legend.position = "top")
  
  return(g)
}

q1 <- mds_quant_fun(d_slide_all_complete, 1)
q2 <- mds_quant_fun(d_slide_all_complete, 2)
q3 <- mds_quant_fun(d_slide_all_complete, 3)
q4 <- mds_quant_fun(d_slide_all_complete, 4)
q5 <- mds_quant_fun(d_slide_all_complete, 5)

cowplot::plot_grid(q1, q2, q3, q4, q5, nrow = 2)


# sliding window?? ------

window_size <- 120
sliding_mds_fun <- function(df, window_size = window_size){
  temp_age <- df %>%
    top_n(-window_size, age) %>%
    summarise(min = min(age),
              max = max(age))
  
  temp_df <- df %>%
    top_n(-window_size, age) %>%
    select(-age) %>%
    gather(mc, response, -character) %>%
    group_by(character, mc) %>%
    summarise_all(. %>% mean(na.rm = T)) %>%
    ungroup() %>%
    spread(mc, response) %>%
    remove_rownames() %>%
    data.frame() %>%
    column_to_rownames("character") %>%
    as.matrix() %>%
    t()
  
  mds_temp <- cmdscale(dist(temp_df)) %>% 
    data.frame() %>%
    rownames_to_column("mc") %>%
    filter(!is.na(mc)) %>%
    distinct() %>%
    full_join(colors)
  
  # g <- ggplot(mds_temp,
  #             aes(x = X1, y = X2,
  #                 color = factor,
  #                 label = mc)) +
  #   # geom_path() +
  #   geom_point() +
  #   geom_text_repel() +
  #   # geom_text() +
  #   scale_color_brewer(palette = "Dark2") +
  #   theme_bw() +
  #   labs(title = paste("ages", round(temp_age$min, 2), "-", 
  #                      round(temp_age$max, 2), "years")) +
  #   theme(legend.position = "none")
  
  return(mds_temp)
}

# sliding_list <- list(NULL)
sliding_list <- data.frame(mc = character(),
                           X1 = numeric(),
                           X2 = numeric(),
                           factor = character(),
                           loading = numeric(),
                           window = numeric())

for(i in 1:(nrow(d_slide_all_complete)-window_size)) {
  df_temp <- d_slide_all_complete %>% arrange(age)
  window_temp <- df_temp[c(i:(i + window_size - 1)),]
  mds_temp <- sliding_mds_fun(df = window_temp, window_size = window_size)
  # sliding_list[[i]] <- mds_temp
  sliding_list <- full_join(sliding_list, 
                            mds_temp %>% mutate(window = i))
}

ggplot(sliding_list,
       aes(x = X1, y = X2,
           color = factor,
           label = mc)) +
  facet_wrap(~ window) +
  # geom_path() +
  geom_point() +
  # geom_text_repel() +
  # geom_text() +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  # labs(title = paste("age window:", window)) +
  theme(legend.position = "top")

ggplot(sliding_list,
       aes(x = X1, y = X2,
           # group = mc,
           color = factor,
           # alpha = window,
           label = mc)) +
  facet_wrap(~ window, nrow = 10) +
  # geom_path() +
  geom_point() +
  # geom_text_repel() +
  # geom_text() +
  scale_color_manual(values = c("#e41a1c", "#4daf4a", "#377eb8")) +
  # scale_color_brewer(palette = "Dark2") +
  # theme_bw() +
  theme_minimal() +
  # labs(title = paste("age window:", window)) +
  theme(legend.position = "top")

# sliding_mds_fun(d_slide_all_complete)
