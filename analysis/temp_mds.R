library(tidyverse)

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

g_anim <- gganimate::gganimate(g, interval = .1)


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
  geom_point(data = all_mds %>% filter(character == "teddy_bear")) +
  geom_path(data = all_mds %>% filter(character == "teddy_bear")) +
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
  # scale_color_distiller(palette = "RdYlBu",
  #                       limits = c(4, 10), breaks = seq(4, 10, 1)) +
  theme_minimal()

ggplot(all_mds,
       aes(x = X1, y = X2,
           group = character,
           color = character,
           alpha = age)) +
  facet_wrap(~ character) +
  geom_point() +
  geom_path() +
  # scale_color_distiller(palette = "RdYlBu",
  #                       limits = c(4, 10), breaks = seq(4, 10, 1)) +
  theme_minimal()
  