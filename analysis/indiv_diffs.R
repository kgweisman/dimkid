# kid studies

temp <- factors_s1_long %>%
  mutate(capacity = gsub(" -- ", ", ", capacity),
         capacity = gsub("pride", "feel proud", capacity),
         capacity = factor(capacity)) %>%
  arrange(desc(loading)) %>%
  group_by(capacity) %>%
  top_n(1)

temp_df <- d1 %>%
  select(subid, character, capWording, responseNumC) %>%
  rename(capacity = capWording) %>%
  mutate(capacity = gsub(" -- ", ", ", capacity)) %>%
  full_join(temp) %>% 
  distinct()

temp_df %>% 
  group_by(subid, character, factor) %>%
  summarise(mean = mean(responseNumC, na.rm = T)) %>%
  ggplot(aes(x = mean)) +
  geom_histogram(bins = 10) +
  facet_grid(character ~ factor) +
  xlim(-.5, .5) +
  geom_vline(xintercept = -0.5, color = "blue", lty = 1) +
  geom_vline(xintercept = 0, color = "blue", lty = 1) +
  geom_vline(xintercept = 0.5, color = "blue", lty = 1) +
  labs(title = "participant-level means by factor")

temp_df %>% 
  group_by(subid, character, factor) %>%
  summarise(sd = sd(responseNumC, na.rm = T)) %>%
  ggplot(aes(x = sd)) +
  geom_histogram(bins = 10) +
  facet_grid(character ~ factor) +
  # xlim(-.5, .5) +
  # geom_vline(xintercept = -0.5, color = "blue", lty = 1) +
  # geom_vline(xintercept = 0, color = "blue", lty = 1) +
  # geom_vline(xintercept = 0.5, color = "blue", lty = 1) +
  labs(title = "participant-level sds by factor")

scores_s1_plotting %>% ggplot(aes(x = score, fill = character)) + geom_density(alpha = 0.5) + facet_grid(~ factor) + theme_bw()

# adult studies

temp <- efa_d1_all_rotatedN_loadings %>%
  mutate(factor = ifelse(MR1 > MR2 & MR1 > MR3, "F1",
                         ifelse(MR2 > MR1 & MR2 > MR3, "F2",
                                ifelse(MR3 > MR1 & MR3 > MR2, "F3",
                                       NA))))

temp_df <- d1 %>%
  gather(mc, responseNumC, happy:pride) %>%
  rename(character = condition) %>%
  select(subid, character, mc, responseNumC) %>%
  full_join(temp) %>% 
  distinct()

temp_df %>% 
  group_by(subid, character, factor) %>%
  summarise(mean = mean(responseNumC, na.rm = T)) %>%
  ggplot(aes(x = mean)) +
  geom_histogram(bins = 10) +
  facet_grid(character ~ factor) +
  xlim(-3, 3) +
  geom_vline(xintercept = -3, color = "blue", lty = 1) +
  geom_vline(xintercept = 0, color = "blue", lty = 1) +
  geom_vline(xintercept = 3, color = "blue", lty = 1) +
  labs(title = "participant-level means by factor")

temp_df %>% 
  group_by(subid, character, factor) %>%
  summarise(sd = sd(responseNumC, na.rm = T)) %>%
  ggplot(aes(x = sd)) +
  geom_histogram(bins = 10) +
  facet_grid(character ~ factor) +
  labs(title = "participant-level sds by factor")

efa_d1_all_rotatedN$scores %>% 
  data.frame() %>%
  rownames_to_column("subid") %>%
  left_join(d1 %>% select(subid, condition)) %>%
  gather(factor, score, starts_with("MR")) %>%
  ggplot(aes(x = score, fill = condition)) + geom_density(alpha = 0.5) + facet_grid(~ factor) + theme_bw()
