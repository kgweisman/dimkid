d_scored <- d2_79_scored_ad %>%
  left_join(d2_79 %>% distinct(subid, age)) %>%
  group_by(character) %>%
  mutate(age_group2 = ntile(age, 10)) %>%
  ungroup() %>%
  full_join(d2_ad_scored_ad %>% mutate(age_group2 = 11))

d_scored_means_BODY <- d_scored %>%
  filter(factor == "BODY") %>%
  group_by(age_group2, character) %>%
  multi_boot_standard(col = "score") %>%
  ungroup() %>%
  rename(ci_lower_BODY = ci_lower, ci_upper_BODY = ci_upper, mean_BODY = mean)

d_scored_means_HEART <- d_scored %>%
  filter(factor == "HEART") %>%
  group_by(age_group2, character) %>%
  multi_boot_standard(col = "score") %>%
  ungroup() %>%
  rename(ci_lower_HEART = ci_lower, ci_upper_HEART = ci_upper, mean_HEART = mean)

d_scored_means_MIND <- d_scored %>%
  filter(factor == "MIND") %>%
  group_by(age_group2, character) %>%
  multi_boot_standard(col = "score") %>%
  ungroup() %>%
  rename(ci_lower_MIND = ci_lower, ci_upper_MIND = ci_upper, mean_MIND = mean)

d_scored_means_all <- d_scored_means_BODY %>%
  full_join(d_scored_means_HEART) %>%
  full_join(d_scored_means_MIND) %>%
  filter(!is.na(character)) %>%
  arrange(character, age_group2)

plot_BH <- ggplot(d_scored_means_all,
                  aes(x = mean_BODY, y = mean_HEART, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  ggforce::geom_link2(arrow = arrow(type = "closed", length = unit(0.5, "lines")),
                      show.legend = F) +
  geom_point(data = . %>% filter(age_group2 != 11),
             aes(size = factor(age_group2))) +
  scale_color_manual("Target character", values = colors02, na.translate = F) +
  scale_size_manual("Age group", values = seq(1, 3, length.out = 11), guide = "none") +
  scale_x_continuous("BODY score", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous("HEART score", limits = c(0, 1), breaks = seq(0, 1, 0.2))

plot_BM <- ggplot(d_scored_means_all,
                  aes(x = mean_BODY, y = mean_MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  ggforce::geom_link2(arrow = arrow(type = "closed", length = unit(0.5, "lines")),
                      show.legend = F) +
  geom_point(data = . %>% filter(age_group2 != 11),
             aes(size = factor(age_group2))) +
  scale_color_manual("Target character", values = colors02, na.translate = F) +
  scale_size_manual("Age group", values = seq(1, 3, length.out = 11), guide = "none") +
  scale_x_continuous("BODY score", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous("MIND score", limits = c(0, 1), breaks = seq(0, 1, 0.2))

plot_HM <- ggplot(d_scored_means_all,
                  aes(x = mean_HEART, y = mean_MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  ggforce::geom_link2(arrow = arrow(type = "closed", length = unit(0.5, "lines")),
                      show.legend = F) +
  geom_point(data = . %>% filter(age_group2 != 11),
             aes(size = factor(age_group2))) +
  scale_color_manual("Target character", values = colors02, na.translate = F) +
  scale_size_manual("Age group", values = seq(1, 3, length.out = 11), guide = "none") +
  scale_x_continuous("HEART score", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous("MIND score", limits = c(0, 1), breaks = seq(0, 1, 0.2))