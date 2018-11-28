temp_squares_d1 <- d1_all_endorse %>%
  # filter(factor %in% c("BODY", "HEART")) %>%
  group_by(subid) %>%
  mutate(sum_endorse = prop_endorse * n) %>%
  ungroup() %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  mutate(BvH = case_when(BODY > 3 & HEART < 3 ~ "BODY without HEART",
                         BODY < 3 & HEART > 3 ~ "HEART without BODY",
                         TRUE ~ "roughly equal"),
         BvM = case_when(BODY > 3 & MIND < 3 ~ "BODY without MIND",
                         BODY < 3 & MIND > 3 ~ "MIND without BODY",
                         TRUE ~ "roughly equal"),
         HvM = case_when(HEART > 3 & MIND < 3 ~ "HEART without MIND",
                         HEART < 3 & MIND > 3 ~ "MIND without HEART",
                         TRUE ~ "roughly equal"))

temp_squares_d1 %>%
  count(age_group, BvH) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()

temp_squares_d1 %>%
  count(age_group, BvM) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()

temp_squares_d1 %>%
  count(age_group, HvM) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()





temp_squares_d2 <- d2_all_endorse %>%
  # filter(factor %in% c("BODY", "HEART")) %>%
  group_by(subid) %>%
  mutate(sum_endorse = prop_endorse * n) %>%
  ungroup() %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  mutate(BvH = case_when(BODY > 3 & HEART < 3 ~ "BODY without HEART",
                         BODY < 3 & HEART > 3 ~ "HEART without BODY",
                         TRUE ~ "roughly equal"),
         BvM = case_when(BODY > 3 & MIND < 3 ~ "BODY without MIND",
                         BODY < 3 & MIND > 3 ~ "MIND without BODY",
                         TRUE ~ "roughly equal"),
         HvM = case_when(HEART > 3 & MIND < 3 ~ "HEART without MIND",
                         HEART < 3 & MIND > 3 ~ "MIND without HEART",
                         TRUE ~ "roughly equal"))

temp_squares_d2 %>%
  count(age_group, BvH) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()

temp_squares_d2 %>%
  count(age_group, BvM) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()

temp_squares_d2 %>%
  count(age_group, HvM) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()

bp = function(x, lev, n = 1e3, alpha=0.05) {
  res = replicate(n, sum(sample(x, length(x), replace=TRUE) == lev)/length(x))
  return(list(mean=mean(res),
              `95% CI`=quantile(res, c(0.5*alpha,1-0.5*alpha))))
}
bp(temp$group, "roughly equal")
bp(temp$group, "BODY without...")


temp_squares_d2 %>%
  select(age_group, subid, character, BvH:HvM) %>%
  gather(comparison, group, c(BvH:HvM)) %>%
  mutate(group = recode_factor(group,
                               "roughly equal" = "roughly equal",
                               "BODY without HEART" = "BODY without...",
                               "BODY without MIND" = "BODY without...",
                               "HEART without BODY" = "HEART without...",
                               "HEART without MIND" = "HEART without...",
                               "MIND without BODY" = "MIND without...",
                               "MIND without HEART" = "MIND without..."),
         comparison = recode_factor(comparison,
                                    "BvH" = "BODY vs. HEART",
                                    "BvM" = "BODY vs. MIND",
                                    "HvM" = "HEART vs. MIND")) %>%
  ggplot(aes(x = age_group, fill = group)) +
  facet_grid(cols = vars(comparison)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("gray90", "#e41a1c", "#377eb8", "#4daf4a")) +
  theme_bw() +
  labs(x = "age group", y = "proportion of responses",
       fill = "response tendency") +
  ylim(0, 0.4)





temp_squares_d3 <- d3_all_endorse %>%
  # filter(factor %in% c("BODY", "HEART")) %>%
  group_by(subid) %>%
  mutate(sum_endorse = prop_endorse * n) %>%
  ungroup() %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  mutate(BvH = case_when(BODY > 3 & HEART < 3 ~ "BODY without HEART",
                         BODY < 3 & HEART > 3 ~ "HEART without BODY",
                         TRUE ~ "roughly equal"),
         BvM = case_when(BODY > 3 & MIND < 3 ~ "BODY without MIND",
                         BODY < 3 & MIND > 3 ~ "MIND without BODY",
                         TRUE ~ "roughly equal"),
         HvM = case_when(HEART > 3 & MIND < 3 ~ "HEART without MIND",
                         HEART < 3 & MIND > 3 ~ "MIND without HEART",
                         TRUE ~ "roughly equal"))

temp_squares_d3 %>%
  count(age_group, BvH) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()

temp_squares_d3 %>%
  count(age_group, BvM) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()

temp_squares_d3 %>%
  count(age_group, HvM) %>%
  group_by(age_group) %>%
  mutate(`%` = round(n/sum(n), 2)*100) %>%
  ungroup()

