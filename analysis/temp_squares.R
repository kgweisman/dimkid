temp_squares_d1 <- d1_all_endorse %>%
  # filter(factor %in% c("BODY", "HEART")) %>%
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





temp_squares_d3 <- d3_all_endorse %>%
  # filter(factor %in% c("BODY", "HEART")) %>%
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
