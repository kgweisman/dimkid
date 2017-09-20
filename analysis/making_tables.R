# PERCENTAGES ----

## Weisman et al (2016 CogSci, 2017 PNAS) ----

# proportions for robot study 1 (adults)
WDM2016_study1_prop <- d1 %>%
  filter(condition == "robot") %>%
  gather(capacity, response, happy:pride) %>%
  mutate(response_cat = recode_factor(response,
                                      "3" = "yes",
                                      "2" = "yes",
                                      "1" = "ambivalent",
                                      "0" = "ambivalent",
                                      "-1" = "ambivalent",
                                      "-2" = "no",
                                      "-3" = "no")) %>%
  group_by(capacity, response_cat) %>%
  count() %>%
  ungroup() %>%
  group_by(capacity) %>%
  mutate(prop = round(n/sum(n), 2),
         total_n = sum(n)) %>%
  arrange(capacity, response_cat) %>%
  filter(!is.na(response_cat)) %>%
  select(-n) %>%
  spread(response_cat, prop) %>%
  mutate(yes = ifelse(is.na(yes), 0, yes),
         ambivalent = ifelse(is.na(ambivalent), 0, ambivalent),
         no = ifelse(is.na(no), 0, no))
# check_total = yes + ambivalent + no)

# proportions for robot study 2 (adults)
WDM2016_study2_prop <- d2 %>%
  filter(condition == "robot") %>%
  gather(capacity, response, happy:pride) %>%
  mutate(response_cat = recode_factor(response,
                                      "3" = "yes",
                                      "2" = "yes",
                                      "1" = "ambivalent",
                                      "0" = "ambivalent",
                                      "-1" = "ambivalent",
                                      "-2" = "no",
                                      "-3" = "no")) %>%
  group_by(capacity, response_cat) %>%
  count() %>%
  ungroup() %>%
  group_by(capacity) %>%
  mutate(prop = round(n/sum(n), 2),
         total_n = sum(n)) %>%
  arrange(capacity, response_cat) %>%
  filter(!is.na(response_cat)) %>%
  select(-n) %>%
  spread(response_cat, prop) %>%
  mutate(yes = ifelse(is.na(yes), 0, yes),
         ambivalent = ifelse(is.na(ambivalent), 0, ambivalent),
         no = ifelse(is.na(no), 0, no))
# check_total = yes + ambivalent + no)

# proportions for robot study 3 (adults, within)
WDM2016_study3_prop <- d3 %>%
  filter(target == "robot") %>%
  gather(capacity, response, happy:pride) %>%
  mutate(response_cat = recode_factor(response,
                                      "3" = "yes",
                                      "2" = "yes",
                                      "1" = "ambivalent",
                                      "0" = "ambivalent",
                                      "-1" = "ambivalent",
                                      "-2" = "no",
                                      "-3" = "no")) %>%
  group_by(capacity, response_cat) %>%
  count() %>%
  ungroup() %>%
  group_by(capacity) %>%
  mutate(prop = round(n/sum(n), 2),
         total_n = sum(n)) %>%
  arrange(capacity, response_cat) %>%
  filter(!is.na(response_cat)) %>%
  select(-n) %>%
  spread(response_cat, prop) %>%
  mutate(yes = ifelse(is.na(yes), 0, yes),
         ambivalent = ifelse(is.na(ambivalent), 0, ambivalent),
         no = ifelse(is.na(no), 0, no))
# check_total = yes + ambivalent + no)


## Weisman et al (2017 CogSci) ---- 

# proportions for robot study 1 (adults)
WDM2017_adults_prop <- d1 %>%
  filter(character == "robot") %>%
  group_by(capacity, response) %>%
  count() %>%
  ungroup() %>%
  group_by(capacity) %>%
  mutate(prop = round(n/sum(n), 2),
         total_n = sum(n),
         response = factor(response, levels = c("yes", "kinda", "no"))) %>%
  arrange(capacity, response) %>%
  filter(!is.na(response)) %>%
  select(-n) %>%
  spread(response, prop) %>%
  mutate(yes = ifelse(is.na(yes), 0, yes),
         kinda = ifelse(is.na(kinda), 0, kinda),
         no = ifelse(is.na(no), 0, no))
         # check_total = yes + kinda + no)

# proportions for robot study 2 (7-9 years)
WDM2017_children_prop <- d2 %>%
  filter(character == "robot") %>%
  group_by(capacity, response) %>%
  count() %>%
  ungroup() %>%
  group_by(capacity) %>%
  mutate(prop = round(n/sum(n), 2),
         total_n = sum(n),
         response = factor(response, levels = c("yes", "kinda", "no"))) %>%
  arrange(capacity, response) %>%
  filter(!is.na(response)) %>%
  select(-n) %>%
  spread(response, prop) %>%
  mutate(yes = ifelse(is.na(yes), 0, yes),
         kinda = ifelse(is.na(kinda), 0, kinda),
         no = ifelse(is.na(no), 0, no))
         # check_total = yes + kinda + no)


# MEANS ----

## Weisman et al (2016 CogSci, 2017 PNAS) ----

# means for robot study 1 (adults)
WDM2016_study1_mean <- d1 %>%
  filter(condition == "robot") %>%
  gather(capacity, response, happy:pride) %>%
  group_by(capacity) %>%
  summarise(n = length(response),
            # mean = round(mean(response, na.rm = T), 2),
            mean_corr = round((mean(response, na.rm = T) + 3)/6, 2)) %>%
  arrange(capacity)

# means for robot study 2 (adults)
WDM2016_study2_mean <- d2 %>%
  filter(condition == "robot") %>%
  gather(capacity, response, happy:pride) %>%
  group_by(capacity) %>%
  summarise(n = length(response),
            # mean = round(mean(response, na.rm = T), 2),
            mean_corr = round((mean(response, na.rm = T) + 3)/6, 2)) %>%
  arrange(capacity)

# means for robot study 3 (adults, within)
WDM2016_study3_mean <- d3 %>%
  filter(target == "robot") %>%
  gather(capacity, response, happy:pride) %>%
  group_by(capacity) %>%
  summarise(n = length(response),
            # mean = round(mean(response, na.rm = T), 2),
            mean_corr = round((mean(response, na.rm = T) + 3)/6, 2)) %>%
  arrange(capacity)


## Weisman et al (2017 CogSci) ---- 

# means for robot study 1 (adults)
WDM2017_adults_mean <- d1 %>%
  filter(character == "robot") %>%
  group_by(capacity) %>%
  summarise(n = length(responseNum),
            mean = round(mean(responseNum, na.rm = T), 2)) %>%
  arrange(capacity)

# means for robot study 2 (7-9 years)
WDM2017_children_mean <- d2 %>%
  filter(character == "robot") %>%
  group_by(capacity) %>%
  summarise(n = length(responseNum),
            mean = round(mean(responseNum, na.rm = T), 2)) %>%
  arrange(capacity)


# STICH TOGETHER ------

WDM_prop_table <- WDM2016_study1_prop %>% mutate(study = "WDM2016_study1") %>%
  full_join(WDM2016_study2_prop %>% mutate(study = "WDM2016_study2")) %>%
  full_join(WDM2016_study3_prop %>% mutate(study = "WDM2016_study3")) %>%
  full_join(WDM2017_adults_prop %>% 
              rename(ambivalent = kinda) %>% mutate(study = "WDM2017_adults")) %>%
  full_join(WDM2017_children_prop %>% 
              rename(ambivalent = kinda) %>% mutate(study = "WDM2017_children")) %>%
  mutate(age_group = ifelse(grepl("children", study), "children", "adults"))

WDM_mean_table <- WDM2016_study1_mean %>% mutate(study = "WDM2016_study1") %>%
  full_join(WDM2016_study2_mean %>% mutate(study = "WDM2016_study2")) %>%
  full_join(WDM2016_study3_mean %>% mutate(study = "WDM2016_study3")) %>%
  rename(mean = mean_corr) %>%
  full_join(WDM2017_adults_mean %>% mutate(study = "WDM2017_adults")) %>%
  full_join(WDM2017_children_mean %>% mutate(study = "WDM2017_children")) %>%
  mutate(age_group = ifelse(grepl("children", study), "children", "adults"))
