library(lme4)

temp <- d2 %>%
  mutate(age_group = "children_79")

# temp <- d3 %>% select(-trial.comments) %>%
#   full_join(d4)

temp1 <- temp %>%
  mutate(factor = recode_factor(capacity,
                                "hungry" = "body",
                                "odors" = "body",
                                "fear" = "body",
                                "pain" = "body",
                                "tired" = "body",
                                "nauseated" = "body",
                                "guilt" = "heart",
                                "embarrassed" = "heart",
                                "pride" = "heart",
                                "disrespected" = "heart",
                                "depressed" = "heart",
                                "love" = "heart",
                                # "angry" = "heart",
                                # "happy" = "heart",
                                "reasoning" = "mind",
                                "choices" = "mind",
                                "remembering" = "mind",
                                "temperature" = "mind",
                                "depth" = "mind",
                                "conscious" = "mind"),
         age_group = factor(age_group,
                            levels = c("children_46", "children_79"),
                            labels = c("4-6y", "7-9y"))) %>%
  # count both "yes" and "kinda" as YES
  filter(response != "no") %>%
  count(age_group, subid, factor) %>%
  complete(factor, nesting(age_group, subid)) %>%
  filter(factor %in% c("body", "heart", "mind")) %>%
  replace_na(list(n = 0)) %>%
  spread(factor, n)

temp2 <- temp1 %>%
  mutate(BminH = body - heart,
         BminM = body - mind,
         HminM = heart - mind) %>%
  select(age_group, subid, BminH, BminM, HminM) %>%
  gather(comparison, diff, -age_group, -subid) %>%
  mutate_at(vars(comparison, age_group), funs(factor)) %>%
  left_join(d2 %>% select(subid, age, character) %>%
              full_join(d3 %>% select(subid, age, character)) %>%
              full_join(d4 %>% select(subid, age, character)) %>%
              distinct()) %>%
  mutate(character = factor(gsub("_", " ", character),
                            levels = c("computer", "robot",
                                       "doll", "teddy bear",
                                       "beetle", "bird", "mouse",
                                       "goat", "elephant"))) %>%
  distinct() %>%
  filter(!is.na(comparison))

ggplot(temp2 %>%
         mutate(comparison = 
                  factor(comparison,
                         labels = c("BODY endorsements minus HEART endorsements",
                                    "BODY endorsements minus MIND endorsements",
                                    "HEART endorsements minus MIND endorsements"))), 
       aes(x = diff, fill = age_group)) +
  facet_wrap(~ comparison, ncol = 3) +
  # geom_density(alpha = 0.5) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  scale_x_continuous(limits = c(-6, 6), breaks = -6:6) +
  labs(title = "Co-occurence of endorsements: Distributions of differences in number of endorsed mental capacities between factors (by participant)",
       fill = "Age group",
       x = "Difference between factors (theoretical range: -6 to +6)",
       y = "Count") +
  annotate("text", 
           x = -6, y = 30, hjust = 0, vjust = 1,
           label = c("HEART\nwithout\nBODY", 
                     "MIND\nwithout\nBODY", 
                     "MIND\nwithout\nHEART")) +
  annotate("text", 
           x = 6, y = 30, hjust = 1, vjust = 1,
           label = c("BODY\nwithout\nHEART", 
                     "BODY\nwithout\nMIND", 
                     "HEART\nwithout\nMIND")) +
  theme_bw() +
  theme(legend.position = "right")

ggplot(temp2 %>%
         ungroup() %>%
         mutate(comparison = 
                  factor(comparison,
                         labels = c("BODY - HEART",
                                    "BODY - MIND",
                                    "HEART - MIND"))),
       aes(x = age, y = diff, group = comparison,
           color = age_group, fill = age_group)) +
  facet_grid(~ comparison) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_violin(aes(group = age_group), alpha = 0.2, draw_quantiles = 0.5) +
  geom_point(position = position_jitter(width = 0, height = 0.2),
             # color = "black", 
             size = 1) +
  # geom_smooth(method = "loess", alpha = 0.1) +
  scale_x_continuous(breaks = 4:10) +
  scale_y_continuous(limits = c(-6, 6), breaks = -6:6) +
  labs(title = "Differences in number of endorsements by factor (by age)",
       # subtitle = "Counting both 'yes' and 'kinda' as endorsement; constrained to 6 mental capacities per factor",
       fill = "Age group",
       color = "Age group",
       x = "Age (years)",
       y = "Difference") +
  annotate("text", 
           x = 7, y = -6, size = 6, hjust = 0.5,
           label = c("HEART without BODY", 
                     "MIND without BODY", 
                     "MIND without HEART")) +
  annotate("text", 
           x = 7, y = 6, size = 6, hjust = 0.5,
           label = c("BODY without HEART", 
                     "BODY without MIND", 
                     "HEART without MIND")) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggplot(temp2 %>%
         ungroup() %>%
         mutate(comparison = 
                  factor(comparison,
                         labels = c("BODY endorsements vs. HEART endorsements",
                                    "BODY endorsements vs. MIND endorsements",
                                    "HEART endorsements vs. MIND endorsements"))),
       aes(x = age, y = diff, group = comparison,
           color = age_group, fill = age_group)) +
  facet_grid(~ comparison) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_point(position = position_jitter(width = 0, height = 0.2), alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2) +
  scale_x_continuous(breaks = 4:10) +
  scale_y_continuous(limits = c(-6.2, 6.2), breaks = -6:6) +
  labs(title = "Absolute differences in number of endorsements by factor (by age)",
       subtitle = "Counting both 'yes' and 'kinda' as endorsement; constrained to 6 mental capacities per factor",
       fill = "Age group",
       color = "Age group",
       x = "Age (years)",
       y = "Absolute difference") +
  theme_bw() 

ggplot(temp2 %>%
         ungroup() %>%
         mutate(comparison = 
                  factor(comparison,
                         labels = c("BODY endorsements vs. HEART endorsements",
                                    "BODY endorsements vs. MIND endorsements",
                                    "HEART endorsements vs. MIND endorsements"))),
       aes(x = age, y = abs(diff), group = comparison,
           color = age_group, fill = age_group)) +
  facet_grid(~ comparison) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_point(position = position_jitter(width = 0, height = 0.2), alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2) +
  scale_x_continuous(breaks = 4:10) +
  scale_y_continuous(limits = c(-0.2, 6.2), breaks = -6:6) +
  labs(title = "Absolute differences in number of endorsements by factor (by age)",
       subtitle = "Counting both 'yes' and 'kinda' as endorsement; constrained to 6 mental capacities per factor",
       fill = "Age group",
       color = "Age group",
       x = "Age (years)",
       y = "Absolute difference") +
  theme_bw() 


# contrasts(temp2$comparison) <- cbind(BvH_BvM = c(1, 0, 0),
#                                      HvM_BvM = c(0, 0, 1))
contrasts(temp2$comparison) <- cbind(BvH_GM = c(1, -1, 0),
                                     HvM_GM = c(0, -1, 1))
summary(lmer(diff ~ comparison * scale(age, scale = F) + (1 | subid),
             data = temp2))
# summary(lmer(abs(diff) ~ comparison * scale(age, scale = F) + (1 | subid), 
#              data = temp2))
summary(glmer(abs(diff) ~ comparison * scale(age, scale = F) + (1 | subid), 
            family = "poisson", data = temp2))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", data = temp2 %>% filter(comparison == "BminH")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", data = temp2 %>% filter(comparison == "BminM")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", data = temp2 %>% filter(comparison == "HminM")))

contrasts(temp2$age_group) <- cbind(young_GM = c(1, -1))
summary(lmer(diff ~ comparison * age_group + (1 | subid),
             data = temp2))
# summary(lmer(abs(diff) ~ comparison * age_group + (1 | subid),
#              data = temp2))
summary(glmer(abs(diff) ~ comparison * age_group + (1 | subid),
            family = "poisson", data = temp2))
summary(glm(abs(diff) ~ age_group,
            family = "poisson", data = temp2 %>% filter(comparison == "BminH")))
summary(glm(abs(diff) ~ age_group,
            family = "poisson", data = temp2 %>% filter(comparison == "BminM")))
summary(glm(abs(diff) ~ age_group,
            family = "poisson", data = temp2 %>% filter(comparison == "HminM")))

# young kids only
# summary(glmer(abs(diff) ~ comparison * scale(age, scale = F) + (1 | subid), 
#               family = "poisson", 
#               data = temp2 %>% filter(age_group == "children_46")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_46", comparison == "BminH")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_46", comparison == "BminM")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_46", comparison == "HminM")))

# old kids only
summary(glmer(abs(diff) ~ comparison * scale(age, scale = F) + (1 | subid),
              family = "poisson",
              data = temp2 %>% filter(age_group == "children_79")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_79", comparison == "BminH")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_79", comparison == "BminM")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_79", comparison == "HminM")))



temp1 %>% select(body, heart, mind) %>% cor()


# compare distributions
# look at indices (qualitative)
temp2 %>%
  group_by(comparison, age_group) %>%
  summarise(mean = mean(diff, na.rm = T),
            sd = sd(diff, na.rm = T),
            median = median(diff),
            skewness = skew(diff),
            kurtosis = kurtosi(diff))

# compare means
t.test(diff ~ age_group, temp2 %>% filter(comparison == "BminH"))
t.test(diff ~ age_group, temp2 %>% filter(comparison == "BminM"))
t.test(diff ~ age_group, temp2 %>% filter(comparison == "HminM"))

# compare variances
bartlett.test(diff ~ age_group, temp2 %>% filter(comparison == "BminH"))
bartlett.test(diff ~ age_group, temp2 %>% filter(comparison == "BminM"))
bartlett.test(diff ~ age_group, temp2 %>% filter(comparison == "HminM"))

# bootstrap all comparisons
# multi_boot(data = temp2,
#            summary_function = "mean",
#            column = "diff",
#            summary_groups = c("comparison", "age_group"),
#            statistics_functions = c("ci_lower", "mean", "ci_upper"))
# 
# multi_boot(data = temp2,
#            summary_function = "var",
#            column = "diff",
#            summary_groups = c("comparison", "age_group"),
#            statistics_functions = c("ci_lower", "mean", "ci_upper"))
# 
# multi_boot(data = temp2,
#            summary_function = "median",
#            column = "diff",
#            summary_groups = c("comparison", "age_group"),
#            statistics_functions = c("ci_lower", "mean", "ci_upper"))
# 
# multi_boot(data = temp2,
#            summary_function = "skew",
#            column = "diff",
#            summary_groups = c("comparison", "age_group"),
#            statistics_functions = c("ci_lower", "mean", "ci_upper"))
# 
# multi_boot(data = temp2,
#            summary_function = "kurtosi",
#            column = "diff",
#            summary_groups = c("comparison", "age_group"),
#            statistics_functions = c("ci_lower", "mean", "ci_upper"))
