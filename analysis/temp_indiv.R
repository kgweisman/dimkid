library(lme4)

# temp <- d2 %>%
#   mutate(age_group = "children_79")

temp <- d3 %>% select(-trial.comments) %>%
  full_join(d4)

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
  mutate(endorse = recode(response,
                          "yes" = "endorse",
                          "kinda" = "endorse",
                          "no" = "reject")) %>%
  # filter(response != "no") %>%
  count(age_group, subid, factor, endorse) %>%
  complete(endorse, nesting(age_group, subid, factor), fill = list(n = 0)) %>%
  filter(endorse == "endorse",
         factor %in% c("body", "heart", "mind")) %>%
  spread(factor, n)

temp2 <- temp1 %>%
  mutate(HminB = heart - body,
         MminB = mind - body,
         MminH = mind - heart) %>%
  select(age_group, subid, HminB, MminB, MminH) %>%
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
                                       "goat", "elephant")),
         comparison = factor(comparison,
                             levels = c("HminB", "MminB", "MminH"))) %>%
  distinct() %>%
  filter(!is.na(comparison))


# compare distributions -----
# look at indices (qualitative)
comp_dist <- temp2 %>%
  group_by(comparison, age_group) %>%
  summarise(n = n(),
            median = median(diff),
            mean = round(mean(diff, na.rm = T), 2),
            var = round(var(diff, na.rm = T), 2),
            skewness = round(skew(diff), 2),
            kurtosis = round(kurtosi(diff), 2)) %>%
  data.frame()
comp_dist

# compare medians
wilcox.test(diff ~ age_group, temp2 %>% filter(comparison == "HminB"))
wilcox.test(diff ~ age_group, temp2 %>% filter(comparison == "MminB"))
wilcox.test(diff ~ age_group, temp2 %>% filter(comparison == "MminH"))

# compare means
t.test(diff ~ age_group, temp2 %>% filter(comparison == "HminB"))
t.test(diff ~ age_group, temp2 %>% filter(comparison == "MminB"))
t.test(diff ~ age_group, temp2 %>% filter(comparison == "MminH"))

# compare variances
bartlett.test(diff ~ age_group, temp2 %>% filter(comparison == "HminB"))
bartlett.test(diff ~ age_group, temp2 %>% filter(comparison == "MminB"))
bartlett.test(diff ~ age_group, temp2 %>% filter(comparison == "MminH"))

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

d_rect <- data.frame(comparison = c("HminB", "MminB", "MminH"),
                     xmin = 2.5,
                     xmax = 5.5,
                     ymin = 0,
                     ymax = 10)

ggplot(temp2 %>%
         full_join(comp_dist) %>%
         mutate(comparison = factor(comparison,
                                    labels = c("HEART minus BODY",
                                               "MIND minus BODY",
                                               "MIND minus HEART")),
                age_group = factor(age_group,
                                   levels = c("4-6y", "7-9y"),
                                   labels = c("4-6y", "7-9y"))), 
       aes(x = diff, fill = age_group)) +
  facet_wrap(~ comparison, ncol = 1) +
  # geom_density(alpha = 0.5) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  geom_vline(aes(xintercept = 0), lty = 2, color = "black") +
  # geom_vline(aes(xintercept = median, color = age_group), 
  #            lty = 1, size = 1) +
  # geom_vline(data = comp_dist,
  #            aes(xintercept = q1, color = age_group), 
  #            lty = 1) +
  # geom_vline(data = comp_dist,
  #            aes(xintercept = q3, color = age_group), 
  #            lty = 1) +
  scale_x_continuous(limits = c(-6, 6), breaks = -6:6) +
  labs(title = "Distributions of differences in endorsements\nbetween categories (BODY, HEART, and MIND)",
       fill = "Age group",
       color = "Age group",
       x = "Difference between factors",
       y = "Count") +
  annotate("text", 
           x = 6, y = 32, hjust = 1, vjust = 1,
           size = 6,
           label = c("more HEART\nthan BODY", 
                     "more MIND\nthan BODY", 
                     "more MIND\nthan HEART")) +
  annotate("text", 
           x = -6, y = 32, hjust = 0, vjust = 1,
           size = 6,
           label = c("more BODY\nthan HEART", 
                     "more BODY\nthan MIND", 
                     "more HEART\nthan MIND")) +
  theme_bw() +
  # theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 18))# + 
  # geom_rect(xmin = 2.5, xmax = 5.5, ymin = 0, ymax = 10,
  #           color = "black", fill = NA)

temp2 %>%
  filter(abs(diff) > 2) %>%
  mutate(sign = sign(diff)) %>%
  count(comparison, 
        age_group,
        sign,
        character)


# ggplot(temp2 %>%
#          ungroup() %>%
#          mutate(comparison = 
#                   factor(comparison,
#                          labels = c("BODY - HEART",
#                                     "BODY - MIND",
#                                     "HEART - MIND"))),
#        aes(x = age, y = diff, group = comparison,
#            color = age_group, fill = age_group)) +
#   facet_grid(~ comparison) +
#   geom_hline(aes(yintercept = 0), lty = 2) +
#   geom_violin(aes(group = age_group), alpha = 0.2, draw_quantiles = 0.5) +
#   geom_point(position = position_jitter(width = 0, height = 0.2),
#              # color = "black", 
#              size = 1) +
#   # geom_smooth(method = "loess", alpha = 0.1) +
#   scale_x_continuous(breaks = 4:10) +
#   scale_y_continuous(limits = c(-6, 6), breaks = -6:6) +
#   labs(title = "Differences in number of endorsements by factor (by age)",
#        # subtitle = "Counting both 'yes' and 'kinda' as endorsement; constrained to 6 mental capacities per factor",
#        fill = "Age group",
#        color = "Age group",
#        x = "Age (years)",
#        y = "Difference") +
#   annotate("text", 
#            x = 7, y = -6, size = 6, hjust = 0.5,
#            label = c("HEART without BODY", 
#                      "MIND without BODY", 
#                      "MIND without HEART")) +
#   annotate("text", 
#            x = 7, y = 6, size = 6, hjust = 0.5,
#            label = c("BODY without HEART", 
#                      "BODY without MIND", 
#                      "HEART without MIND")) +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 20))

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


# regressions ------

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
            family = "poisson", data = temp2 %>% filter(comparison == "HminB")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", data = temp2 %>% filter(comparison == "MminB")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", data = temp2 %>% filter(comparison == "MminH")))

contrasts(temp2$age_group) <- cbind(young_GM = c(1, -1))
summary(lmer(diff ~ comparison * age_group + (1 | subid),
             data = temp2))
# summary(lmer(abs(diff) ~ comparison * age_group + (1 | subid),
#              data = temp2))
summary(glmer(abs(diff) ~ comparison * age_group + (1 | subid),
            family = "poisson", data = temp2))
summary(glm(abs(diff) ~ age_group,
            family = "poisson", data = temp2 %>% filter(comparison == "HminB")))
summary(glm(abs(diff) ~ age_group,
            family = "poisson", data = temp2 %>% filter(comparison == "MminB")))
summary(glm(abs(diff) ~ age_group,
            family = "poisson", data = temp2 %>% filter(comparison == "MminH")))

# young kids only
# summary(glmer(abs(diff) ~ comparison * scale(age, scale = F) + (1 | subid), 
#               family = "poisson", 
#               data = temp2 %>% filter(age_group == "children_46")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_46", comparison == "HminB")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_46", comparison == "MminB")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_46", comparison == "MminH")))

# old kids only
summary(glmer(abs(diff) ~ comparison * scale(age, scale = F) + (1 | subid),
              family = "poisson",
              data = temp2 %>% filter(age_group == "children_79")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_79", comparison == "HminB")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_79", comparison == "MminB")))
summary(glm(abs(diff) ~ scale(age, scale = F),
            family = "poisson", 
            data = temp2 %>% 
              filter(age_group == "children_79", comparison == "MminH")))



temp1 %>% select(body, heart, mind) %>% cor()



# ----


ggplot(temp2 %>%
         filter(abs(diff) > 2) %>%
         mutate(sign = factor(sign(diff),
                              labels = c("negative difference", 
                                         "positive difference")),
                comparison = factor(comparison,
                                    labels = c("HEART minus BODY",
                                               "MIND minus BODY",
                                               "MIND minus HEART"))),
       aes(x = age_group, 
           fill = factor(character, levels = rev(levels(character))))) +
  facet_grid(comparison ~ sign) +
  # facet_wrap(~ comparison ~ sign, nrow = 1) +
  geom_bar(position = position_stack()) +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  ylim(0, 34) +
  labs(title = "Extreme difference scores (<-2 or >2)",
       x = "Age group",
       y = "Count",
       fill = "Target character") +
  theme_bw() +
  annotate("text", x = 1.5, y = 32, hjust = 0.5,
           label = c("more BODY than HEART",
                     "more HEART than BODY",
                     "more BODY than MIND",
                     "more MIND than BODY",
                     "more HEART than MIND",
                     "more MIND than HEART")) +
  theme(text = element_text(size = 14))

