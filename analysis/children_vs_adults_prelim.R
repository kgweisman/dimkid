# PRELIMINARIES ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stats)
library(psych)
library(ggplot2)
library(tibble)
library(GPArotation)
library(langcog)

# clear environment
rm(list=ls())
graphics.off()

# READ IN DATA ----------------------------------------------------------------

# ADULT run 01 (3-point scale)
d_adult01 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-01_2016-06-05_anonymized.csv") %>% select(-X)

# # ADULT run 02 (7-point scale)
# d_adult02 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-02_2016-07-19_anonymized.csv")
# 
# # ADULT run 03 (3-point scale, original wording for 'free will' and 'intentions')
# d_adult03 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-03_2016-12-08_anonymized.csv")

# CHILD run 01 [lydia, olivia, allie (summer 2016) + nicky, dru, ariel, olivia (fall 2016) + campbell (winter 2017)]
d_child01 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2017-01-13_anonymized.csv") %>% select(-X.1, -X)

# TIDY DATA -------------------------------------------------------------------

# combine datasets
d <- d_adult01 %>%
  select(subid, charName, trialNum, capacity, capWording, 
         hoverTime, rt, response, responseNum) %>%
  rename(character = charName) %>%
  mutate(ageGroup = "adult") %>%
  full_join(d_child01 %>% 
              filter(is.na(age) | (age >= 7 & age <10)) %>%
              select(subid, character, trialNum, capacity, capWording, 
                     hoverTime, rt, response, responseNum) %>%
              mutate(ageGroup = "child")) %>%
  mutate(subid = factor(subid),
         character = factor(character),
         capWording = factor(capWording),
         responseCat = factor(response, levels = c("no", "kinda", "yes")),
         ageGroup = factor(ageGroup, levels = c("adult", "child")))

# filter by condition (no elephant!)
d0 <- d %>%
  filter(character %in% c("beetle", "robot"))

# examine and filter by RTs
ggplot(d0) +
  geom_histogram(aes(x = rt), bins = 100) +
  facet_wrap(~ageGroup) +
  scale_x_log10(breaks = seq(0, 1000, 100)) +
  geom_vline(xintercept = 350, color = "red")

d1 <- d0 %>%
  filter(rt >= 250)

# finish tidying
d2 <- d1 %>%
  select(capacity, responseNum, subid) %>%
  filter(!is.na(capacity), capacity != "na") %>%
  spread(capacity, responseNum)

d3 <- data.frame(d2[,-1], row.names = d2[,1])

cor3 <- cor(d3, method = "spearman", use = "complete.obs")

# EFA and PLOT MEAN RESPONSES BY FACTOR ---------------------------------------

# set correlation type: pearson or polychoric?
cor_type <- "cor"
# cor_type <- "poly"

# set rotation type: varimax or oblimin?
rot_type <- "varimax"
# rot_type <- "oblimin"

# make adult (first 3 factors from rotated maximal solution) 
d3_adult <- d3 %>%
  rownames_to_column(var = "subid") %>%
  left_join(d %>% select(subid, ageGroup)) %>%
  filter(ageGroup == "adult") %>%
  select(-subid, -ageGroup)

factors_adult <- fa.sort(fa(d3_adult,
                            nfactors = 3,
                            rotate = rot_type,
                            cor = cor_type)$loadings[]) %>%
  data.frame() %>%
  select(1:3) %>%
  rownames_to_column(var = "capacity")

colnames(factors_adult)[2:4] <- c("F1", "F2", "F3")

factors_adult <- factors_adult %>%
  mutate(F1_abs = abs(F1),
         F2_abs = abs(F2),
         F3_abs = abs(F3),
         loading_abs = pmax(F1_abs, F2_abs, F3_abs),
         loading = ifelse(loading_abs == abs(F1), F1,
                          ifelse(loading_abs == abs(F2), F2,
                                 ifelse(loading_abs == abs(F3), F3,
                                        NA))),
         factor = ifelse(loading == F1, "F1",
                         ifelse(loading == F2, "F2",
                                ifelse(loading == F3, "F3",
                                       NA))),
         factorName = ifelse(loading == F1, "Factor 1",
                             ifelse(loading == F2, "Factor 2",
                                    ifelse(loading == F3, "Factor 3",
                                           NA)))) %>%
  arrange(factor, desc(loading_abs)) %>%
  select(capacity, factor, factorName, loading, loading_abs)

factors_adult <- 
  full_join(factors_adult %>%
              filter(factor == "F1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors_adult %>%
              filter(factor == "F2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors_adult %>%
              filter(factor == "F3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         textColor = ifelse(loading < 0, "dodgerblue3", "black"))

factors_adult <- factors_adult %>%
  left_join(d %>% select(capacity, capWording) %>% distinct()) %>%
  mutate(capWordingShort = gsub(" --.*", "", capWording)) %>%
  select(-capWording) %>%
  rename(capWording = capWordingShort) %>%
  distinct()

# make child (first 3 factors from rotated maximal solution) 
d3_child <- d3 %>%
  rownames_to_column(var = "subid") %>%
  left_join(d %>% select(subid, ageGroup)) %>%
  filter(ageGroup == "child") %>%
  select(-subid, -ageGroup)

factors_child <- fa.sort(fa(d3_child,
                            nfactors = 3,
                            rotate = rot_type,
                            cor = cor_type)$loadings[]) %>%
  data.frame() %>%
  select(1:3) %>%
  rownames_to_column(var = "capacity")

colnames(factors_child)[2:4] <- c("F1", "F2", "F3")

factors_child <- factors_child %>%
  mutate(F1_abs = abs(F1),
         F2_abs = abs(F2),
         F3_abs = abs(F3),
         loading_abs = pmax(F1_abs, F2_abs, F3_abs),
         loading = ifelse(loading_abs == abs(F1), F1,
                          ifelse(loading_abs == abs(F2), F2,
                                 ifelse(loading_abs == abs(F3), F3,
                                        NA))),
         factor = ifelse(loading == F1, "F1",
                         ifelse(loading == F2, "F2",
                                ifelse(loading == F3, "F3",
                                       NA))),
         factorName = ifelse(loading == F1, "Factor 1",
                             ifelse(loading == F2, "Factor 2",
                                    ifelse(loading == F3, "Factor 3",
                                           NA)))) %>%
  arrange(factor, desc(loading_abs)) %>%
  select(capacity, factor, factorName, loading, loading_abs)

factors_child <- 
  full_join(factors_child %>%
              filter(factor == "F1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors_child %>%
              filter(factor == "F2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors_child %>%
              filter(factor == "F3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         textColor = ifelse(loading < 0, "dodgerblue3", "black"))

factors_child <- factors_child %>%
  left_join(d %>% select(capacity, capWording) %>% distinct()) %>%
  mutate(capWordingShort = gsub(" --.*", "", capWording)) %>%
  select(-capWording) %>%
  rename(capWording = capWordingShort) %>%
  distinct()

# by condition
d1_bycond <- d0 %>%
  select(character, capacity, capWording, responseNum, subid, ageGroup) %>%
  filter(character != "elephant") %>%
  filter(capacity != "na", is.na(responseNum) == F) %>%
  mutate(capWording = gsub(" --.*", "", capWording))

# make df for plotting
d1_bycond_mb <- multi_boot(d1_bycond,
                           column = "responseNum",
                           summary_groups = c("ageGroup", "character", "capacity", "capWording"),
                           statistics_functions = c("mean", "ci_lower", "ci_upper"))

d1_bycond_mb_factorsAdult <- d1_bycond_mb %>% 
  full_join(factors_adult) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order)

d1_bycond_mb_factorsChild <- d1_bycond_mb %>% 
  full_join(factors_child) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order)

# plot: by adult factors
ggplot(d1_bycond_mb_factorsAdult, 
       aes(x = desc(order*2), y = mean,
           group = character, color = character, shape = character,
           label = capWording)) +
  facet_grid(ageGroup ~ factorName) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
  scale_shape_manual(values = c(19, 15)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = .75), width = 0) +
  geom_text(aes(y = -0.18, hjust = 0), color = d1_bycond_mb_factorsAdult$textColor, size = 6) +
  labs(title = "Responses, by adult-derived factors",
       y = "\nMean response (0 = NO, 0.5 = KINDA, 1 = YES)",
       x = "Capacity\n",
       color = "Character: ", shape = "Character: ") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 24),
        # axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top")

# plot: by child factors
ggplot(d1_bycond_mb_factorsChild, 
       aes(x = desc(order*2), y = mean,
           group = character, color = character, shape = character,
           label = capWording)) +
  facet_grid(ageGroup ~ factorName) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
  scale_shape_manual(values = c(19, 15)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = .75), width = 0) +
  geom_text(aes(y = -0.18, hjust = 0), color = d1_bycond_mb_factorsChild$textColor, size = 6) +
  labs(title = "Responses, by child-derived factors",
       y = "\nMean response (0 = NO, 0.5 = KINDA, 1 = YES)",
       x = "Capacity\n",
       color = "Character: ", shape = "Character: ") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 24),
        # axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top")

# combine adults and children
# fa.parallel(d3)
fa(d3, nfactors = 13, rotate = "none", cor = cor_type)

factors_all <- fa.sort(fa(d3,
                          nfactors = 3,
                          rotate = rot_type,
                          cor = cor_type)$loadings[]) %>%
  data.frame() %>%
  select(1:3) %>%
  rownames_to_column(var = "capacity")

colnames(factors_all)[2:4] <- c("F1", "F2", "F3")

factors_all <- factors_all %>%
  mutate(F1_abs = abs(F1),
         F2_abs = abs(F2),
         F3_abs = abs(F3),
         loading_abs = pmax(F1_abs, F2_abs, F3_abs),
         loading = ifelse(loading_abs == abs(F1), F1,
                          ifelse(loading_abs == abs(F2), F2,
                                 ifelse(loading_abs == abs(F3), F3,
                                        NA))),
         factor = ifelse(loading == F1, "F1",
                         ifelse(loading == F2, "F2",
                                ifelse(loading == F3, "F3",
                                       NA))),
         factorName = ifelse(loading == F1, "Factor 1",
                             ifelse(loading == F2, "Factor 2",
                                    ifelse(loading == F3, "Factor 3",
                                           NA)))) %>%
  arrange(factor, desc(loading_abs)) %>%
  select(capacity, factor, factorName, loading, loading_abs)

factors_all <- 
  full_join(factors_all %>%
              filter(factor == "F1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors_all %>%
              filter(factor == "F2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors_all %>%
              filter(factor == "F3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         textColor = ifelse(loading < 0, "dodgerblue3", "black"))

factors_all <- factors_all %>%
  left_join(d %>% select(capacity, capWording) %>% distinct()) %>%
  mutate(capWordingShort = gsub(" --.*", "", capWording)) %>%
  select(-capWording) %>%
  rename(capWording = capWordingShort) %>%
  distinct()

# make df for plotting
d1_bycond_mb_factorsAll <- d1_bycond_mb %>% 
  full_join(factors_all) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order)

# plot: by all factors
ggplot(d1_bycond_mb_factorsAll, 
       aes(x = desc(order*2), y = mean,
           group = character, color = character, shape = character,
           label = capWording)) +
  facet_grid(ageGroup ~ factorName) +
  # facet_grid(factorName ~ ageGroup) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
  scale_shape_manual(values = c(19, 15)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = .75), width = 0) +
  geom_text(aes(y = -0.18, hjust = 0), color = d1_bycond_mb_factorsAll$textColor, size = 6) +
  labs(title = "Responses, by all-derived factors",
       y = "\nMean response (0 = NO, 0.5 = KINDA, 1 = YES)",
       x = "Capacity\n",
       color = "Character: ", shape = "Character: ") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 24),
        # axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top")

# REGRESSION ON FACTOR SCORES -------------------------------------------------

# set score type
score_type <- "regression"
# score_type <- "Thurstone"
# score_type <- "tenBerge"
# score_type <- "Anderson"
# score_type <- "Bartlett"
# score_type <- "Harman"

scores_all <- fa(d3, nfactors = 13, rotate = rot_type,
                 cor = cor_type, scores = score_type)$scores %>%
  data.frame() %>%
  select(1:3) %>%
  rownames_to_column(var = "subid") %>%
  mutate(ageGroup = factor(ifelse(grepl("run", subid), "adult", "child")))

colnames(scores_all)[2:4] <- c("score_F1", "score_F2", "score_F3")

d_reg2 <- d0 %>%
  select(subid, ageGroup, character) %>%
  filter(character != "elephant") %>%
  distinct() %>%
  full_join(scores_all) %>%
  mutate(character = factor(character)) %>%
  filter(!is.na(score_F1), !is.na(score_F2), !is.na(score_F3), !is.na(ageGroup)) %>%
  gather(factor, score, starts_with("score_")) %>%
  mutate(factor = factor(factor)) %>%
  multi_boot(column = "score",
             summary_groups = c("ageGroup", "character", "factor"),
             statistics_functions = c("mean", "ci_lower", "ci_upper"))

# plot
ggplot(d_reg2 %>%
         ungroup() %>%
         mutate(factor = factor(factor,
                                labels = c("Social-emotional",
                                           "Physiological",
                                           "Perceptual-cognitive")),
                ageGroup = factor(ageGroup,
                                  levels = c("child", "adult"),
                                  labels = c("children", "adults"))),
       aes(x = ageGroup, y = mean, color = character, shape = character)) +
  facet_wrap("factor", ncol = 3) +
  theme_bw() +
  theme(text = element_text(size = 28),
        legend.position = "bottom") +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_shape_manual(values = c(19, 15)) +
  labs(title = "Factor scores by age group",
       # subtitle = "Adults (Study 1) vs. children (Study 2)\n",
       x = "Age group",
       y = "Mean factor score") # 1000 by 500

# analyze
d_reg3 <- d0 %>%
  select(subid, ageGroup, character) %>%
  filter(character != "elephant") %>%
  distinct() %>%
  left_join(scores_all) %>%
  mutate(character = factor(character)) %>%
  filter(!is.na(score_F1) & !is.na(score_F2) & !is.na(score_F3), !is.na(ageGroup)) %>%
  gather(factor, score, starts_with("score_")) %>%
  mutate(factor = factor(factor))

# set contrasts
contrasts(d_reg3$factor) <- cbind(F1 = c(1, -1, 0), # MAKE SURE TO DOUBLE-CHECK!!
                                  F3 = c(0, -1, 1))
contrasts(d_reg3$character) <- cbind(robot = c(-1, 1))
contrasts(d_reg3$ageGroup) <- cbind(child = c(-1, 1))

r1 <- lmer(score ~ character * factor + (1 | subid), d_reg3)
r2 <- lmer(score ~ character * factor + ageGroup + (1 | subid), d_reg3)
r3 <- lmer(score ~ character * factor * ageGroup + (1 | subid), d_reg3)
anova(r1, r2, r3)
# summary(r1)
# summary(r2)
summary(r3)

round(summary(r3)$coefficients, 2) %>% data.frame() %>% View()

# robot only
robot_r1 <- lmer(score ~ factor + (1 | subid), d_reg3 %>% filter(character == "robot"))
robot_r2 <- lmer(score ~ factor + ageGroup + (1 | subid), d_reg3 %>% filter(character == "robot"))
robot_r3 <- lmer(score ~ factor * ageGroup + (1 | subid), d_reg3 %>% filter(character == "robot"))
anova(robot_r1, robot_r2, robot_r3)
# summary(robot_r1)
# summary(robot_r2)
summary(robot_r3)

# children only (AGE: robot and beetle)
d_reg3_child <- d_reg3 %>%
  filter(ageGroup == "child") %>%
  left_join(d_child01 %>% select(subid, age)) %>%
  filter(!is.na(age)) %>%
  distinct()

contrasts(d_reg3_child$factor) <- cbind(F1 = c(1, -1, 0), # MAKE SURE TO DOUBLE-CHECK!!
                                        F3 = c(0, -1, 1))
contrasts(d_reg3_child$character) <- cbind(robot = c(-1, 1))

r1_child <- lmer(score ~ character * factor + (1 | subid), d_reg3_child)
r2_child <- lmer(score ~ character * factor + scale(age) + (1 | subid), d_reg3_child)
r3_child <- lmer(score ~ character * factor * scale(age) + (1 | subid), d_reg3_child)
anova(r1_child, r2_child, r3_child) # r3
# summary(r1_child)
# summary(r2_child)
summary(r3_child)

r4_child <- lmer(score ~ character * factor * poly(age, 1) + (1 | subid), d_reg3_child)
r5_child <- lmer(score ~ character * factor * poly(age, 2) + (1 | subid), d_reg3_child)
r6_child <- lmer(score ~ character * factor * poly(age, 3) + (1 | subid), d_reg3_child)
anova(r4_child, r5_child, r6_child) # r4
summary(r4_child)
# summary(r5_child)
# summary(r6_child)

round(summary(r7_child)$coefficients, 2) %>% data.frame() %>% View()

ggplot(d_reg3_child %>%
         ungroup() %>%
         mutate(factor = factor(factor,
                                labels = c("Social-emotional",
                                           "Physiological",
                                           "Perceptual-cognitive"))),
       aes(x = age, y = score, color = character, fill = character, shape = character)) +
  facet_wrap("factor", ncol = 3) +
  theme_bw() +
  theme(text = element_text(size = 28),
        legend.position = "bottom") +
  geom_smooth(method = "lm", alpha = 0.4) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(19, 15)) +
  labs(title = "Factor scores by children's age",
       # subtitle = "Children (Study 2)\n",
       x = "Age (years)",
       y = "Factor score") # 1000 by 500

# adults only (AGE: robot and beetle)
d_reg3_adult <- d_reg3 %>%
  filter(ageGroup == "adult") %>%
  left_join(d_adult01 %>% select(subid, age)) %>%
  filter(!is.na(age)) %>%
  distinct()

contrasts(d_reg3_adult$factor) <- cbind(F1 = c(1, -1, 0), # MAKE SURE TO DOUBLE-CHECK!!
                                        F3 = c(0, -1, 1))
contrasts(d_reg3_adult$character) <- cbind(robot = c(-1, 1))

r1_adult <- lmer(score ~ character * factor + (1 | subid), d_reg3_adult)
r2_adult <- lmer(score ~ character * factor + scale(age) + (1 | subid), d_reg3_adult)
r3_adult <- lmer(score ~ character * factor * scale(age) + (1 | subid), d_reg3_adult)
anova(r1_adult, r2_adult, r3_adult) # r3
# summary(r1_adult)
# summary(r2_adult)
summary(r3_adult)

r4_adult <- lmer(score ~ character * factor * poly(age, 1) + (1 | subid), d_reg3_adult)
r5_adult <- lmer(score ~ character * factor * poly(age, 2) + (1 | subid), d_reg3_adult)
r6_adult <- lmer(score ~ character * factor * poly(age, 3) + (1 | subid), d_reg3_adult)
anova(r4_adult, r5_adult, r6_adult) # r4
summary(r4_adult)
# summary(r5_adult)
# summary(r6_adult)

round(summary(r4_adult)$coefficients, 2) %>% data.frame() %>% View()

ggplot(d_reg3_adult %>%
         ungroup() %>%
         mutate(factor = factor(factor,
                                labels = c("Social-emotional",
                                           "Physiological",
                                           "Perceptual-cognitive"))),
       aes(x = age, y = score, color = character, fill = character, shape = character)) +
  facet_wrap("factor", ncol = 3) +
  theme_bw() +
  theme(text = element_text(size = 28),
        legend.position = "bottom") +
  geom_smooth(method = "loess", alpha = 0.4) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(19, 15)) +
  labs(title = "Factor scores by adults' age",
       # subtitle = "Adults (Study 1)\n",
       x = "Age (years)",
       y = "Factor score") # 1000 by 500

# make table ------------

# nfactors_choice <- 13
nfactors_choice <- 3

comb <- round(fa.sort(fa(d3, nfactors = nfactors_choice, 
                         rotate = rot_type, cor = cor_type)$loadings[]), 2) %>%
  data.frame() %>%
  select(1:3) %>%
  rownames_to_column(var = "capacity") %>%
  rownames_to_column(var = "order") %>%
  mutate(order = as.numeric(order)) %>%
  left_join(factors_all %>% select(capacity, capWording))
colnames(comb)[3:5] <- c("comb_F1", "comb_F2", "comb_F3")

temp_adult <- d3 %>%
  rownames_to_column(var = "subid") %>%
  left_join(d %>% select(subid, ageGroup) %>% distinct()) %>%
  filter(ageGroup == "adult") %>%
  select(-ageGroup) %>% 
  column_to_rownames(var = "subid")

temp_child <- d3 %>%
  rownames_to_column(var = "subid") %>%
  left_join(d %>% select(subid, ageGroup) %>% distinct()) %>%
  filter(ageGroup == "child") %>%
  select(-ageGroup) %>% 
  column_to_rownames(var = "subid")

fa(temp_adult, nfactors = 13, 
   rotate = rot_type, cor = cor_type)
ad <- round(fa.sort(fa(temp_adult, nfactors = nfactors_choice, 
                       rotate = rot_type, cor = cor_type)$loadings[]), 2) %>%
  data.frame() %>%
  select(1:3) %>%
  rownames_to_column(var = "capacity")
colnames(ad)[2:4] <- c("ad_F1", "ad_F2", "ad_F3")

ch <- round(fa.sort(fa(temp_child, nfactors = 4, # IMPORTANT 
                       rotate = rot_type, cor = cor_type)$loadings[]), 2) %>%
  data.frame() %>%
  select(1:3) %>%
  rownames_to_column(var = "capacity")
colnames(ch)[2:4] <- c("ch_F1", "ch_F2", "ch_F3")

loadings_table <- comb %>%
  full_join(ad) %>%
  full_join(ch) %>%
  select(order, capacity, capWording,
         # DOUBLE CHECK THESE!
         ad_F1, ch_F1, comb_F1,
         ad_F2, ch_F2, comb_F2,
         ad_F3, ch_F3, comb_F3) %>%
  arrange(order) %>%
  select(-order, -capacity)

# plot all items
# by condition
d1_bycond2 <- d0 %>%
  left_join(d_child01 %>% select(subid, age)) %>%
  select(character, capacity, capWording, responseNum, subid, ageGroup, age) %>%
  filter(character != "elephant") %>%
  filter(capacity != "na", is.na(responseNum) == F) %>%
  mutate(capWording = gsub(" --.*", "", capWording),
         ageGroup3 = ifelse(ageGroup == "adult", "adult",
                            ifelse(is.na(age), NA,
                                   ifelse(age < 8, "7y", 
                                          ifelse(age < 9, "8y",
                                                 ifelse(age < 10, "9y",
                                                        NA)))))) %>%
  distinct()

d1_bycond2 %>% select(ageGroup3, subid) %>% distinct() %>% count(ageGroup3)

# make df for plotting
d1_bycond2_mb <- multi_boot(d1_bycond2,
                           column = "responseNum",
                           summary_groups = c("ageGroup3", "character", "capacity", "capWording"),
                           statistics_functions = c("mean", "ci_lower", "ci_upper"))

d1_bycond2_mb_factorsAll <- d1_bycond2_mb %>% 
  full_join(factors_all) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order) %>%
  filter(!is.na(ageGroup3)) %>%
  mutate(factorName = factor(factorName,
                             levels = c("Factor 1", "Factor 2", "Factor 3"),
                             labels = c("Social-emotional", 
                                        "Physiological", 
                                        "Perceptual-cognitive")))

dodge_width <- 2

ggplot(d1_bycond2_mb_factorsAll, 
       aes(x = desc(order*2), y = mean,
           color = ageGroup3, shape = ageGroup3,
           label = capWording)) +
  facet_grid(factorName ~ character, scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = dodge_width), size = 8) +
  scale_shape_manual(values = c(rep(18, 3), 17)) +
  # scale_colour_brewer(type = "seq", palette = "PuRd") +
  scale_colour_hue(h = c(0, 180)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = dodge_width), width = 0) +
  geom_text(aes(y = -0.5, hjust = 0), 
            color = d1_bycond2_mb_factorsAll$textColor,
            size = 8) +
  labs(title = "Responses by mental capacity item",
       y = "Mean response (0 = NO, 0.5 = KINDA, 1 = YES)",
       x = "Capacity",
       color = "Age group: ", shape = "Age group: ") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 28),
        # axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") # 1700 by 2000
