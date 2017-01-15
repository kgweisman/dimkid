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

# d0 <- d0 %>%
#   filter(rt >= 350)

d1 <- d0

d2 <- d1 %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)

d3 <- data.frame(d2[,-1], row.names = d2[,1])

cor3 <- cor(d3, method = "spearman", use = "complete.obs")

# PLOT MEAN RESPONSES BY FACTOR -----------------------------------------------

# set correlation type: pearson or polychoric?
cor_type <- "cor"
# cor_type <- "poly"

factors_adult <- fa.sort(fa(d3 %>% 
                               rownames_to_column(var = "subid") %>%
                               filter(grepl("run", subid)) %>%
                               select(-subid),
                             nfactors = 3,
                             rotate = "varimax",
                             cor = cor_type)$loadings[]) %>%
  data.frame() %>%
  rownames_to_column(var = "capacity") %>%
  mutate(MR1_abs = abs(MR1),
         MR2_abs = abs(MR2),
         MR3_abs = abs(MR3),
         loading_abs = pmax(MR1_abs, MR2_abs, MR3_abs),
         loading = ifelse(loading_abs == abs(MR1), MR1,
                          ifelse(loading_abs == abs(MR2), MR2,
                                 ifelse(loading_abs == abs(MR3), MR3,
                                        NA))),
         factor = ifelse(loading == MR1, "MR1",
                         ifelse(loading == MR2, "MR2",
                                ifelse(loading == MR3, "MR3",
                                       NA))),
         factorName = ifelse(loading == MR1, "Factor 1",
                             ifelse(loading == MR2, "Factor 2",
                                    ifelse(loading == MR3, "Factor 3",
                                           NA)))) %>%
  arrange(factor, desc(loading_abs)) %>%
  select(capacity, factor, factorName, loading, loading_abs)

factors_adult <- 
  full_join(factors_adult %>%
              filter(factor == "MR1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors_adult %>%
              filter(factor == "MR2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors_adult %>%
              filter(factor == "MR3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         textColor = ifelse(loading < 0, "dodgerblue3", "black"))

factors_child <- fa.sort(fa(d3 %>% 
                              rownames_to_column(var = "subid") %>%
                              filter(!grepl("run", subid)) %>%
                              select(-subid),
                            nfactors = 3,
                            rotate = "varimax",
                            cor = cor_type)$loadings[]) %>%
  data.frame() %>%
  rownames_to_column(var = "capacity") %>%
  mutate(MR1_abs = abs(MR1),
         MR2_abs = abs(MR2),
         MR3_abs = abs(MR3),
         loading_abs = pmax(MR1_abs, MR2_abs, MR3_abs),
         loading = ifelse(loading_abs == abs(MR1), MR1,
                          ifelse(loading_abs == abs(MR2), MR2,
                                 ifelse(loading_abs == abs(MR3), MR3,
                                        NA))),
         factor = ifelse(loading == MR1, "MR1",
                         ifelse(loading == MR2, "MR2",
                                ifelse(loading == MR3, "MR3",
                                       NA))),
         factorName = ifelse(loading == MR1, "Factor 1",
                             ifelse(loading == MR2, "Factor 2",
                                    ifelse(loading == MR3, "Factor 3",
                                           NA)))) %>%
  arrange(factor, desc(loading_abs)) %>%
  select(capacity, factor, factorName, loading, loading_abs)

factors_child <- 
  full_join(factors_child %>%
              filter(factor == "MR1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors_child %>%
              filter(factor == "MR2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors_child %>%
              filter(factor == "MR3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         textColor = ifelse(loading < 0, "dodgerblue3", "black"))

# by condition
d1_bycond <- d0 %>%
  select(character, capacity, capWording, responseNum, subid, ageGroup) %>%
  filter(character != "elephant") %>%
  filter(capacity != "na", is.na(responseNum) == F) %>%
  mutate(capWordingShort = gsub(" --.*", "", capWording)) %>%
  select(-capWording)

# make df for plotting
d1_bycond_mb <- multi_boot(d1_bycond,
                           column = "responseNum",
                           summary_groups = c("ageGroup", "character", "capacity", "capWordingShort"),
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
           label = capWordingShort)) +
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
           label = capWordingShort)) +
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
fa.parallel(d3)
howmanyfac <- 3
# howmanyfac <- 4

factors_all <- fa.sort(fa(d3,
                          nfactors = howmanyfac,
                          rotate = "varimax",
                          cor = cor_type)$loadings[]) %>%
  data.frame() %>%
  rownames_to_column(var = "capacity") %>%
  mutate(MR1_abs = abs(MR1),
         MR2_abs = abs(MR2),
         MR3_abs = abs(MR3),
         # MR4_abs = abs(MR4),
         loading_abs = pmax(MR1_abs, MR2_abs, MR3_abs), #, MR4_abs),
         loading = ifelse(loading_abs == abs(MR1), MR1,
                          ifelse(loading_abs == abs(MR2), MR2,
                                 ifelse(loading_abs == abs(MR3), MR3,
                                        # ifelse(loading_abs == abs(MR4), MR4,
                                               NA))), #),
         factor = ifelse(loading == MR1, "MR1",
                         ifelse(loading == MR2, "MR2",
                                ifelse(loading == MR3, "MR3",
                                       # ifelse(loading == MR4, "MR4",
                                              NA))), #),
         factorName = ifelse(loading == MR1, "Factor 1",
                             ifelse(loading == MR2, "Factor 2",
                                    ifelse(loading == MR3, "Factor 3",
                                           # ifelse(loading == MR4, "Factor 4",
                                                  NA)))) %>% #) %>%
  arrange(factor, desc(loading_abs)) %>%
  select(capacity, factor, factorName, loading, loading_abs)

factors_all <- 
  full_join(factors_all %>%
              filter(factor == "MR1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors_all %>%
              filter(factor == "MR2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors_all %>%
              filter(factor == "MR3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  # full_join(factors_all %>%
  #             filter(factor == "MR4") %>%
  #             rownames_to_column(var = "order") %>%
  #             mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         textColor = ifelse(loading < 0, "dodgerblue3", "black"))

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
           label = capWordingShort)) +
  facet_grid(ageGroup ~ factorName) +
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

# set correlation type: pearson or polychoric?
cor_type <- "cor"
# cor_type <- "poly"

# set score type
score_type <- "regression"
# score_type <- "Thurstone"
# score_type <- "tenBerge"
# score_type <- "Anderson"
# score_type <- "Bartlett"

# scores_all <- fa(d3, nfactors = 3, rotate = "varimax",
#                  cor = cor_type, scores = score_type)$scores %>%
#   data.frame() %>%
#   rownames_to_column(var = "subid") %>%
#   mutate(ageGroup = factor(ifelse(grepl("run", subid), "adult", "child"))) %>%
#   rename(score_MR1 = MR1, score_MR2 = MR2, score_MR3 = MR3)

scores_all <- fa(d3, nfactors = 4, rotate = "varimax",
                 cor = cor_type, scores = score_type)$scores %>%
  data.frame() %>%
  rownames_to_column(var = "subid") %>%
  mutate(ageGroup = factor(ifelse(grepl("run", subid), "adult", "child"))) %>%
  rename(score_MR1 = MR1, score_MR2 = MR2, score_MR3 = MR3, score_MR4 = MR4)

# d_reg2 <- d0 %>%
#   select(subid, ageGroup, character) %>%
#   filter(character != "elephant") %>%
#   distinct() %>%
#   full_join(scores_all) %>%
#   mutate(character = factor(character)) %>%
#   filter(!is.na(score_MR1), !is.na(score_MR2), !is.na(score_MR3), !is.na(ageGroup)) %>%
#   gather(factor, score, starts_with("score_")) %>%
#   mutate(factor = factor(factor)) %>%
#   multi_boot(column = "score",
#              summary_groups = c("ageGroup", "character", "factor"),
#              statistics_functions = c("mean", "ci_lower", "ci_upper"))

d_reg2 <- d0 %>%
  select(subid, ageGroup, character) %>%
  filter(character != "elephant") %>%
  distinct() %>%
  full_join(scores_all) %>%
  mutate(character = factor(character)) %>%
  filter(!is.na(score_MR1), !is.na(score_MR2), !is.na(score_MR3), !is.na(score_MR4), 
         !is.na(ageGroup)) %>%
  gather(factor, score, starts_with("score_")) %>%
  mutate(factor = factor(factor)) %>%
  multi_boot(column = "score",
             summary_groups = c("ageGroup", "character", "factor"),
             statistics_functions = c("mean", "ci_lower", "ci_upper"))

# plot
ggplot(d_reg2,
       aes(x = ageGroup, y = mean, color = character, fill = character)) +
  facet_wrap("factor", ncol = 4) +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = "top") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2)

# analyze?
# d_reg3 <- d0 %>%
#   select(subid, ageGroup, character) %>%
#   filter(character != "elephant") %>%
#   distinct() %>%
#   full_join(scores_all) %>%
#   mutate(character = factor(character)) %>%
#   filter(!is.na(score_MR1) & !is.na(score_MR2) & !is.na(score_MR3), !is.na(ageGroup)) %>%
#   gather(factor, score, starts_with("score_")) %>%
#   mutate(factor = factor(factor))

d_reg3 <- d0 %>%
  select(subid, ageGroup, character) %>%
  filter(character != "elephant") %>%
  distinct() %>%
  full_join(scores_all) %>%
  mutate(character = factor(character)) %>%
  filter(!is.na(score_MR1), !is.na(score_MR2), !is.na(score_MR3), !is.na(score_MR4),
         !is.na(ageGroup)) %>%
  gather(factor, score, starts_with("score_")) %>%
  mutate(factor = factor(factor))

# set contrasts
# contrasts(d_reg3$factor) <- cbind(MR1 = c(1, 0, 0), # MAKE SURE TO DOUBLE-CHECK!!
#                                   MR3 = c(0, 0, 1))
contrasts(d_reg3$factor) <- cbind(MR1 = c(1, 0, 0, 0), # MAKE SURE TO DOUBLE-CHECK!!
                                  MR3 = c(0, 0, 1, 0),
                                  MR4 = c(0, 0, 0, 1))
contrasts(d_reg3$character) <- cbind(robot = c(-1, 1))
contrasts(d_reg3$ageGroup) <- cbind(child = c(-1, 1))

r1 <- lmer(score ~ character * factor + (1 | subid), d_reg3)
r2 <- lmer(score ~ character * factor + ageGroup + (1 | subid), d_reg3)
r3 <- lmer(score ~ character * factor * ageGroup + (1 | subid), d_reg3)
anova(r1, r2, r3)
# summary(r1)
# summary(r2)
summary(r3)

# robot only
robot_r1 <- lmer(score ~ factor + (1 | subid), d_reg3 %>% filter(character == "robot"))
robot_r2 <- lmer(score ~ factor + ageGroup + (1 | subid), d_reg3 %>% filter(character == "robot"))
robot_r3 <- lmer(score ~ factor * ageGroup + (1 | subid), d_reg3 %>% filter(character == "robot"))
anova(robot_r1, robot_r2, robot_r3)
# summary(robot_r1)
# summary(robot_r2)
summary(robot_r3)

# make table ------------

comb <- round(fa.sort(fa(d3, nfactors = 3, rotate = "varimax", cor = cor_type)$loadings[]), 2) %>%
  data.frame() %>%
  rename(comb_MR1 = MR1, comb_MR2 = MR2, comb_MR3 = MR3) %>%
  rownames_to_column(var = "item") %>%
  rownames_to_column(var = "order") %>%
  mutate(order = as.numeric(order))

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

ad <- round(fa.sort(fa(temp_adult, nfactors = 3, rotate = "varimax", cor = cor_type)$loadings[]), 2) %>%
  data.frame() %>%
  rename(ad_MR1 = MR1, ad_MR2 = MR2, ad_MR3 = MR3) %>%
  rownames_to_column(var = "item")

ch <- round(fa.sort(fa(temp_child, nfactors = 3, rotate = "varimax", cor = cor_type)$loadings[]), 2) %>%
  data.frame() %>%
  rename(ch_MR1 = MR1, ch_MR2 = MR2, ch_MR3 = MR3) %>%
  rownames_to_column(var = "item")

temp_table <- comb %>%
  full_join(ad) %>%
  full_join(ch) %>%
  select(order, item,
         # DOUBLE CHECK THESE!
         ad_MR2, ch_MR1, comb_MR1,
         ad_MR1, ch_MR2, comb_MR2,
         ad_MR3, ch_MR3, comb_MR3) %>%
  arrange(order) %>%
  select(-order)
