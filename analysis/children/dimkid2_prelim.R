# PRELIMINARIES ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stats)
library(psych)
library(ggplot2)
library(tibble)
library(GPArotation)
library(lme4)
library(langcog)

# clear environment
rm(list=ls())
graphics.off()

# READ IN DATA ----------------------------------------------------------------

# run02: campbell, nicky, dru (winter 2017)
d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-02_2017-03-20_anonymized.csv")

# # read in ages
# ages <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/dimkid_participant_ages_2017-01-19.csv") %>%
#   select(-age_formula, -comments) %>%
#   mutate(ethnicityCat1 = 
#            factor(ifelse(is.na(ethnicity), NA,
#                          ifelse(ethnicity == "white, caucasian, or european american",
#                                 "euro-american", "multicultural"))),
#          ethnicityCat2 = 
#            factor(ifelse(is.na(ethnicity), NA,
#                          ifelse(ethnicity == "white, caucasian, or european american",
#                                 "euro-american",
#                                 ifelse(grepl("east asian", ethnicity) |
#                                          grepl("south", ethnicity) |
#                                          grepl("chinese", ethnicity) |
#                                          grepl("filipino", ethnicity),
#                                        "asian", NA)))),
#          age = as.numeric(as.character(age)))

# TIDY DATA -------------------------------------------------------------------

# filter by condition
d0 <- d %>%
  filter(character %in% c("computer", "robot", "beetle", "bird", "goat", "mouse", "elephant"))

# # examine and filter by ages
# qplot(age, data = d0 %>% select(subid, age) %>% distinct()) +
#   geom_vline(xintercept = 7, color = "red") +
#   geom_vline(xintercept = 10, color = "red")
# 
# d1 <- d0 %>%
#   filter(is.na(age) | (age >= 7 & age <10)) %>%
#   select(-X, -X.1)

d1 <- d0

# examine and filter by RTs
qplot(d1$rt, bins = 100) +
  scale_x_log10(breaks = seq(0, 1000, 100)) +
  geom_vline(xintercept = 250, color = "red")

d1 %>% 
  mutate(short_rt = ifelse(rt < 250, T, F)) %>% 
  count(short_rt) %>%
  mutate(percent = n/sum(n))

d2 <- d1 %>%
  filter(rt >= 250)

# finish tidying
d3 <- d2 %>%
  select(capacity, responseNum, subid) %>%
  filter(!is.na(capacity), capacity != "na") %>%
  spread(capacity, responseNum)

d4 <- data.frame(d3[,-1], row.names = d3[,1])

cor3 <- cor(d4, method = "spearman", use = "complete.obs")

# DEMOGRAPHICS ----------------------------------------------------------------

# total n
d2 %>% 
  select(subid) %>%
  distinct(.keep_all = T) %>%
  count()

# # age 
# d2 %>% 
#   select(subid, age) %>%
#   distinct(.keep_all = T) %>%
#   summarise(mean_age = mean(age, na.rm = T),
#             sd_age = sd(age, na.rm = T),
#             median_age = median(age, na.rm = T),
#             min_age = min(age, na.rm = T),
#             max_age = max(age, na.rm = T))
# 
# qplot(d2 %>% distinct(subid, .keep_all = T) %>% select(age), bins = 18) +
#   geom_vline(xintercept = median(d2$age, na.rm = T), color = "red")
# 
# d2 %>% 
#   distinct(subid, .keep_all = T) %>% 
#   select(age, character) %>%
#   group_by(character) %>% 
#   summarise(median = median(age, na.rm = T))
# 
# t.test(age ~ character, d2 %>% select(subid, age, character) %>% distinct)
# 
# ggplot(d2 %>% 
#          distinct(subid, .keep_all = T) %>% 
#          select(age, character) %>%
#          group_by(character) %>%
#          mutate(median_age = median(age, na.rm = T)),
#        aes(x = age)) +
#   geom_histogram(bins = 9) +
#   facet_wrap(~ character) +
#   geom_vline(xintercept = median(d2$age, na.rm = T), color = "black") +
#   geom_vline(aes(xintercept = median_age, color = character), lty = 2)

# # gender
# d2 %>%
#   select(subid, gender) %>%
#   distinct(.keep_all = T) %>%
#   count(gender)

# # ethnicity
# d2 %>% 
#   select(subid, ethnicity) %>%
#   # MIGHT BE SOME PROBLEMS HERE
#   mutate(black = grepl("black", ethnicity),
#          east_asian = grepl("east asian", ethnicity) |
#            grepl("chinese", ethnicity) |
#            grepl("korea", ethnicity) |
#            grepl("japan", ethnicity) |
#            grepl("taiwan", ethnicity),
#          south_asian = grepl("south", ethnicity) |
#            grepl("india", ethnicity) |
#            grepl("pakistan", ethnicity) |
#            grepl("bangla", ethnicity) |
#            grepl("sri lanka", ethnicity),
#          latino = grepl("latin", ethnicity) |
#            grepl("hispanic", ethnicity) |
#            grepl("mexic", ethnicity),
#          middle_eastern = grepl("middle", ethnicity),
#          pac_island = grepl("pacific", ethnicity),
#          native_am = grepl("native", ethnicity) & !grepl("hawaiian", ethnicity),
#          white = grepl("white", ethnicity),
#          other = grepl("other", ethnicity)) %>%
#   distinct(.keep_all = T) %>%
#   gather(ethnicityTF, TF, -subid, -ethnicity) %>%
#   filter(TF) %>%
#   count(ethnicityTF)

# condition
d2 %>% 
  select(subid, character) %>%
  distinct(.keep_all = T) %>%
  count(character)

# duration
d2 %>%
  left_join(d %>% select(subid, sessionDuration)) %>%
  summarise(mean = mean(sessionDuration, na.rm = T),
            median = median(sessionDuration, na.rm = T),
            min = min(sessionDuration, na.rm = T),
            max = max(sessionDuration, na.rm = T))

qplot(d2 %>% 
        left_join(d %>% select(subid, sessionDuration)) %>% 
        mutate(sessionDuration = round(as.numeric(as.character(sessionDuration)), 3)) %>%
        select(sessionDuration)) +
  scale_x_continuous(breaks = 0:15)

summary(lm(sessionDuration ~ character,
       d2 %>%
         select(subid, character, sessionDuration) %>%
         left_join(d %>% select(subid, sessionDuration)) %>%
         mutate(subid = as.character(subid),
                character = factor(character),
                sessionDuration = round(as.numeric(as.character(sessionDuration)), 3)) %>%
         distinct()))

# incomplete
d1 %>% # use d1 (before dropping trials) 
  count(subid) %>%
  filter(n != 20)

# HEATMAP, CLUSTERING ---------------------------------------------------------

m1 <- as.matrix(d4[complete.cases(d4),]) # remove rows with NAs
heatmap(m1)

m1 <- as.matrix(d4) # keep NAs

m2 <- d3[complete.cases(d3),] %>%
  left_join(d %>% select(subid, character)) %>%
  distinct() %>% 
  mutate(subid = paste(character, subid, sep = "_")) %>%
  select(-character) %>%
  column_to_rownames(var = "subid") %>%
  as.matrix()
heatmap(m2)


cluster <- hclust(dist(t(m1)))
plot(cluster)

# d2_young <- d2 %>%
#   filter(age < median(d2$age, na.rm = T)) %>%
#   select(capacity, responseNum, subid) %>%
#   filter(capacity != "na") %>%
#   spread(capacity, responseNum)
# d4_young <- data.frame(d2_young[,-1], row.names = d2_young[,1])
# m_young <- as.matrix(d4_young)
# cluster_young <- hclust(dist(t(m_young)))
# plot(cluster_young)
# d2_young %>% count()
# 
# d2_old <- d2 %>%
#   filter(age >= median(d2$age, na.rm = T)) %>%
#   select(capacity, responseNum, subid) %>%
#   filter(capacity != "na") %>%
#   spread(capacity, responseNum)
# d4_old <- data.frame(d2_old[,-1], row.names = d2_old[,1])
# m_old <- as.matrix(d4_old)
# cluster_old <- hclust(dist(t(m_old)))
# plot(cluster_old)
# d2_old %>% count()

# FACTOR ANALYSIS -------------------------------------------------------------

# pearson correlations
VSS.scree(d4)
fa.parallel(d4)
fa(r = d4, nfactors = 13, rotate = "none", fm = "minres", cor = "cor")
fa(r = d4, nfactors = 13, rotate = "varimax", fm = "minres", cor = "cor")
fa.sort(fa(d4, nfactors = 7, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d4, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d4, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()

fa.sort(fa(d4, nfactors = 4, rotate = "oblimin")$loadings[]) %>% View() # oblimin rotation
fa.sort(fa(d4, nfactors = 3, rotate = "oblimin")$loadings[]) %>% View() # oblimin rotation

# # polychoric correlations
# fa.parallel(d4, cor = "poly")
# fa(d4, nfactors = 13, rotate = "none", cor = "poly")
# fa(d4, nfactors = 13, rotate = "varimax", cor = "poly")
# fa.sort(fa(d4, nfactors = 7, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
# fa.sort(fa(d4, nfactors = 6, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
# fa.sort(fa(d4, nfactors = 5, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
# fa.sort(fa(d4, nfactors = 4, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
# fa.sort(fa(d4, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) %>% View()

# PLOTTING, USING CHILD FACTOR LOADINGS ---------------------------------------

# set correlation type: pearson or polychoric?
cor_type <- "cor"
# cor_type <- "poly"

factors <- fa.sort(fa(d4, nfactors = 3, rotate = "varimax", cor = cor_type)$loadings[]) 

factors2 <- factors %>%
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

factors3 <- 
  full_join(factors2 %>%
              filter(factor == "MR1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors2 %>%
              filter(factor == "MR2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors2 %>%
              filter(factor == "MR3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         textColor = ifelse(loading < 0, "dodgerblue3", "black"))

# by condition
d2_bycond <- d0 %>%
  select(character, capacity, capWording, responseNum, subid) %>%
  filter(capacity != "na", is.na(responseNum) == F) %>%
  mutate(capWordingShort = gsub(" --.*", "", capWording)) %>%
  select(-capWording)

library(langcog)
d2_bycond_mb <- multi_boot(d2_bycond,
                           column = "responseNum",
                           summary_groups = c("character", "capacity", "capWordingShort"),
                           statistics_functions = c("mean", "ci_lower", "ci_upper")) %>% 
  full_join(factors3) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order)

ggplot(d2_bycond_mb, 
       aes(x = desc(order*2), y = mean,
           group = character, color = character, shape = character,
           label = capWordingShort)) +
  facet_grid(. ~ factorName) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
  scale_shape_manual(values = c(19, # beetle
                                13, # bird
                                7, # computer
                                8, # elephant
                                17, # goat
                                18, # mouse
                                15)) + # robot
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = .75), width = 0) +
  geom_text(aes(y = -0.18, hjust = 0), color = d2_bycond_mb$textColor, size = 6) +
  labs(title = "Children's responses, by child-derived factors",
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

# ...USING ADULT FACTOR LOADINGS ----------------------------------------------

# read in adult loadings
factors_poly_varimax_3f <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/plots and tables/factors_poly_varimax_3f (adults)") %>%
  select(capacity, factor, factorName, loading, loading_abs) 

# make factor assignments
factors3_ADULT <- 
  full_join(factors_poly_varimax_3f %>%
              filter(factor == "MR1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors_poly_varimax_3f %>%
              filter(factor == "MR2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors_poly_varimax_3f %>%
              filter(factor == "MR3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         textColor = ifelse(loading < 0, "dodgerblue3", "black"))

# by condition
d2_bycond_ADULT_mb <- multi_boot(d2_bycond, # get from above
                                 column = "responseNum",
                                 summary_groups = c("character", "capacity", "capWordingShort"),
                                 statistics_functions = c("mean", "ci_lower", "ci_upper")) %>% 
  full_join(factors3_ADULT) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order)

ggplot(d2_bycond_ADULT_mb, 
       aes(x = desc(order*2), y = mean,
           group = character, color = character, shape = character,
           label = capWordingShort)) +
  facet_grid(. ~ factorName) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
  scale_shape_manual(values = c(19, # beetle
                                13, # bird
                                7, # computer
                                8, # elephant
                                17, # goat
                                18, # mouse
                                15)) + # robot
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = .75), width = 0) +
  geom_text(aes(y = -0.18, hjust = 0), color = d2_bycond_ADULT_mb$textColor, size = 6) +
  labs(title = "Children's responses, by adult-derived factors",
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

# ggplot(d2_bycond %>% full_join(factors3_ADULT), 
#        aes(x = desc(order*2),
#            fill = factor(responseNum), 
#            label = capWordingShort)) +
#   facet_grid(character ~ factorName) +
#   geom_bar(position = "fill") +
#   geom_text(aes(y = 0, hjust = 0), size = 7) +
#   labs(title = "Children's responses, by adult-derived factors",
#        y = "\nProportion of responses",
#        x = "Capacity\n",
#        color = "Character: ", shape = "Character: ") +
#   coord_flip() +
#   theme_bw() +
#   theme(text = element_text(size = 24),
#         # axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         legend.position = "top")

# ...USING ADULT FACTOR LOADINGS, by age --------------------------------------

# # make age thing
# d2_bycond_age <- d2_bycond %>%
#   left_join(ages %>% select(subid, age)) %>%
#   filter(!is.na(age)) %>%
#   mutate(ageCat3 = ifelse(age < 8, "7", ifelse(age < 9, "8", ifelse(age < 10, "9", NA)))) %>%
#   mutate(ageCat2 = ifelse(age < median(age, na.rm = T), "young", ifelse(age < 10, "old", NA)))
# 
# # by condition
# d2_bycond_ADULT_AGE_mb <- multi_boot(d2_bycond_age, # get from above
#                                      column = "responseNum",
#                                      summary_groups = c("ageCat2",
#                                                         "character", "capacity", "capWordingShort"),
#                                      statistics_functions = c("mean", "ci_lower", "ci_upper")) %>%
#   full_join(factors3_ADULT) %>%
#   arrange(character, factor, desc(loading_abs)) %>%
#   rownames_to_column(var = "full_order") %>%
#   mutate(full_order = as.numeric(full_order)) %>%
#   arrange(factorName, full_order) %>%
#   filter(!is.na(ageCat2))
# 
# ggplot(d2_bycond_ADULT_AGE_mb,
#        aes(x = desc(order*2), y = mean,
#            group = character, color = character, shape = character,
#            label = capWordingShort)) +
#   facet_grid(ageCat2 ~ factorName) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_hline(yintercept = 0.5, lty = 3) +
#   geom_hline(yintercept = 1, lty = 3) +
#   geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
#   scale_shape_manual(values = c(19, 15)) +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
#                 position = position_dodge(width = .75), width = 0) +
#   geom_text(aes(y = -0.18, hjust = 0), color = d2_bycond_ADULT_AGE_mb$textColor, size = 6) +
#   labs(title = "Children's responses, by adult-derived factors",
#        y = "\nMean response (0 = NO, 0.5 = KINDA, 1 = YES)",
#        x = "Capacity\n",
#        color = "Character: ", shape = "Character: ") +
#   coord_flip() +
#   theme_bw() +
#   theme(text = element_text(size = 24),
#         # axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         legend.position = "top")

# ...BY ITEM, age -------------------------------------------------------------

# # alphabetical order
# ggplot(d2, aes(x = age, y = responseNum, color = character, fill = character)) +
#   facet_wrap(~ capacity, ncol = 8) +
#   geom_jitter(height = 0.1, width = 0, size = 0.25) +
#   # geom_point(position = "jitter", size = .25) + 
#   geom_smooth(alpha = 0.2) + # is this legit for 3-point scale?
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# # # ranks (like spearman correlations)
# # ggplot(d2, aes(x = rank(age), y = responseNum, color = character, fill = character)) +
# #   facet_wrap(~ capacity, ncol = 8) +
# #   geom_jitter(height = 0.1, width = 0, size = 0.25) +
# #   # geom_point(position = "jitter", size = .25) +
# #   geom_smooth(alpha = 0.2) +
# #   theme_bw() +
# #   theme(text = element_text(size = 18),
# #       legend.position = "top")
# 
# # ordered by factor
# d2_byfactor <- d2 %>% 
#   left_join(factors2 %>%
#               mutate(factor = factor(factor)) %>%
#               arrange(factor, desc(loading_abs)) %>% 
#               rownames_to_column(var = "order") %>%
#               mutate(order = as.numeric(order)))
# 
# # ggplot(d2_byfactor, 
# #        aes(x = age, y = responseNum, color = character, fill = character)) +
# #   facet_wrap(factor ~ reorder(capacity, order), ncol = 8) +
# #   geom_jitter(height = 0.1, width = 0, size = 0.25) +
# #   # geom_point(position = "jitter", size = .25) + 
# #   geom_smooth(alpha = 0.2) + # is this legit for 3-point scale?
# #   theme_bw() +
# #   theme(text = element_text(size = 18),
# #         legend.position = "top")
# 
# # factor 1 only
# ggplot(d2_byfactor %>%
#          filter(factor == "MR1"), 
#        aes(x = age, y = responseNum, color = character, fill = character)) +
#   facet_wrap(~ reorder(capacity, order), ncol = 5) +
#   geom_jitter(height = 0.1, width = 0, size = 0.25) +
#   # geom_point(position = "jitter", size = .25) + 
#   geom_smooth(alpha = 0.2) + # is this legit for 3-point scale?
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# # factor 2 only
# ggplot(d2_byfactor %>%
#          filter(factor == "MR2"), 
#        aes(x = age, y = responseNum, color = character, fill = character)) +
#   facet_wrap(~ reorder(capacity, order), ncol = 5) +
#   geom_jitter(height = 0.1, width = 0, size = 0.25) +
#   # geom_point(position = "jitter", size = .25) + 
#   geom_smooth(alpha = 0.2) + # is this legit for 3-point scale?
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# # factor 3 only
# ggplot(d2_byfactor %>%
#          filter(factor == "MR3"), 
#        aes(x = age, y = responseNum, color = character, fill = character)) +
#   facet_wrap(~ reorder(capacity, order), ncol = 5) +
#   geom_jitter(height = 0.1, width = 0, size = 0.25) +
#   # geom_point(position = "jitter", size = .25) + 
#   geom_smooth(alpha = 0.2) + # is this legit for 3-point scale?
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# # raw counts
# 
# # factor 1 only
# ggplot(d2_byfactor %>%
#          filter(factor == "MR1", !is.na(age)), 
#        aes(x = cut_number(age, 4), fill = factor(responseNum))) +
#   facet_grid(character ~ reorder(capacity, order)) +
#   geom_bar(position = "fill") +
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top",
#         axis.text.x = element_blank(),
#         axis.title.y = element_blank())
# 
# # factor 2 only
# ggplot(d2_byfactor %>%
#          filter(factor == "MR2", !is.na(age)), 
#        aes(x = cut_number(age, 4), fill = factor(responseNum))) +
#   facet_grid(character ~ reorder(capacity, order)) +
#   geom_bar(position = "fill") +
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top",
#         axis.text.x = element_blank(),
#         axis.title.y = element_blank())
# 
# # factor 3 only
# ggplot(d2_byfactor %>%
#          filter(factor == "MR3", !is.na(age)), 
#        aes(x = cut_number(age, 4), fill = factor(responseNum))) +
#   facet_grid(character ~ reorder(capacity, order)) +
#   geom_bar(position = "fill") +
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top",
#         axis.text.x = element_blank(),
#         axis.title.y = element_blank())

# ...FACTOR SCORES ------------------------------------------------------------

# set score_type
score_type <- "tenBerge"

scores_children <- fa(d4, nfactors = 3, rotate = "varimax", 
                      cor = cor_type, scores = score_type)$scores %>%
  data.frame() %>%
  rownames_to_column(var = "subid") %>%
  rename(score_MR1 = MR1, score_MR2 = MR2, score_MR3 = MR3)

d_reg2 <- d2 %>%
  select(subid, 
         #age, 
         character) %>%
  distinct() %>%
  full_join(scores_children) %>%
  # left_join(ages %>% select(subid, age)) %>%
  # mutate(age = as.numeric(as.character(age))) %>%
  # mutate(ageCat3 = ifelse(age < 8, "7", ifelse(age < 9, "8", ifelse(age < 10, "9", NA))),
         # ageCat2 = ifelse(age < median(age, na.rm = T), "young", ifelse(age < 10, "old", NA))) %>%
  mutate(character = factor(character,
                            levels = c("computer", "robot", 
                                       "beetle", "bird", 
                                       "mouse", "goat", "elephant"))) %>%
  filter(!is.na(score_MR1) & !is.na(score_MR2) & !is.na(score_MR3)) %>% #, !is.na(age)) %>%
  gather(factor, score, starts_with("score_")) %>%
  mutate(factor = factor(factor))

# plot
ggplot(d_reg2, aes(x = character, y = score)) +
  facet_wrap("factor") +
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "top") +
  geom_boxplot() +
  geom_point(position = position_jitter(height = 0, width = 0.3), color = "blue") 

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

scores_children <- fa(d4, nfactors = 3, rotate = "varimax", 
                      cor = cor_type, scores = score_type)$scores %>%
  data.frame() %>%
  rownames_to_column(var = "subid") %>%
  rename(score_MR1 = MR1, score_MR2 = MR2, score_MR3 = MR3)

d_reg2 <- d0 %>%
  # filter(character == "robot") %>%
  select(subid, 
         # age, 
         character) %>%
  distinct() %>%
  full_join(scores_children) %>%
  # left_join(ages %>% select(subid, age)) %>%
  # mutate(age = as.numeric(as.character(age))) %>%
  # mutate(ageCat3 = ifelse(age < 8, "7", ifelse(age < 9, "8", ifelse(age < 10, "9", NA))),
         # ageCat2 = ifelse(age < median(age, na.rm = T), "young", ifelse(age < 10, "old", NA))) %>%
  mutate(character = factor(character)) %>%
  filter(!is.na(score_MR1) & !is.na(score_MR2) & !is.na(score_MR3)
         # , !is.na(age)
         ) %>%
  gather(factor, score, starts_with("score_")) %>%
  mutate(factor = factor(factor))

# # plot
# ggplot(d_reg2, aes(x = age, y = score, color = character, fill = character)) +
#   facet_wrap("factor") +
#   theme_bw() +
#   theme(text = element_text(size = 24),
#         legend.position = "top") +
#   geom_point() + 
#   geom_smooth(alpha = 0.2, method = "loess")
# # geom_smooth(alpha = 0.2, method = "lm") 

# set contrasts
contrasts(d_reg2$factor) <- cbind(MR1 = c(1, 0, 0), # MAKE SURE TO DOUBLE-CHECK!!
                                  MR3 = c(0, 0, 1))
contrasts(d_reg2$character) <- cbind(robot = c(0, 0, -1, 0, 0, 0, 1),
                                     beetle = c(1, 0, -1, 0, 0, 0, 0),
                                     bird = c(0, 1, -1, 0, 0, 0, 0),
                                     mouse = c(0, 0, -1, 0, 0, 1, 0),
                                     goat = c(0, 0, -1, 0, 1, 0, 0),
                                     elephant = c(0, 0, -1, 1, 0, 0, 0))

r1 <- lmer(score ~ character * factor + (1 | subid), d_reg2)
# r2 <- lmer(score ~ character * factor + scale(age) + (1 | subid), d_reg2)
# r3 <- lmer(score ~ character * factor * scale(age) + (1 | subid), d_reg2)
# anova(r1, r2, r3)
summary(r1)
# summary(r2)
# summary(r3)

# r3b <- lmer(score ~ character * factor * poly(age, 1) + (1 | subid), d_reg2)
# r4 <- lmer(score ~ character * factor * poly(age, 2) + (1 | subid), d_reg2)
# r5 <- lmer(score ~ character * factor * poly(age, 3) + (1 | subid), d_reg2)
# anova(r3b, r4, r5)
# summary(r3b)
# summary(r4)
# summary(r5)

# robot only
robot_r1 <- lmer(score ~ factor + (1 | subid), d_reg2 %>% filter(character == "robot"))
# robot_r2 <- lmer(score ~ factor + scale(age) + (1 | subid), d_reg2 %>% filter(character == "robot"))
# robot_r3 <- lmer(score ~ factor * scale(age) + (1 | subid), d_reg2 %>% filter(character == "robot"))
# anova(robot_r1, robot_r2, robot_r3)
summary(robot_r1)
# summary(robot_r2)
# summary(robot_r3)

# robot_r3b <- lmer(score ~ factor * poly(age, 1) + (1 | subid), d_reg2 %>% filter(character == "robot"))
# robot_r4 <- lmer(score ~ factor * poly(age, 2) + (1 | subid), d_reg2 %>% filter(character == "robot"))
# robot_r5 <- lmer(score ~ factor * poly(age, 3) + (1 | subid), d_reg2 %>% filter(character == "robot"))
# anova(robot_r3b, robot_r4, robot_r5)
# summary(robot_r3b)
# summary(robot_r4)
# summary(robot_r5)

# EXPLORATORY.... -------------------------------------------------------------

# stepwise regression ---------------------------------------------------------

d_step <- d4[complete.cases(d4),] %>%
  rownames_to_column(var = "subid") %>%
  # left_join(ages) %>%
  # filter(!is.na(age), !is.na(happy)) %>%
  filter(!is.na(happy)) %>%
  left_join(d2_bycond %>% select(subid, character) %>% distinct()) %>%
  mutate(character = factor(character))

# # age (all characters)
# step_r1 <- lm(age ~ character * (happy + depressed + fear + angry + calm + sounds + seeing + temperature + odors + depth + computations + thoughts + reasoning + remembering + beliefs + hungry + tired + pain + nauseated + safe + love + recognizing + communicating + guilt + disrespected + free_will + choices + self_restraint + intentions + goal + conscious + self_aware + desires + embarrassed + emo_recog + joy + morality + personality + pleasure + pride), data = d_step)
# # summary(step_r1)
# 
# step_r2 <- step(step_r1)
# # step_r2
# summary(step_r2)
# 
# # step_r3 <- step(step_r1, direction = "backward")
# # step_r3
# # summary(step_r3)
# # 
# # step_r4 <- step(step_r1, direction = "forward")
# # step_r4
# # summary(step_r4)
# 
# # age (robot only)
# 
# step_r5 <- lm(age ~ happy + depressed + fear + angry + calm + sounds + seeing + temperature + odors + depth + computations + thoughts + reasoning + remembering + beliefs + hungry + tired + pain + nauseated + safe + love + recognizing + communicating + guilt + disrespected + free_will + choices + self_restraint + intentions + goal + conscious + self_aware + desires + embarrassed + emo_recog + joy + morality + personality + pleasure + pride, data = d_step %>% filter(character == "robot"))
# # summary(step_r5)
# 
# step_r6 <- step(step_r5)
# # step_r6
# summary(step_r6)

# glmpath ---------------------------------------------------------------------

# # data formatting
# d_glmpath <- d_step %>%
#   select(angry:tired, character, age, ethnicityCat2)
# mental <- as.matrix(d_glmpath[,1:40])
# char <- as.matrix(as.numeric(d_glmpath[,41])-1)
# age <- as.matrix(log(d_glmpath[,42])) # log transform!
# culture <- as.matrix(as.numeric(d_glmpath[,43])-1)
# 
# d_glmpath_robot <- d_glmpath %>%
#   filter(character == "robot") %>%
#   select(-character)
# mental_robot <- as.matrix(d_glmpath_robot[,1:40])
# age_robot <- as.matrix(log(d_glmpath_robot[,41])) # log transform!
# culture_robot <- as.matrix(as.numeric(d_glmpath_robot[,42])-1)
# 
# d_glmpath_beetle <- d_glmpath %>%
#   filter(character == "beetle") %>%
#   select(-character)
# mental_beetle <- as.matrix(d_glmpath_beetle[,1:40])
# age_beetle <- as.matrix(log(d_glmpath_beetle[,41])) # log transform!
# culture_beetle <- as.matrix(as.numeric(d_glmpath_beetle[,42])-1)
# 
# # without cross-validation
# 
# library(glmpath)
# # rs1 <- glmpath(mental, char, family = "binomial"); rs1
# # rs1 <- glmpath(mental, age, family = "gaussian"); rs1
# # rs1 <- glmpath(mental, culture, family = "gaussian"); rs1
# rs1 <- glmpath(mental_robot, age_robot, family = "gaussian"); rs1
# # rs1 <- glmpath(mental_beetle, age_beetle, family = "gaussian"); rs1
# # rs1 <- glmpath(mental_robot, culture_robot, family = "gaussian"); rs1
# # rs1 <- glmpath(mental_beetle, culture_beetle, family = "gaussian"); rs1
# # head(summary(rs1))
# # head(rs1$b.predictor)
# 
# # get max lambda (for use in CV below)
# lambda_max <- max(rs1$lambda)
# 
# # make df for predictors
# d_predictor <- rs1$b.predictor %>% # extract the coefficient estimates
#   data.frame() %>% # turn them into a dataframe
#   rownames_to_column(var = "step") %>% # add the rownames as a "step" variable
#   mutate(lambda = rs1$lambda, # add the lambda values for each step
#          aic = rs1$aic, # add AIC for each step
#          bic = rs1$bic) %>% # add BIC for each step
#   gather(variable, coeff, happy:pride) %>% # turn into longform
#   arrange(step, variable) # arrange by step number and selfrating
# 
# # check out fit
# d_predictor %>% 
#   spread(variable, coeff) %>% # make it wideform again
#   select(step:bic) # don't include the coefficients for all the predictors
# 
# # find lambda that minimizes AIC
# lambda_minAIC <- d_predictor %>%
#   filter(aic == min(aic)) %>%
#   select(lambda, aic) %>%
#   unique()
# lambda_minAIC
# 
# # look at the coefficients when lambda = best lambda (for AIC)
# d_predictor %>%
#   filter(lambda == lambda_minAIC$lambda) %>%
#   filter(coeff != 0)
# 
# # find lambda that minimizes BIC
# lambda_minBIC <- d_predictor %>%
#   filter(bic == min(bic)) %>%
#   select(lambda, bic) %>%
#   unique()
# lambda_minBIC
# 
# # look at the coefficients when lambda = best lambda (for BIC)
# d_predictor %>%
#   filter(lambda == lambda_minBIC$lambda) %>%
#   filter(coeff != 0)

# ...BY ITEM, culture ---------------------------------------------------------

# # look at demographics again
# 
# # age 
# d2 %>% 
#   select(subid, ethnicityCat2, age) %>%
#   distinct(.keep_all = T) %>%
#   group_by(ethnicityCat2) %>%
#   summarise(mean_age = mean(age, na.rm = T),
#             sd_age = sd(age, na.rm = T),
#             median_age = median(age, na.rm = T),
#             min_age = min(age, na.rm = T),
#             max_age = max(age, na.rm = T))
# 
# t.test(age ~ ethnicityCat2, d2 %>% select(subid, ethnicityCat2, age) %>% distinct(.keep_all = T))
# 
# # gender
# d2 %>%
#   select(subid, ethnicityCat2, gender) %>%
#   distinct(.keep_all = T) %>%
#   count(ethnicityCat2, gender)
# 
# summary(with(d2 %>% 
#                select(subid, ethnicityCat2, gender) %>% 
#                distinct(.keep_all = T) %>%
#                mutate(gender = factor(gender)),
#              table(gender, ethnicityCat2)))
# 
# # ethnicity
# d2 %>% 
#   select(subid, ethnicityCat2, ethnicity) %>%
#   mutate(east_asian = grepl("east asian", ethnicity),
#          white = grepl("white", ethnicity),
#          latino = grepl("latino", ethnicity),
#          middle_eastern = grepl("middle eastern", ethnicity),
#          native = grepl("native", ethnicity),
#          south_asian = grepl("south", ethnicity)) %>%
#   distinct(.keep_all = T) %>%
#   gather(ethnicityTF, TF, -subid, -ethnicity, -ethnicityCat2) %>%
#   filter(TF) %>%
#   count(ethnicityCat2, ethnicityTF)
# 
# # condition
# d2 %>% 
#   select(subid, ethnicityCat2, character) %>%
#   distinct(.keep_all = T) %>%
#   count(ethnicityCat2, character)
# 
# summary(with(d2 %>% 
#                select(subid, ethnicityCat2, character) %>% 
#                distinct(.keep_all = T) %>%
#                mutate(character = factor(character)),
#              table(character, ethnicityCat2)))
# 
# # plot!
# d2_byculture <- multi_boot(d2_bycond %>% left_join(d2 %>% select(subid, ethnicityCat2)), 
#                            column = "responseNum",
#                            summary_groups = c("ethnicityCat2", "character", 
#                                               "capacity", "capWordingShort"),
#                            statistics_functions = c("mean", "ci_lower", "ci_upper"))
# 
# d2_byculture2 <- d2_byculture %>%
#   ungroup() %>%
#   mutate(ethnicityCat2 = factor(ethnicityCat2, labels = c("A", "W"))) %>%
#   left_join(factors2 %>%
#               mutate(factor = factor(factor)) %>%
#               arrange(factor, desc(loading_abs)) %>% 
#               rownames_to_column(var = "order") %>%
#               mutate(order = as.numeric(order)))
# 
# # alphabetical order
# ggplot(d2_byculture2 %>% filter(!is.na(ethnicityCat2)), 
#        aes(x = ethnicityCat2, y = mean, color = character, fill = character)) +
#   facet_wrap(~ capacity, ncol = 8) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# # ordered by factor
# ggplot(d2_byculture2 %>% filter(!is.na(ethnicityCat2)), 
#        aes(x = ethnicityCat2, y = mean, color = character, fill = character)) +
#   facet_wrap(~ reorder(capacity, order), ncol = 8) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# # factor 1 only
# ggplot(d2_byculture2 %>% filter(!is.na(ethnicityCat2)) %>% filter(factor == "MR1"), 
#        aes(x = ethnicityCat2, y = mean, color = character, fill = character)) +
#   facet_wrap(~ reorder(capacity, order), ncol = 5) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# # factor 2 only
# ggplot(d2_byculture2 %>% filter(!is.na(ethnicityCat2)) %>% filter(factor == "MR2"), 
#        aes(x = ethnicityCat2, y = mean, color = character, fill = character)) +
#   facet_wrap(~ reorder(capacity, order), ncol = 5) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# # factor 3 only
# ggplot(d2_byculture2 %>% filter(!is.na(ethnicityCat2)) %>% filter(factor == "MR3"), 
#        aes(x = ethnicityCat2, y = mean, color = character, fill = character)) +
#   facet_wrap(~ reorder(capacity, order), ncol = 5) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
#   theme_bw() +
#   theme(text = element_text(size = 18),
#         legend.position = "top")
# 
# d_byculture_scores <- d0 %>%
#   select(subid, ethnicityCat2, age, character) %>%
#   filter(character != "elephant") %>%
#   distinct() %>%
#   full_join(scores_children) %>%
#   left_join(ages %>% select(subid, age)) %>%
#   mutate(character = factor(character)) %>%
#   filter(!is.na(score_MR1) & !is.na(score_MR2) & !is.na(score_MR3), !is.na(age)) %>%
#   gather(factor, score, starts_with("score_")) %>%
#   mutate(factor = factor(factor)) %>%
#   multi_boot(column = "score",
#              summary_groups = c("ethnicityCat2", "character", "factor"),
#              statistics_functions = c("mean", "ci_lower", "ci_upper"))
# 
# # plot
# ggplot(d_byculture_scores %>% filter(!is.na(ethnicityCat2)) %>% ungroup %>% mutate(ethnicityCat2 = factor(ethnicityCat2, labels = c("A", "W"))), 
#        aes(x = ethnicityCat2, y = mean, color = character, fill = character)) +
#   facet_wrap("factor") +
#   theme_bw() +
#   theme(text = element_text(size = 24),
#         legend.position = "top") +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2)
# 
# # analyze?
# d_reg3 <- d_reg2 %>%
#   left_join(d2 %>% select(subid, ethnicityCat2)) %>%
#   filter(!is.na(ethnicityCat2))
# 
# # set contrasts
# contrasts(d_reg3$factor) <- cbind(MR1 = c(1, 0, 0), # MAKE SURE TO DOUBLE-CHECK!!
#                                   MR3 = c(0, 0, 1))
# contrasts(d_reg3$character) <- cbind(robot = c(-1, 1))
# contrasts(d_reg3$ethnicityCat2) <- cbind(W = c(-1, 1))
# 
# r1 <- lmer(score ~ character * factor + (1 | subid), d_reg3)
# r2 <- lmer(score ~ character * factor + ethnicityCat2 + (1 | subid), d_reg3)
# r3 <- lmer(score ~ character * factor * ethnicityCat2 + (1 | subid), d_reg3)
# anova(r1, r2, r3)
# # summary(r1)
# # summary(r2)
# summary(r3)
# 
# # robot only
# robot_r1 <- lmer(score ~ factor + (1 | subid), d_reg3 %>% filter(character == "robot"))
# robot_r2 <- lmer(score ~ factor + ethnicityCat2 + (1 | subid), d_reg3 %>% filter(character == "robot"))
# robot_r3 <- lmer(score ~ factor * ethnicityCat2 + (1 | subid), d_reg3 %>% filter(character == "robot"))
# anova(robot_r1, robot_r2, robot_r3)
# # summary(robot_r1)
# # summary(robot_r2)
# summary(robot_r3)


