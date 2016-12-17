# PRELIMINARIES ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stats)
library(psych)
library(ggplot2)
library(tibble)
library(GPArotation)

# clear environment
rm(list=ls())
graphics.off()

# READ IN DATA ----------------------------------------------------------------

# kara pilot
d_pilot <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/compiled/dimkid_p01-14_2016-04-01.csv")

d1_pilot <- d_pilot %>%
  select(cap_short, response_coded, subid) %>%
  mutate(response_coded = as.numeric(ifelse(response_coded %in% c(0, 0.5, 1),
                                            as.numeric(as.character(response_coded)),
                                            NA))) %>%
  filter(cap_short != "na") %>%
  spread(cap_short, response_coded)

# lydia, olivia, allie (summer 2016) + nicky, dru, ariel, olivia (fall 2016)

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2016-12-16_anonymized.csv")

# TIDY DATA -------------------------------------------------------------------

# filter by condition (no elephant!)
d0 <- d %>%
  filter(character %in% c("beetle", "robot"))

# examine and filter by RTs
qplot(d0$rt, bins = 100) +
  scale_x_log10(breaks = seq(0, 1000, 100)) +
  geom_vline(xintercept = 350, color = "red")

d0 <- d0 %>%
  filter(rt >= 350)

# examine and filter by ages
qplot(age, data = d0 %>% select(subid, age) %>% distinct()) +
  geom_vline(xintercept = 7, color = "red") +
  geom_vline(xintercept = 10, color = "red")

d1 <- d0 %>%
  filter(is.na(age) | (age >= 6.5 & age <10.5))

d2 <- d1 %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)

# names(d2) <- gsub(" ", "\\.", names(d2))
# names(d2) <- gsub("\\-", "\\.", names(d2))

# if merging
d2_combo <- d2_pilot %>%
  rename(communicating = communicate,
         computations = math,
         depressed = sad,
         disrespected = hurt_feelings,
         fear = scared,
         guilt = guilty,
         nauseated = sick,
         odors = smells,
         pride = proud,
         reasoning = thinking,
         recognizing = recognize,
         remembering = remember,
         seeing = see,
         self_restraint = self_control,
         temperature = temperatures) %>%
  mutate(conscious = ifelse(!is.na(conscious), consious,
                            ifelse(!is.na(aware), aware,
                                   NA)),
         free_will = ifelse(!is.na(free_will), consious,
                            ifelse(!is.na(decide), decide,
                                   NA)),
         intentions = ifelse(!is.na(intentions), consious,
                            ifelse(!is.na(plan), plan,
                                   NA))) %>%
  select(-aware, -decide, -plan) %>%
  gather(item, response, -subid) %>%
  mutate(response = response * 2) %>%
  spread(item, response) %>%
  full_join(d2)

# d2 <- d2_combo

# continue

d3 <- data.frame(d2[,-1], row.names = d2[,1])

cor3 <- cor(d3, method = "spearman", use = "complete.obs")

# DEMOGRAPHICS ----------------------------------------------------------------

# age 
d1 %>% 
  select(subid, age) %>%
  distinct(.keep_all = T) %>%
  summarise(mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T),
            median_age = median(age, na.rm = T),
            min_age = min(age, na.rm = T),
            max_age = max(age, na.rm = T))

qplot(d1 %>% distinct(subid, .keep_all = T) %>% select(age), bins = 18)

# gender
d1 %>%
  select(subid, gender) %>%
  distinct(.keep_all = T) %>%
  count(gender)

# ethnicity
d1 %>% 
  select(subid, ethnicity) %>%
  mutate(east_asian = grepl("east asian", ethnicity),
         white = grepl("white", ethnicity),
         latino = grepl("latino", ethnicity),
         middle_eastern = grepl("middle eastern", ethnicity),
         native = grepl("native", ethnicity),
         south_asian = grepl("south", ethnicity)) %>%
  distinct(.keep_all = T) %>%
  gather(ethnicityTF, TF, -subid, -ethnicity) %>%
  filter(TF) %>%
  count(ethnicityTF)

# condition
d1 %>% 
  select(subid, character) %>%
  distinct(.keep_all = T) %>%
  count(character)

# HEATMAP, CLUSTERING ---------------------------------------------------------

m <- as.matrix(d3)
heatmap(m)

cluster <- hclust(dist(t(m)))
plot(cluster)

d2_young <- d1 %>%
  filter(age < median(d1$age, na.rm = T)) %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)
d3_young <- data.frame(d2_young[,-1], row.names = d2_young[,1])
m_young <- as.matrix(d3_young)
cluster_young <- hclust(dist(t(m_young)))
plot(cluster_young)
d2_young %>% count()

d2_old <- d1 %>%
  filter(age >= median(d1$age, na.rm = T)) %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)
d3_old <- data.frame(d2_old[,-1], row.names = d2_old[,1])
m_old <- as.matrix(d3_old)
cluster_old <- hclust(dist(t(m_old)))
plot(cluster_old)
d2_old %>% count()

# FACTOR ANALYSIS -------------------------------------------------------------

# pearson correlations
VSS.scree(d3)
fa.parallel(d3)
fa(r = d3, nfactors = 13, rotate = "none", fm = "minres", cor = "cor")
fa(r = d3, nfactors = 13, rotate = "varimax", fm = "minres", cor = "cor")
# fa.sort(fa(d3, nfactors = 7, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d3, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d3, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()

# polychoric correlations
fa.parallel(d3, cor = "poly")
fa(d3, nfactors = 13, rotate = "none", cor = "poly")
fa(d3, nfactors = 13, rotate = "varimax", cor = "poly")
fa.sort(fa(d3, nfactors = 7, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d3, nfactors = 5, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d3, nfactors = 4, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d3, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) %>% View()

# separate by character
d1_robot <- d0 %>%
  filter(character == "robot") %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)
d3_robot <- data.frame(d1_robot[,-1], row.names = d1_robot[,1])
fa.parallel(d3_robot)

# fa.sort(fa(d3_robot, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()

d1_beetle <- d0 %>%
  filter(character == "beetle") %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)
d3_beetle <- data.frame(d1_beetle[,-1], row.names = d1_beetle[,1])
# fa.parallel(d3_beetle)

# fa.sort(fa(d3_beetle, nfactors = 2, rotate = "varimax")$loadings[]) %>% View()

# PLOTTING --------------------------------------------------------------------

# make factor assignments
factors <- fa.sort(fa(d3, nfactors = 3, rotate = "varimax", cor = "cor")$loadings[]) 

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
d1_bycond <- d0 %>%
  select(character, capacity, capWording, responseNum, subid) %>%
  filter(character != "elephant") %>%
  filter(capacity != "na", is.na(responseNum) == F) %>%
  mutate(capWordingShort = gsub(" --.*", "", capWording)) %>%
  select(-capWording)

library(langcog)
d1_bycond_mb <- multi_boot(d1_bycond,
                           column = "responseNum",
                           summary_groups = c("character", "capacity", "capWordingShort"),
                           statistics_functions = c("mean", "ci_lower", "ci_upper")) %>% 
  full_join(factors3) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order)

ggplot(d1_bycond_mb, 
       aes(x = desc(order*2), y = mean,
           group = character, color = character, shape = character,
           label = capWordingShort)) +
  facet_grid(. ~ factorName) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
  scale_shape_manual(values = c(19, 15)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = .75), width = 0) +
  geom_text(aes(y = -0.18, hjust = 0), color = d1_bycond_mb$textColor, size = 6) +
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
  

# USING ADULT FACTOR LOADINGS -------------------------------------------------

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
d1_bycond_ADULT_mb <- multi_boot(d1_bycond, # get from above
                           column = "responseNum",
                           summary_groups = c("character", "capacity", "capWordingShort"),
                           statistics_functions = c("mean", "ci_lower", "ci_upper")) %>% 
  full_join(factors3_ADULT) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order)

ggplot(d1_bycond_ADULT_mb, 
       aes(x = desc(order*2), y = mean,
           group = character, color = character, shape = character,
           label = capWordingShort)) +
  facet_grid(. ~ factorName) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
  scale_shape_manual(values = c(19, 15)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = .75), width = 0) +
  geom_text(aes(y = -0.18, hjust = 0), color = d1_bycond_ADULT_mb$textColor, size = 6) +
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

ggplot(d1_bycond %>% full_join(factors3_ADULT), 
       aes(x = desc(order*2),
           fill = factor(responseNum), 
           label = capWordingShort)) +
  facet_grid(character ~ factorName) +
  geom_bar(position = "fill") +
  geom_text(aes(y = 0, hjust = 0)) +
  labs(title = "Children's responses, by adult-derived factors",
       y = "\nProportion of responses",
       x = "Capacity\n",
       color = "Character: ", shape = "Character: ") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 24),
        # axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top")

# USING ADULT FACTOR LOADINGS, by age -----------------------------------------

# read in ages
ages <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/dimkid_participant_ages_2016-12-16.csv") %>%
  select(-age_formula, -comments) %>%
  mutate(ethnicityCat1 = 
           factor(ifelse(is.na(ethnicity), NA,
                         ifelse(ethnicity == "white, caucasian, or european american",
                                "euro-american", "multicultural"))),
         ethnicityCat2 = 
           factor(ifelse(is.na(ethnicity), NA,
                         ifelse(ethnicity == "white, caucasian, or european american",
                                "euro-american",
                                ifelse(grepl("east asian", ethnicity) |
                                         grepl("south", ethnicity) |
                                         grepl("chinese", ethnicity) |
                                         grepl("filipino", ethnicity),
                                       "asian", NA)))),
         age = as.numeric(as.character(age)))

# make age thing
d1_bycond_age <- d1_bycond %>%
  left_join(ages %>% select(subid, age)) %>%
  filter(!is.na(age)) %>%
  mutate(ageCat3 = ifelse(age < 8, "7", ifelse(age < 9, "8", ifelse(age < 10, "9", NA)))) %>%
  mutate(ageCat2 = ifelse(age < median(age), "young", ifelse(age < 10, "old", NA)))

# by condition
d1_bycond_ADULT_AGE_mb <- multi_boot(d1_bycond_age, # get from above
                                 column = "responseNum",
                                 summary_groups = c("ageCat2", 
                                                    "character", "capacity", "capWordingShort"),
                                 statistics_functions = c("mean", "ci_lower", "ci_upper")) %>% 
  full_join(factors3_ADULT) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order) %>%
  filter(!is.na(ageCat2))

ggplot(d1_bycond_ADULT_AGE_mb, 
       aes(x = desc(order*2), y = mean,
           group = character, color = character, shape = character,
           label = capWordingShort)) +
  facet_grid(ageCat2 ~ factorName) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 0.5, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(width = .75), size = 4) +
  scale_shape_manual(values = c(19, 15)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = .75), width = 0) +
  geom_text(aes(y = -0.18, hjust = 0), color = d1_bycond_ADULT_AGE_mb$textColor, size = 6) +
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

# BASIC REGRESSION ANALYSES ---------------------------------------------------

d_reg <- d1_bycond %>% 
  full_join(factors3_ADULT) %>%
  left_join(ages %>% select(subid, age)) %>%
  mutate(ageCat3 = ifelse(age < 8, "7", ifelse(age < 9, "8", ifelse(age < 10, "9", NA))),
         ageCat2 = ifelse(age < median(age), "young", ifelse(age < 10, "old", NA))) %>%
  filter(posneg == "pos", !is.na(age), character != "elephant") %>%
  mutate(character = factor(character))

# set contrasts
contrasts(d_reg$factor) <- cbind(socemo = c(0, 1, 0),
                                 percog = c(0, 0, 1))
# contrasts(d_reg$factor) <- cbind(Ph.v.UGM = c(1, 0, -1),
#                                  SE.v.UGM = c(0, 1, -1))
# contrasts(d_reg$factor) <- cbind(SEPC.v.Ph = c(-2, 1, 1),
#                                  SE.v.PC = c(0, 1, -1))
# contrasts(d_reg$character) <- cbind(robot = c(0, 1))
contrasts(d_reg$character) <- cbind(robot = c(-1, 1))

r1 <- lmer(responseNum ~ character * factor + (1 | subid) + (1 | capacity), d_reg)
summary(r1)
r2 <- lmer(responseNum ~ character * factor + scale(age) + (1 | subid) + (1 | capacity), d_reg)
summary(r2)
r3 <- lmer(responseNum ~ character * factor * scale(age) + (1 | subid) + (1 | capacity), d_reg)
summary(r3)
anova(r1, r2, r3)

# ordinal
library(ordinal)
r4 <- clmm(factor(responseNum) ~ character * factor + (1 | subid) + (1 | capacity), d_reg)
summary(r4)
r5 <- clmm(factor(responseNum) ~ character * factor + scale(age) + (1 | subid) + (1 | capacity), d_reg)
summary(r5)
r6 <- clmm(factor(responseNum) ~ character * factor * scale(age) + (1 | subid) + (1 | capacity), d_reg)
summary(r6)
anova(r4, r5, r6)
