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

# # run 01 (3-point scale)
d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-01_2016-06-05_anonymized.csv")[-1]

# # run 02 (7-point scale)
# d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-02_2016-07-19_anonymized.csv")[-1]

# run 03 (3-point scale, original wording for 'free will' and 'intentions')
# d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-03_2016-12-08_anonymized.csv")[-1]

# TIDY DATA -------------------------------------------------------------------

# filter by condition (no elephant!)
d0 <- d %>%
  rename(character = charName) %>%
  filter(character %in% c("beetle", "robot"))

# examine durations
qplot(d0$duration)
d0 %>% summarise(mean = mean(duration, na.rm = T),
                 median = median(duration, na.rm = T),
                 min = min(duration, na.rm = T),
                 max = max(duration, na.rm = T))

# examine and filter by RTs
qplot(d0$rt, bins = 100) +
  scale_x_log10(breaks = seq(0, 1000, 100)) +
  geom_vline(xintercept = 250, color = "red")

d0 %>% 
  mutate(short_rt = ifelse(rt < 250, T, F)) %>% 
  count(short_rt) %>%
  mutate(percent = n/sum(n))

d1 <- d0 %>%
  filter(rt >= 250)

# finish tidying
d2 <- d1 %>%
  select(capacity, responseNum, subid) %>%
  filter(!is.na(capacity), capacity != "na") %>%
  spread(capacity, responseNum)

d3 <- data.frame(d2[,-1], row.names = d2[,1])

cor3 <- cor(d3, method = "spearman", use = "complete.obs")

# DEMOGRAPHICS ----------------------------------------------------------------

# total n
d1 %>% 
  select(subid) %>%
  distinct(.keep_all = T) %>%
  count()

# age 
d1 %>% 
  select(subid, age) %>%
  distinct(.keep_all = T) %>%
  summarise(mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T),
            median_age = median(age, na.rm = T),
            min_age = min(age, na.rm = T),
            max_age = max(age, na.rm = T))

qplot(d1 %>% distinct(subid, .keep_all = T) %>% select(age), bins = 18) +
  geom_vline(xintercept = median(d1$age, na.rm = T), color = "red")

d1 %>% 
  distinct(subid, .keep_all = T) %>% 
  select(age, character) %>%
  group_by(character) %>% 
  summarise(median = median(age, na.rm = T))

t.test(age ~ character, d1)

# gender
d1 %>%
  select(subid, gender) %>%
  distinct(.keep_all = T) %>%
  count(gender)

# ethnicity
d1 %>% 
  select(subid, ethnicity) %>%
  # MIGHT BE SOME PROBLEMS HERE
  mutate(east_asian = grepl("eastAsian", ethnicity),
         white = grepl("white", ethnicity),
         latino = grepl("hispanicLatino", ethnicity),
         middle_eastern = grepl("middleEastern", ethnicity),
         native = grepl("native", ethnicity),
         south_asian = grepl("southAsian", ethnicity),
         black = grepl("black", ethnicity)) %>%
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

m1 <- as.matrix(d3[complete.cases(d3),]) # remove rows with NAs
heatmap(m1)

m1 <- as.matrix(d3) # keep NAs

cluster <- hclust(dist(t(m1)))
plot(cluster)

# FACTOR ANALYSIS -------------------------------------------------------------

# pearson correlations
VSS.scree(d3)
fa.parallel(d3)
fa(r = d3, nfactors = 13, rotate = "none", fm = "minres", cor = "cor")
fa(r = d3, nfactors = 13, rotate = "varimax", fm = "minres", cor = "cor")
fa.sort(fa(d3, nfactors = 13, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d3, nfactors = 7, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d3, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d3, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()

fa.sort(fa(d3, nfactors = 4, rotate = "oblimin")$loadings[]) %>% View() # oblimin rotation
fa.sort(fa(d3, nfactors = 3, rotate = "oblimin")$loadings[]) %>% View() # oblimin rotation

# # polychoric correlations
# fa.parallel(d3, cor = "poly")
# fa(d3, nfactors = 13, rotate = "none", cor = "poly")
# fa(d3, nfactors = 13, rotate = "varimax", cor = "poly")
# fa.sort(fa(d3, nfactors = 7, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
# fa.sort(fa(d3, nfactors = 6, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
# fa.sort(fa(d3, nfactors = 5, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
# fa.sort(fa(d3, nfactors = 4, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
# fa.sort(fa(d3, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) %>% View()

# separate by character
d1_robot <- d0 %>%
  filter(character == "robot") %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)
d3_robot <- data.frame(d1_robot[,-1], row.names = d1_robot[,1])

# fa.parallel(d3_robot)
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

# choose correlation type
cor_type <- "cor"
# cor_type <- "poly"

# choose rotation type
rot_type <- "varimax"
# rot_type <- "oblimin"

# make factor assignments
factors <- fa.sort(fa(d3, nfactors = 3, rotate = rot_type, cor = cor_type)$loadings[]) 

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
  labs(title = "Adults' responses, by adult-derived factors",
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

# EXPORT LOADINGS -------------------------------------------------------------

# make factor assignments: polychoric correlations, varimax rotation, 3 factors
factors_0 <- fa.sort(fa(d3, nfactors = 3, rotate = rot_type, cor = cor_type)$loadings[]) %>%
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
                                       NA)))) %>%
  arrange(factor, desc(loading_abs)) %>%
  select(capacity, factor, loading, loading_abs)

factors <- 
  full_join(factors_0 %>%
              filter(factor == "MR1") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order)),
            factors_poly_varimax_3f_0 %>%
              filter(factor == "MR2") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  full_join(factors_poly_varimax_3f_0 %>%
              filter(factor == "MR3") %>%
              rownames_to_column(var = "order") %>%
              mutate(order = as.numeric(order))) %>%
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")),
         factorName = factor(ifelse(factor == "MR1", "F1: Social-emotional",
                                    ifelse(factor == "MR2", "F2: Physiological",
                                           ifelse(factor == "MR3", "F3: Perceptual",
                                                  NA)))))

# export to csv
if(cor_type == "poly") {
  write.csv(factors, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/plots and tables/factors_poly_varimax_3f (adults)")
}

if(cor_type == "cor") {
  write.csv(factors_0, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/plots and tables/factors_cor_varimax_3f (adults)")
}

# #### EXPLORATORY ------------------------------------------------------------
# 
# # remove items with mandatory definitions
# d3a <- d3[!grepl("\\.\\.\\.", names(d3))]
# 
# cor3a <- cor(d3a, method = "spearman", use = "complete.obs")
# 
# VSS.scree(d3a)
# fa(d3a, nfactors = 13, rotate = "none")
# fa(d3a, nfactors = 13, rotate = "none", n.obs = 200)
# # fa(d3a, nfactors = 4, rotate = "none")
# # fa(d3a, nfactors = 3, rotate = "none")
# fa(d3a, nfactors = 13, rotate = "varimax")
# # fa(d3a, nfactors = 4, rotate = "varimax")
# # fa(d3a, nfactors = 3, rotate = "varimax")
# fa.sort(fa(d3a, nfactors = 5, rotate = "varimax")$loadings[]) %>% View()
# fa.sort(fa(d3a, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
# fa.sort(fa(d3a, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()
# # fa.sort(fa(d3a, nfactors = 2, rotate = "varimax")$loadings[]) %>% View()
# 
# 
# # remove "cognitive" agency items
# d3b <- d3 %>%
#   select (-decide.what.to.do, -figure.out.how.to.do.things, -have.self.control....like.when.you.stop.yourself.from.doing.something.you.shouldn.t.do)
# 
# cor3b <- cor(d3b, method = "spearman", use = "complete.obs")
# 
# VSS.scree(d3b)
# fa(d3b, nfactors = 13, rotate = "none")
# # fa(d3b, nfactors = 4, rotate = "none")
# # fa(d3b, nfactors = 3, rotate = "none")
# fa(d3b, nfactors = 13, rotate = "varimax")
# # fa(d3b, nfactors = 4, rotate = "varimax")
# # fa(d3b, nfactors = 3, rotate = "varimax")
# # fa.sort(fa(d3b, nfactors = 5, rotate = "varimax")$loadings[]) %>% View()
# # fa.sort(fa(d3b, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
# fa.sort(fa(d3b, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()
# # fa.sort(fa(d3b, nfactors = 2, rotate = "varimax")$loadings[]) %>% View()
# 
# # remove (a priori) "substnantially reworded" agency items (see Mental capacity items 2016-06-30.doc)
# d3c <- d3 %>%
#   select (-do.math, -be.aware.of.things, -feel.sad, -sense.whether.something.is.close.by.or.far.away, -get.hurt.feelings, -feel.scared, -decide.what.to.do, -make.plans, -know.what.s.nice.and.what.s.mean, -feel.sick....like.when.you.feel.like.you.might.throw.up, -smell.things, -figure.out.how.to.do.things, -be.aware.of.itself, -have.self.control....like.when.you.stop.yourself.from.doing.something.you.shouldn.t.do, -hear.sounds)
# 
# cor3c <- cor(d3c, method = "spearman", use = "complete.obs")
# 
# VSS.scree(d3c)
# fa(d3c, nfactors = 13, rotate = "none")
# # fa(d3c, nfactors = 4, rotate = "none")
# # fa(d3c, nfactors = 3, rotate = "none")
# fa(d3c, nfactors = 13, rotate = "varimax")
# # fa(d3c, nfactors = 4, rotate = "varimax")
# # fa(d3c, nfactors = 3, rotate = "varimax")
# # fa.sort(fa(d3c, nfactors = 5, rotate = "varimax")$loadings[]) %>% View()
# # fa.sort(fa(d3c, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
# fa.sort(fa(d3c, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()
# # fa.sort(fa(d3c, nfactors = 2, rotate = "varimax")$loadings[]) %>% View()
# 
# 
