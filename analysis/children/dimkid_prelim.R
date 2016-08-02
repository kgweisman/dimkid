library(dplyr)
library(tidyr)
library(stats)
library(psych)
library(ggplot2)
library(tibble)
library(GPArotation)

# kara pilot
d_pilot <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/compiled/dimkid_p01-14_2016-04-01.csv")

d1_pilot <- d_pilot %>%
  select(cap_short, response_coded, subid) %>%
  mutate(response_coded = as.numeric(ifelse(response_coded %in% c(0, 0.5, 1),
                                            as.numeric(as.character(response_coded)),
                                            NA))) %>%
  filter(cap_short != "na") %>%
  spread(cap_short, response_coded)

# lydia, olivia, allie run

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2016-08-01_anonymized.csv")

qplot(d$rt, bins = 100) +
  scale_x_log10(breaks = seq(0, 1000, 100)) +
  geom_vline(xintercept = 250, color = "red")

d0 <- d %>%
  filter(rt >= 250)

d1 <- d0 %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)

# names(d1) <- gsub(" ", "\\.", names(d1))
# names(d1) <- gsub("\\-", "\\.", names(d1))

# if merging
d1_combo <- d1_pilot %>%
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
  full_join(d1)

# d1 <- d1_combo

# continue

d2 <- data.frame(d1[,-1], row.names = d1[,1])

cor3 <- cor(d2, method = "spearman", use = "complete.obs")

# m <- as.matrix(d2)
# heatmap(m)
# 
# cluster <- hclust(dist(t(m)))
# plot(cluster)

# VSS.scree(d2)
fa.parallel(d2)
fa(r = d2, nfactors = 13, rotate = "none", fm = "minres", cor = "cor")
fa(r = d2, nfactors = 13, rotate = "varimax", fm = "minres", cor = "cor")
fa.sort(fa(d2, nfactors = 7, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()

fa.parallel(d2, cor = "poly")
fa.sort(fa(d2, nfactors = 6, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) %>% View()

# make factor assignments
factors <- fa.sort(fa(d2, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) 

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
                                       NA)))) %>%
  arrange(factor, desc(loading_abs)) %>%
  select(capacity, factor, loading, loading_abs)

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
  mutate(posneg = factor(ifelse(loading < 0, "neg", "pos")))

# by condition
d1_bycond <- d0 %>%
  select(character, capacity, responseNum, subid) %>%
  filter(capacity != "na", is.na(responseNum) == F)

library(langcog)
d1_bycond_mb <- multi_boot(d1_bycond,
                           column = "responseNum",
                           summary_groups = c("character", "capacity"),
                           statistics_functions = c("mean", "ci_lower", "ci_upper")) %>% 
  full_join(factors3) %>%
  arrange(character, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order))

ggplot(d1_bycond_mb, 
       aes(x = desc(order), y = mean,
           group = character, color = character,
           label = paste0(capacity, ": ", 
                          formatC(round(loading, 2), 
                                  format = "f",
                                  digits = 2)))) +
  facet_grid(. ~ factor) +
  geom_point(stat = "identity", position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 1), width = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() +
  geom_text(aes(y = -0.18, hjust = 0, color = posneg)) +
  coord_flip()

# separate by character
d1_robot <- d0 %>%
  filter(character == "robot") %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)
d2_robot <- data.frame(d1_robot[,-1], row.names = d1_robot[,1])
fa.parallel(d2_robot)

fa.sort(fa(d2_robot, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()

d1_beetle <- d0 %>%
  filter(character == "beetle") %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)
d2_beetle <- data.frame(d1_beetle[,-1], row.names = d1_beetle[,1])
fa.parallel(d2_beetle)

fa.sort(fa(d2_beetle, nfactors = 2, rotate = "varimax")$loadings[]) %>% View()


