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

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2016-07-27_anonymized.csv")

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
fa.sort(fa(d2, nfactors = 5, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) %>% View()

# by condition
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


