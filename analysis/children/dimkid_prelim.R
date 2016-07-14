library(dplyr)
library(tidyr)
library(stats)
library(psych)
library(ggplot2)
library(tibble)

# children

# # kara pilot
# d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/compiled/dimkid_p01-14_2016-04-01.csv")
# 
# d1 <- d %>%
#   select(cap_short, response_coded, subid) %>%
#   mutate(response_coded = as.numeric(ifelse(response_coded %in% c(0, 0.5, 1),
#                                             as.numeric(as.character(response_coded)),
#                                             NA))) %>%
#   filter(cap_short != "na") %>%
#   spread(cap_short, response_coded)

# lydia, olivia, allie run

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2016-07-14_anonymized.csv")

qplot(d$rt, bins = 100) +
  scale_x_log10(breaks = seq(0, 1000, 100)) +
  geom_vline(xintercept = 250, color = "red")

d0 <- d %>%
  filter(rt >= 250)

d1 <- d0 %>%
  select(capacity, responseNum, subid) %>%
  filter(capacity != "na") %>%
  spread(capacity, responseNum)

# continue

d2 <- data.frame(d1[,-1], row.names = d1[,1])

cor3 <- cor(d2, method = "spearman", use = "complete.obs")

m <- as.matrix(d2)
heatmap(m)

cluster <- hclust(dist(t(m)))
plot(cluster)

VSS.scree(cor3)
fa(cor3, nfactors = 13, rotate = "none")
fa(cor3, nfactors = 3, rotate = "none")
fa(cor3, nfactors = 13, rotate = "varimax")
fa(cor3, nfactors = 3, rotate = "varimax")
fa.sort(fa(cor3, nfactors = 4, rotate = "varimax")$loadings)
fa.sort(fa(cor3, nfactors = 3, rotate = "varimax")$loadings)
fa.sort(fa(cor3, nfactors = 2, rotate = "varimax")$loadings)

