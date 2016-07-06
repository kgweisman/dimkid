library(dplyr)
library(tidyr)
library(stats)
library(psych)

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/compiled/dimkid_p01-14_2016-04-01.csv")

d1 <- d %>%
  select(cap_short, response_coded, subid) %>%
  mutate(response_coded = as.numeric(ifelse(response_coded %in% c(0, 0.5, 1),
                                            as.numeric(as.character(response_coded)), 
                                            NA))) %>%
  filter(cap_short != "na") %>%
  spread(cap_short, response_coded)

d2 <- data.frame(d1[,-1], row.names = d1[,1])

d3 <- data.frame(d2[, !apply(is.na(d2), 2, any)])

cor3 <- cor(d3, method = "spearman")
cor4 <- cor3
cor4[is.na(cor4)] <- 1

m <- as.matrix(d3)
heatmap(m)

cluster <- hclust(dist(t(m)))
plot(cluster)


VSS.scree(cor4)
fa(cor4, nfactors = 13, rotate = "none")
fa(cor4, nfactors = 3, rotate = "none")
fa(cor4, nfactors = 13, rotate = "varimax")
fa(cor4, nfactors = 3, rotate = "varimax")
fa.sort(fa(cor4, nfactors = 4, rotate = "varimax")$loadings)
fa.sort(fa(cor4, nfactors = 3, rotate = "varimax")$loadings)
fa.sort(fa(cor4, nfactors = 2, rotate = "varimax")$loadings)

