library(dplyr)
library(tidyr)
library(stats)
library(psych)
library(ggplot2)
library(tibble)

# # children
# d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/compiled/dimkid_p01-14_2016-04-01.csv")
# 
# d1 <- d %>%
#   select(cap_short, response_coded, subid) %>%
#   mutate(response_coded = as.numeric(ifelse(response_coded %in% c(0, 0.5, 1),
#                                             as.numeric(as.character(response_coded)), 
#                                             NA))) %>%
#   filter(cap_short != "na") %>%
#   spread(cap_short, response_coded)
# 
# d2 <- data.frame(d1[,-1], row.names = d1[,1])
# 
# d3 <- data.frame(d2[, !apply(is.na(d2), 2, any)])
# 
# cor3 <- cor(d3, method = "spearman")
# cor4 <- cor3
# cor4[is.na(cor4)] <- 1
# 
# m <- as.matrix(d3)
# heatmap(m)
# 
# cluster <- hclust(dist(t(m)))
# plot(cluster)
# 
# 
# VSS.scree(cor4)
# fa(cor4, nfactors = 13, rotate = "none")
# fa(cor4, nfactors = 3, rotate = "none")
# fa(cor4, nfactors = 13, rotate = "varimax")
# fa(cor4, nfactors = 3, rotate = "varimax")
# fa.sort(fa(cor4, nfactors = 4, rotate = "varimax")$loadings)
# fa.sort(fa(cor4, nfactors = 3, rotate = "varimax")$loadings)
# fa.sort(fa(cor4, nfactors = 2, rotate = "varimax")$loadings)




# adults
d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-01_2016-06-05_anonymized.csv")          

# plot log(rts)
qplot(d$rt, bins = 100) + 
  scale_x_log10(breaks = seq(0, 1000, 100)) +
  geom_vline(xintercept = 250, color = "red")

d0 <- d %>%
  filter(rt >= 250)

d1 <- d0 %>%
  select(capWording, responseNum, subid) %>%
  spread(capWording, responseNum)

d2 <- data.frame(d1[,-1], row.names = d1[,1])

# d3 <- data.frame(d2[, !apply(is.na(d2), 2, any)])

cor3 <- cor(d2, method = "spearman", use = "complete.obs")
cor4 <- cor3
cor4[is.na(cor4)] <- 1

m <- as.matrix(d2)
heatmap(m)

cluster <- hclust(dist(t(m)))
plot(cluster)


VSS.scree(cor4)
fa(cor4, nfactors = 13, rotate = "none")
# fa(cor4, nfactors = 4, rotate = "none")
# fa(cor4, nfactors = 3, rotate = "none")
fa(cor4, nfactors = 13, rotate = "varimax")
# fa(cor4, nfactors = 4, rotate = "varimax")
# fa(cor4, nfactors = 3, rotate = "varimax")
fa.sort(fa(cor4, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(cor4, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()
# fa.sort(fa(cor4, nfactors = 2, rotate = "varimax")$loadings[]) %>% View()

fa.sort(fa(cor4, nfactors = 3, rotate = "varimax")$loadings[]) %>% round(2) %>% data.frame() %>% rownames_to_column() %>% View()

fa.sort(fa(cor4, nfactors = 4, rotate = "varimax")$loadings[]) %>% round(2) %>% data.frame() %>% rownames_to_column() %>% View()

fa.sort(fa(cor4, nfactors = 7, rotate = "varimax")$loadings[]) %>% round(2) %>% data.frame() %>% rownames_to_column() %>% View()


# loadings <- fa(cor4, nfactors = 3, rotate = "varimax")$loadings[] %>% data.frame() %>% rownames_to_column() %>% mutate(rowname = factor(rowname)) %>% arrange(rowname)
# capacities <- data.frame(rowname = levels(d$capWording)) %>% arrange(rowname)
# 
# full_join(loadings, capacities) %>% View() 




plot(fa(cor4, nfactors = 4, rotate = "varimax"))

