library(dplyr)
library(tidyr)
library(stats)
library(psych)
library(ggplot2)
library(tibble)

# READ IN DATA ----------------------------------------------------------------

# # run 01
d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-01_2016-06-05_anonymized.csv")

# run 02
# d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-02_2016-07-19_anonymized.csv")          

# TIDY DATA -------------------------------------------------------------------

# plot log(rts)
qplot(d$rt, bins = 100) + 
  scale_x_log10(breaks = seq(0, 1000, 100)) +
  geom_vline(xintercept = 250, color = "red")

d0 <- d %>%
  filter(rt >= 250)

d1 <- d0 %>%
  select(capacity, responseNum, subid) %>%
  spread(capacity, responseNum)

d2 <- data.frame(d1[,-1], row.names = d1[,1])

cor3 <- cor(d2, method = "spearman", use = "complete.obs")

# HEATMAP, CLUSTERING ---------------------------------------------------------

# m <- as.matrix(d2)
# heatmap(m)
# 
# cluster <- hclust(dist(t(m)))
# plot(cluster)

# FACTOR ANALYSIS -------------------------------------------------------------

# pearson correlations
# VSS.scree(d2)
fa.parallel(d2)
fa(r = d2, nfactors = 13, rotate = "none", fm = "minres", cor = "cor")
fa(r = d2, nfactors = 13, rotate = "varimax", fm = "minres", cor = "cor")
fa.sort(fa(d2, nfactors = 7, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 4, rotate = "varimax")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()

# polychoric correlations
fa.parallel(d2, cor = "poly")
fa(r = d2, nfactors = 13, rotate = "none", fm = "minres", cor = "poly")
fa(r = d2, nfactors = 13, rotate = "varimax", fm = "minres", cor = "poly")
fa.sort(fa(d2, nfactors = 7, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 4, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) %>% View()

# #### EXPLORATORY ------------------------------------------------------------
# 
# # remove items with mandatory definitions
# d3a <- d2[!grepl("\\.\\.\\.", names(d2))]
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
# d3b <- d2 %>%
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
# d3c <- d2 %>%
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
