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
fa.sort(fa(d2, nfactors = 6, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 4, rotate = "varimax", cor = "poly")$loadings[]) %>% View()
fa.sort(fa(d2, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) %>% View()

# PLOTTING --------------------------------------------------------------------
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
                                       NA))),
         factorName = ifelse(loading == MR1, "F1: Social-emotional",
                             ifelse(loading == MR2, "F2: Physiological",
                                    ifelse(loading == MR3, "F3: Perceptual",
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
         textColor = ifelse(loading < 0, "black", "dodgerblue3"))

# by condition
d1_bycond <- d0 %>%
  select(charName, capacity, capWording, responseNum, subid) %>%
  filter(capacity != "na", is.na(responseNum) == F) %>%
  mutate(capWordingShort = gsub(" --.*", "", capWording)) %>%
  select(-capWording)

library(langcog)
d1_bycond_mb <- multi_boot(d1_bycond,
                           column = "responseNum",
                           summary_groups = c("charName", "capacity", "capWordingShort"),
                           statistics_functions = c("mean", "ci_lower", "ci_upper")) %>% 
  full_join(factors3) %>%
  arrange(charName, factor, desc(loading_abs)) %>%
  rownames_to_column(var = "full_order") %>%
  mutate(full_order = as.numeric(full_order)) %>%
  arrange(factorName, full_order)

ggplot(d1_bycond_mb, 
       aes(x = desc(order*2), y = mean,
           group = charName, color = charName, shape = charName,
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
factors_poly_varimax_3f_0 <- fa.sort(fa(d2, nfactors = 3, rotate = "varimax", cor = "poly")$loadings[]) %>%
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

factors_poly_varimax_3f <- 
  full_join(factors_poly_varimax_3f_0 %>%
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
write.csv(factors_poly_varimax_3f, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/plots and tables/factors_poly_varimax_3f (adults)")

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
