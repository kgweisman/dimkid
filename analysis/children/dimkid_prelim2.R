d100 <- d1 %>%
  filter(CATCH == 1) %>%
  gather(mc, score, happy:CATCH) %>%
  mutate(score = ifelse(score %in% c(-3, -2), -1,
                        ifelse(score %in% c(3, 2), 1,
                               0))) %>%
  spread(mc, score) %>%
  select(happy:pride) %>%
  mutate_each(funs(as.integer))

fa(d100,
   nfactors = 39,
   rotate = "none",
   cor = "poly")

efa_d100_all_rotatedN <- fa(d100,
                            nfactors = 3,
                            rotate = "varimax",
                            cor = "poly")

# get loadings for each factor
efa_d100_all_rotatedN_loadings <- loadings(efa_d100_all_rotatedN)[] %>%
  data.frame() %>% 
  add_rownames(var = "mc")

#####
# CHILDREN

dimkid <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/compiled/dimkid_p01-14_2016-04-01.csv")

dimkid2 <- dimkid %>%
  select(subid, cap_short, response_coded) %>%
  rename(mc = cap_short,
         score = response_coded) %>%
  filter(mc != "na", score != "na") %>%
  mutate(score = as.integer(as.character(
    factor(score, levels = c(0, 0.5, 1), labels = c(-1, 0, 1))))) %>%
  group_by(subid) %>%
  spread(mc, score) %>%
  select(-subid)

dimkid3 <- dimkid2 %>%
  select(-aware, -decide, -plan, -self_aware, -self_control)

fa(dimkid3,
   nfactors = 30,
   rotate = "none",
   cor = "poly")

efa_dimkid3_all_rotatedN <- fa(dimkid3,
                               nfactors = 3,
                               rotate = "varimax",
                               cor = "poly")

# get loadings for each factor
efa_dimkid3_all_rotatedN_loadings <- loadings(efa_dimkid3_all_rotatedN)[] %>%
  data.frame() %>% 
  add_rownames(var = "mc")

