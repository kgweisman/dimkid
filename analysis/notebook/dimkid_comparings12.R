# thoughts from 2017-08-16
# use the maximal factor solution (to avoid non-independence)
# for the correlation approach: simulate the whole process and estimate error rate?
# for the strong items approach: simulate how many strong factor loadings would be held in common for two random factors
# for the CFA approach... group analysis? or modeling based on adults and fitting kids?


# SETUP ------

# NEED TO RUN dimkid_cogsci_analysis.Rmd FIRST!

# load libraries
library(lavaan)
library(semTools)

# make temp dataframe with all factor loadings for 3-factor solutions
temp <- efa_d1_rotatedN_loadings %>%
  rename(adult_MR1 = MR1, adult_MR2 = MR2, adult_MR3 = MR3) %>%
  full_join(efa_d2_rotatedN_loadings) %>%
  rename(child_MR1 = MR1, child_MR2 = MR2, child_MR3 = MR3) %>%
  column_to_rownames("capacity")

# CORRELATIONS -----

# look at correlations
corCi(temp, method = "pearson")
# corCi(temp, method = "spearman")

tempCI <- corCi(temp, method = "pearson")
tempCI$ci %>% 
  round(2) %>% 
  rownames_to_column("pair") %>%
  # should use lower or low.e? (and likewise for upper/u.e?)
  mutate(nonzero = ifelse(lower*upper > 0, TRUE, FALSE),
         factorA = substr(pair, 1, 5),
         factorB = substr(pair, 7, 11)) %>% 
  select(factorA, factorB, lower, upper, p, nonzero) %>%
  # arrange(nonzero, upper)
  arrange(factorA, factorB)

# CATTELL's SALIENT SIMILARITY INDEX (S) -----

factor_fn <- function(var){
  factor(var, levels = c("pos_salient", "hyper_plane", "neg_salient"))
}

temp2 <- temp %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  mutate(strength = ifelse(loading >= .33, "pos_salient",
                           ifelse(loading <= -.33, "neg_salient",
                                  "hyper_plane"))) %>%
  select(-loading) %>%
  spread(factor, strength) %>%
  remove_rownames() %>%
  column_to_rownames("capacity") %>%
  mutate_all(funs("factor_fn"))


cattell_s <- function(df){
  num <- df[1,1] + df[3,3] - df[1,3] - df[3,1]
  denom <- df[1,1] + df[3,3] + df[1,3] + df[3,1] + 
    0.5*(df[1,2] + df[2,1] + df[2,3] + df[3,2])
  s <- num/denom
  return(s)
}

temp2 %>%
  select(ends_with("MR1")) %>%
  count(adult_MR1, child_MR1) %>%
  complete(adult_MR1, child_MR1, fill = list(n = 0)) %>%
  spread(child_MR1, n) %>%
  data.frame() %>%
  remove_rownames() %>%
  column_to_rownames("adult_MR1") %>%
  cattell_s()

temp2 %>%
  select(ends_with("MR2")) %>%
  count(adult_MR2, child_MR2) %>%
  complete(adult_MR2, child_MR2, fill = list(n = 0)) %>%
  spread(child_MR2, n) %>%
  data.frame() %>%
  remove_rownames() %>%
  column_to_rownames("adult_MR2") %>%
  cattell_s()

temp2 %>%
  select(ends_with("MR3")) %>%
  count(adult_MR3, child_MR3) %>%
  complete(adult_MR3, child_MR3, fill = list(n = 0)) %>%
  spread(child_MR3, n) %>%
  data.frame() %>%
  remove_rownames() %>%
  column_to_rownames("adult_MR3") %>%
  cattell_s()

# COEFFICIENT OF CONGRUENCE -----

factor.congruence(temp %>% select(starts_with("adult")),
                  temp %>% select(starts_with("child")))

# SUM OF SQUARED DIFFERENCES -----

temp %>%
  mutate(MR1_sqdiff = (child_MR1 - adult_MR1)^2,
         MR2_sqdiff = (child_MR2 - adult_MR2)^2,
         MR3_sqdiff = (child_MR3 - adult_MR3)^2,
         MR12_sqdiff = (child_MR1 - adult_MR2)^2,
         MR13_sqdiff = (child_MR1 - adult_MR3)^2,
         MR21_sqdiff = (child_MR2 - adult_MR1)^2,
         MR23_sqdiff = (child_MR2 - adult_MR3)^2,
         MR31_sqdiff = (child_MR3 - adult_MR1)^2,
         MR32_sqdiff = (child_MR3 - adult_MR2)^2) %>%
  select(starts_with("MR")) %>%
  gather(factor, sqdiff) %>%
  group_by(factor) %>%
  summarise(SS = sum(sqdiff),
            var_ish = SS/40,
            sd_ish = sqrt(var_ish)) %>%
  arrange(SS)

# DOMINANT ITEMS -----
temp %>%
  select(starts_with("adult")) %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  group_by(capacity) %>%
  mutate(loading_abs = abs(loading)) %>%
  top_n(1, loading_abs) %>%
  full_join(temp %>%
              select(starts_with("child")) %>%
              rownames_to_column("capacity") %>%
              gather(factor, loading, -capacity) %>%
              group_by(capacity) %>%
              mutate(loading_abs = abs(loading)) %>%
              top_n(1, loading_abs)) %>%
  select(-loading, -loading_abs) %>%
  mutate(age_group = substr(factor, 1, 5),
         factor = substr(factor, 7, 10)) %>%
  spread(age_group, factor) %>%
  mutate(match = (adult == child)) %>%
  ungroup() %>%
  count(adult, match)

temp %>%
  select(starts_with("adult")) %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  group_by(capacity) %>%
  mutate(loading_abs = abs(loading)) %>%
  top_n(1, loading_abs) %>%
  full_join(temp %>%
              select(starts_with("child")) %>%
              rownames_to_column("capacity") %>%
              gather(factor, loading, -capacity) %>%
              group_by(capacity) %>%
              mutate(loading_abs = abs(loading)) %>%
              top_n(1, loading_abs)) %>%
  select(-loading, -loading_abs) %>%
  mutate(age_group = substr(factor, 1, 5),
         factor = substr(factor, 7, 10)) %>%
  spread(age_group, factor) %>%
  mutate(match = (adult == child)) %>%
  ungroup() %>%
  filter(!match) %>%
  arrange(adult, child)

# STRONG ITEMS -----

# make dataframe of loadings >=.60
temp %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  filter(abs(loading) >= 0.6) %>%
  spread(factor, loading) %>%
  arrange(desc(abs(adult_MR1)), desc(abs(adult_MR2)))

temp %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  filter(abs(loading) >= 0.6) %>%
  spread(factor, loading) %>%
  arrange(desc(abs(adult_MR1)), desc(abs(adult_MR2))) %>%
  mutate(MR1_common = ifelse(is.na(adult_MR1) | is.na(child_MR1), F, T),
         MR2_common = ifelse(is.na(adult_MR2) | is.na(child_MR2), F, T))

# make dataframe of loadings >=.40
temp3 <- temp %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  filter(abs(loading) >= 0.4) %>%
  spread(factor, loading) %>%
  mutate(adult_MR1_cat = ifelse(is.na(adult_MR1), F, T),
         adult_MR2_cat = ifelse(is.na(adult_MR2), F, T),
         adult_MR3_cat = ifelse(is.na(adult_MR3), F, T),
         child_MR1_cat = ifelse(is.na(child_MR1), F, T),
         child_MR2_cat = ifelse(is.na(child_MR2), F, T),
         child_MR3_cat = ifelse(is.na(child_MR3), F, T)) %>%
  mutate(MR1_common = ifelse(is.na(adult_MR1) | is.na(child_MR1), F, T),
         MR2_common = ifelse(is.na(adult_MR2) | is.na(child_MR2), F, T),
         MR3_common = ifelse(is.na(adult_MR3) | is.na(child_MR3), F, T)) %>%
  arrange(desc(abs(adult_MR1)), desc(abs(adult_MR2)), desc(abs(adult_MR3)))
  
# HEART
temp3 %>%
  filter(adult_MR1_cat | child_MR1_cat) %>%
  arrange(desc(adult_MR1), desc(child_MR1)) %>%
  select(capacity, adult_MR1, child_MR1, MR1_common)
  # count(MR1_common, adult_MR1_cat, child_MR1_cat)

# BODY
temp3 %>%
  filter(adult_MR2_cat | child_MR2_cat) %>%
  arrange(desc(adult_MR2), desc(child_MR2)) %>%
  select(capacity, adult_MR2, child_MR2, MR2_common)
# count(MR2_common, adult_MR2_cat, child_MR2_cat)

# MIND
temp3 %>%
  filter(adult_MR3_cat | child_MR3_cat) %>%
  arrange(desc(adult_MR3), desc(child_MR3)) %>%
  select(capacity, adult_MR3, child_MR3, MR3_common)
# count(MR3_common, adult_MR3_cat, child_MR3_cat)

# CFA -----

temp_loadings <- efa_d1_rotatedN_loadings %>% column_to_rownames("capacity")

# adult_model <- '
# 
# adult_MR1 =~ temp_loadings["angry","MR1"]*angry + temp_loadings["beliefs","MR1"]*beliefs + temp_loadings["calm","MR1"]*calm + temp_loadings["choices","MR1"]*choices + temp_loadings["communicating","MR1"]*communicating + temp_loadings["computations","MR1"]*computations + temp_loadings["conscious","MR1"]*conscious + temp_loadings["depressed","MR1"]*depressed + temp_loadings["depth","MR1"]*depth + temp_loadings["desires","MR1"]*desires + temp_loadings["disrespected","MR1"]*disrespected + temp_loadings["embarrassed","MR1"]*embarrassed + temp_loadings["emo_recog","MR1"]*emo_recog + temp_loadings["fear","MR1"]*fear + temp_loadings["free_will","MR1"]*free_will + temp_loadings["goal","MR1"]*goal + temp_loadings["guilt","MR1"]*guilt + temp_loadings["happy","MR1"]*happy + temp_loadings["hungry","MR1"]*hungry + temp_loadings["intentions","MR1"]*intentions + temp_loadings["joy","MR1"]*joy + temp_loadings["love","MR1"]*love + temp_loadings["morality","MR1"]*morality + temp_loadings["nauseated","MR1"]*nauseated + temp_loadings["odors","MR1"]*odors + temp_loadings["pain","MR1"]*pain + temp_loadings["personality","MR1"]*personality + temp_loadings["pleasure","MR1"]*pleasure + temp_loadings["pride","MR1"]*pride + temp_loadings["reasoning","MR1"]*reasoning + temp_loadings["recognizing","MR1"]*recognizing + temp_loadings["remembering","MR1"]*remembering + temp_loadings["safe","MR1"]*safe + temp_loadings["seeing","MR1"]*seeing + temp_loadings["self_aware","MR1"]*self_aware + temp_loadings["self_restraint","MR1"]*self_restraint + temp_loadings["sounds","MR1"]*sounds + temp_loadings["temperature","MR1"]*temperature + temp_loadings["thoughts","MR1"]*thoughts + temp_loadings["tired","MR1"]*tired
# 
# adult_MR2 =~ temp_loadings["angry","MR2"]*angry + temp_loadings["beliefs","MR2"]*beliefs + temp_loadings["calm","MR2"]*calm + temp_loadings["choices","MR2"]*choices + temp_loadings["communicating","MR2"]*communicating + temp_loadings["computations","MR2"]*computations + temp_loadings["conscious","MR2"]*conscious + temp_loadings["depressed","MR2"]*depressed + temp_loadings["depth","MR2"]*depth + temp_loadings["desires","MR2"]*desires + temp_loadings["disrespected","MR2"]*disrespected + temp_loadings["embarrassed","MR2"]*embarrassed + temp_loadings["emo_recog","MR2"]*emo_recog + temp_loadings["fear","MR2"]*fear + temp_loadings["free_will","MR2"]*free_will + temp_loadings["goal","MR2"]*goal + temp_loadings["guilt","MR2"]*guilt + temp_loadings["happy","MR2"]*happy + temp_loadings["hungry","MR2"]*hungry + temp_loadings["intentions","MR2"]*intentions + temp_loadings["joy","MR2"]*joy + temp_loadings["love","MR2"]*love + temp_loadings["morality","MR2"]*morality + temp_loadings["nauseated","MR2"]*nauseated + temp_loadings["odors","MR2"]*odors + temp_loadings["pain","MR2"]*pain + temp_loadings["personality","MR2"]*personality + temp_loadings["pleasure","MR2"]*pleasure + temp_loadings["pride","MR2"]*pride + temp_loadings["reasoning","MR2"]*reasoning + temp_loadings["recognizing","MR2"]*recognizing + temp_loadings["remembering","MR2"]*remembering + temp_loadings["safe","MR2"]*safe + temp_loadings["seeing","MR2"]*seeing + temp_loadings["self_aware","MR2"]*self_aware + temp_loadings["self_restraint","MR2"]*self_restraint + temp_loadings["sounds","MR2"]*sounds + temp_loadings["temperature","MR2"]*temperature + temp_loadings["thoughts","MR2"]*thoughts + temp_loadings["tired","MR2"]*tired
# 
# adult_MR3 =~ temp_loadings["angry","MR3"]*angry + temp_loadings["beliefs","MR3"]*beliefs + temp_loadings["calm","MR3"]*calm + temp_loadings["choices","MR3"]*choices + temp_loadings["communicating","MR3"]*communicating + temp_loadings["computations","MR3"]*computations + temp_loadings["conscious","MR3"]*conscious + temp_loadings["depressed","MR3"]*depressed + temp_loadings["depth","MR3"]*depth + temp_loadings["desires","MR3"]*desires + temp_loadings["disrespected","MR3"]*disrespected + temp_loadings["embarrassed","MR3"]*embarrassed + temp_loadings["emo_recog","MR3"]*emo_recog + temp_loadings["fear","MR3"]*fear + temp_loadings["free_will","MR3"]*free_will + temp_loadings["goal","MR3"]*goal + temp_loadings["guilt","MR3"]*guilt + temp_loadings["happy","MR3"]*happy + temp_loadings["hungry","MR3"]*hungry + temp_loadings["intentions","MR3"]*intentions + temp_loadings["joy","MR3"]*joy + temp_loadings["love","MR3"]*love + temp_loadings["morality","MR3"]*morality + temp_loadings["nauseated","MR3"]*nauseated + temp_loadings["odors","MR3"]*odors + temp_loadings["pain","MR3"]*pain + temp_loadings["personality","MR3"]*personality + temp_loadings["pleasure","MR3"]*pleasure + temp_loadings["pride","MR3"]*pride + temp_loadings["reasoning","MR3"]*reasoning + temp_loadings["recognizing","MR3"]*recognizing + temp_loadings["remembering","MR3"]*remembering + temp_loadings["safe","MR3"]*safe + temp_loadings["seeing","MR3"]*seeing + temp_loadings["self_aware","MR3"]*self_aware + temp_loadings["self_restraint","MR3"]*self_restraint + temp_loadings["sounds","MR3"]*sounds + temp_loadings["temperature","MR3"]*temperature + temp_loadings["thoughts","MR3"]*thoughts + temp_loadings["tired","MR3"]*tired 
# 
# '

# use top 4 non-negative dominant items from adult model
adult_model <- '

adult_MR1 =~ pride + joy + depressed + happy # + love + guilt + disrespected + embarrassed

adult_MR2 =~ hungry + pain + tired + fear # + odors + safe + nauseated + desires

adult_MR3 =~ depth + reasoning + seeing + choices # + recognizing + remembering + temperature + conscious

'

# fit adult data
fit1 <- cfa(adult_model, 
            data = d1_all)
summary(fit1, fit.measures = T)

# fit child data
fit2 <- cfa(adult_model, 
            data = d2_all)
summary(fit2, fit.measures = T)


# use dominant loadings in adult model
adult_model <- '

adult_MR1 =~ pride + joy + depressed + happy + love + guilt + disrespected + embarrassed + beliefs + personality + angry + morality + emo_recog + self_restraint

adult_MR2 =~ hungry + -1*computations + pain + tired + fear + odors + safe + nauseated + desires + calm + pleasure

adult_MR3 =~ depth + reasoning + seeing + choices + recognizing + remembering + temperature + conscious + communicating + sounds + free_will + intentions + goal + self_aware + thoughts

'

# fit adult data
fit1 <- cfa(adult_model, 
            data = d1_all)
summary(fit1, fit.measures = T)

# fit child data
fit2 <- cfa(adult_model, 
            data = d2_all)
summary(fit2, fit.measures = T)


# use loadings > .6 in adult model
adult_model <- '

adult_MR1 =~ pride + joy + depressed + happy + love + guilt + disrespected + embarrassed

adult_MR2 =~ hungry + pain + fear + tired + safe + nauseated + -1*computations

adult_MR3 =~ depth

'

# fit adult data
fit1 <- cfa(adult_model, 
            data = d1_all)
summary(fit1, fit.measures = T)

# fit child data
fit2 <- cfa(adult_model, 
            data = d2_all)
summary(fit2, fit.measures = T)


# use loadings > .4 in adult model
adult_model <- '

adult_MR1 =~ pride + joy + depressed + happy + love + guilt + disrespected + embarrassed + beliefs + personality + angry + pleasure + morality + emo_recog + self_restraint + calm

adult_MR2 =~ hungry + pain + fear + tired + safe + nauseated + odors + pleasure + calm + desires + -1*recognizing + -1*computations

adult_MR3 =~ reasoning + depth + choices + recognizing + remembering + seeing + communicating + temperature + conscious + sounds + free_will + intentions + goal + self_aware

'

# fit adult data
fit1 <- cfa(adult_model, 
           data = d1_all)
summary(fit1, fit.measures = T)

# fit child data
fit2 <- cfa(adult_model, 
           data = d2_all)
summary(fit2, fit.measures = T)

# use loadings > .4 in both models
shared_model <- '

shared_MR1 =~ pride + joy + depressed + happy + love + guilt + disrespected + embarrassed + beliefs + personality + angry + pleasure + morality + emo_recog + calm

shared_MR2 =~ hungry + pain + fear + tired + nauseated + odors + -1*computations

shared_MR3 =~ reasoning + depth + remembering + temperature + conscious + self_aware

'

shared_model_full <- '

shared_MR1 =~ angry + beliefs + calm + choices + communicating + computations + conscious + depressed + depth + desires + disrespected + embarrassed + emo_recog + fear + free_will + goal + guilt + happy + hungry + intentions + joy + love + morality + nauseated + odors + pain + personality + pleasure + pride + reasoning + recognizing + remembering + safe + seeing + self_aware + self_restraint + sounds + temperature + thoughts + tired

shared_MR2 =~ angry + beliefs + calm + choices + communicating + computations + conscious + depressed + depth + desires + disrespected + embarrassed + emo_recog + fear + free_will + goal + guilt + happy + hungry + intentions + joy + love + morality + nauseated + odors + pain + personality + pleasure + pride + reasoning + recognizing + remembering + safe + seeing + self_aware + self_restraint + sounds + temperature + thoughts + tired

shared_MR3 =~ angry + beliefs + calm + choices + communicating + computations + conscious + depressed + depth + desires + disrespected + embarrassed + emo_recog + fear + free_will + goal + guilt + happy + hungry + intentions + joy + love + morality + nauseated + odors + pain + personality + pleasure + pride + reasoning + recognizing + remembering + safe + seeing + self_aware + self_restraint + sounds + temperature + thoughts + tired

'

# fit all data by group
d12_all_temp <- d12_all %>%
  rownames_to_column("subid") %>%
  mutate(age_group = ifelse(grepl("us_run", subid), "adult", "child")) %>%
  column_to_rownames("subid")

fit3 <- cfa(shared_model,
            ordered = c("angry", "beliefs", "calm", "choices", "communicating", "computations", "conscious", "depressed", "depth", "desires", "disrespected", "embarrassed", "emo_recog", "fear", "free_will", "goal", "guilt", "happy", "hungry", "intentions", "joy", "love", "morality", "nauseated", "odors", "pain", "personality", "pleasure", "pride", "reasoning", "recognizing", "remembering", "safe", "seeing", "self_aware", "self_restraint", "sounds", "temperature", "thoughts", "tired"),
            data = d12_all_temp,
            group = "age_group")
summary(fit3, fit.measures = T)

# fit4 <- cfa(shared_model_full,
#             ordered = c("angry", "beliefs", "calm", "choices", "communicating", "computations", "conscious", "depressed", "depth", "desires", "disrespected", "embarrassed", "emo_recog", "fear", "free_will", "goal", "guilt", "happy", "hungry", "intentions", "joy", "love", "morality", "nauseated", "odors", "pain", "personality", "pleasure", "pride", "reasoning", "recognizing", "remembering", "safe", "seeing", "self_aware", "self_restraint", "sounds", "temperature", "thoughts", "tired"),
#             data = d12_all_temp,
#             group = "age_group")
# summary(fit4, fit.measures = T)

measurementInvariance(shared_model, 
                      # ordered = c("angry", "beliefs", "calm", "choices", "communicating", "computations", "conscious", "depressed", "depth", "desires", "disrespected", "embarrassed", "emo_recog", "fear", "free_will", "goal", "guilt", "happy", "hungry", "intentions", "joy", "love", "morality", "nauseated", "odors", "pain", "personality", "pleasure", "pride", "reasoning", "recognizing", "remembering", "safe", "seeing", "self_aware", "self_restraint", "sounds", "temperature", "thoughts", "tired"),
                      data = d12_all_temp, 
                      group = "age_group")

measurementInvariance(shared_model_full, 
                      # ordered = c("angry", "beliefs", "calm", "choices", "communicating", "computations", "conscious", "depressed", "depth", "desires", "disrespected", "embarrassed", "emo_recog", "fear", "free_will", "goal", "guilt", "happy", "hungry", "intentions", "joy", "love", "morality", "nauseated", "odors", "pain", "personality", "pleasure", "pride", "reasoning", "recognizing", "remembering", "safe", "seeing", "self_aware", "self_restraint", "sounds", "temperature", "thoughts", "tired"),
                      data = d12_all_temp, 
                      group = "age_group")

measurementInvariance(adult_model, data = d12_all_temp, group = "age_group")
