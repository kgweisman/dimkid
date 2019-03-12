# STUDY 1a: ADULTS -----
# read in & tidy data
d1a_ad <- read.csv("./anonymized_data/bodyheartmind_study1a.csv") %>%
  mutate(finished_mod = ifelse(is.na(CATCH), 0,
                               ifelse(finished == 1, 1,
                                      0.5))) %>%
  filter(CATCH == 1, # exclude Ps who fail catch trials 
         finished_mod != 0) %>% # exclude Ps who did not complete task
  mutate(yob_correct = as.numeric(
    ifelse(as.numeric(as.character(yob)) > 1900 &
             as.numeric(as.character(yob)) < 2000,
           as.numeric(as.character(yob)), NA)), # correct formatting in yob
    age_approx = 2016 - yob_correct) %>% # calculate approximate age
  filter(age_approx >= 18) %>% # exclude Ps who are younger than 18 years
  mutate(yob_correct = as.numeric(
    ifelse(as.numeric(as.character(yob)) > 1900 &
             as.numeric(as.character(yob)) < 2000,
           as.numeric(as.character(yob)), NA)), # correct formatting in yob
    age_approx = 2016 - yob_correct) %>% # calculate approximate age
  filter(age_approx >= 18) %>% # exclude Ps who are younger than 18 years
  select(subid, condition, happy:pride) %>%
  rename(character = condition) %>%
  mutate(study = "Study 1a: Adults",
         age_group = "adults") %>%
  gather(capacity, response_num, -c(study, subid, age_group, character)) %>%
  mutate(response_num = as.numeric(response_num) + 3) %>%
  mutate(capacity = recode(capacity,
                           "angry" = "getting angry",
                           "beliefs" = "holding beliefs",
                           "calm" = "feeling calm",
                           "choices" = "making choices",
                           "communicating" = "communicating with others",
                           "computations" = "doing computations",
                           "conscious" = "being conscious",
                           "depressed" = "feeling depressed",
                           "depth" = "perceiving depth",
                           "desires" = "having desires",
                           "disrespected" = "feeling disrespected",
                           "embarrassed" = "feeling embarrassed",
                           "emo_recog" = "understanding how others are feeling",
                           "fear" = "experiencing fear",
                           "free_will" = "having free will",
                           "goal" = "working toward a goal",
                           "guilt" = "experiencing guilt",
                           "happy" = "feeling happy",
                           "hungry" = "getting hungry",
                           "intentions" = "having intentions",
                           "joy" = "experiencing joy",
                           "love" = "feeling love",
                           "morality" = "telling right from wrong",
                           "nauseated" = "feeling nauseated",
                           "odors" = "detecting odors",
                           "pain" = "experiencing pain",
                           "personality" = "having a personality",
                           "pleasure" = "experiencing pleasure",
                           "pride" = "experiencing pride",
                           "reasoning" = "reasoning about things",
                           "recognizing" = "recognizing someone",
                           "remembering" = "remembering things",
                           "safe" = "feeling safe",
                           "seeing" = "seeing things",
                           "self_aware" = "being self-aware",
                           "self_restraint" = "exercising self-restraint",
                           "sounds" = "detecting sounds",
                           "temperature" = "sensing temperatures",
                           "thoughts" = "having thoughts",
                           "tired" = "feeling tired")) %>%
  distinct()

# clean data: NA

# make wideform
d1a_ad_wide <- d1a_ad %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")

# impute missing values using the mean by character and capacity
d1a_ad_wide_i <- d1a_ad_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")

# STUDY 1b: ADULTS -----
# read in & tidy data
d1b_ad <- read.csv("./anonymized_data/bodyheartmind_study1b.csv") %>%
  mutate(finished_mod = ifelse(is.na(CATCH), 0,
                               ifelse(finished == 1, 1,
                                      0.5))) %>%
  filter(CATCH == 1, # exclude Ps who fail catch trials 
         finished_mod != 0) %>% # exclude Ps who did not complete task
  mutate(yob_correct = as.numeric(
    ifelse(as.numeric(as.character(yob)) > 1900 &
             as.numeric(as.character(yob)) < 2000,
           as.numeric(as.character(yob)), NA)), # correct formatting in yob
    age_approx = 2016 - yob_correct) %>% # calculate approximate age
  filter(age_approx >= 18) %>% # exclude Ps who are younger than 18 years
  select(subid, condition, happy:pride) %>%
  rename(character = condition) %>%
  mutate(study = "Study 1b: Adults",
         age_group = "adults") %>%
  gather(capacity, response_num, -c(study, subid, age_group, character)) %>%
  mutate(response_num = as.numeric(response_num) + 3) %>%
  mutate(capacity = recode(capacity,
                           "angry" = "getting angry",
                           "beliefs" = "holding beliefs",
                           "calm" = "feeling calm",
                           "choices" = "making choices",
                           "communicating" = "communicating with others",
                           "computations" = "doing computations",
                           "conscious" = "being conscious",
                           "depressed" = "feeling depressed",
                           "depth" = "perceiving depth",
                           "desires" = "having desires",
                           "disrespected" = "feeling disrespected",
                           "embarrassed" = "feeling embarrassed",
                           "emo_recog" = "understanding how others are feeling",
                           "fear" = "experiencing fear",
                           "free_will" = "having free will",
                           "goal" = "working toward a goal",
                           "guilt" = "experiencing guilt",
                           "happy" = "feeling happy",
                           "hungry" = "getting hungry",
                           "intentions" = "having intentions",
                           "joy" = "experiencing joy",
                           "love" = "feeling love",
                           "morality" = "telling right from wrong",
                           "nauseated" = "feeling nauseated",
                           "odors" = "detecting odors",
                           "pain" = "experiencing pain",
                           "personality" = "having a personality",
                           "pleasure" = "experiencing pleasure",
                           "pride" = "experiencing pride",
                           "reasoning" = "reasoning about things",
                           "recognizing" = "recognizing someone",
                           "remembering" = "remembering things",
                           "safe" = "feeling safe",
                           "seeing" = "seeing things",
                           "self_aware" = "being self-aware",
                           "self_restraint" = "exercising self-restraint",
                           "sounds" = "detecting sounds",
                           "temperature" = "sensing temperatures",
                           "thoughts" = "having thoughts",
                           "tired" = "feeling tired")) %>%
  distinct()

# clean data: NA

# make wideform
d1b_ad_wide <- d1b_ad %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")

# impute missing values using the mean by character and capacity
d1b_ad_wide_i <- d1b_ad_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")

# STUDY 1c: ADULTS -----
# read in & tidy data
d1c_ad_0 <- read.csv("./anonymized_data/bodyheartmind_study1c.csv") %>%
  mutate(finished_mod = ifelse((is.na(CATCH..characterL) | 
                                  is.na(CATCH..characterR)), 0,
                               ifelse(finished == 1, 1,
                                      0.5))) %>%
  filter(CATCH..characterL == 5, 
         CATCH..characterR == 5, # exclude Ps who fail catch trials 
         finished_mod != 0) %>% # exclude Ps who did not complete task
  mutate(yob_correct = as.numeric(
    ifelse(as.numeric(as.character(yob)) > 1900 &
             as.numeric(as.character(yob)) < 2000,
           as.numeric(as.character(yob)), NA)), # correct formatting in yob
    age_approx = 2016 - yob_correct) %>% # calculate approximate age
  filter(age_approx >= 18) %>% # exclude Ps who are younger than 18 years
  select(subid, ends_with("characterL"), ends_with("characterR"),
         -starts_with("CATCH")) %>%
  mutate(study = "Study 1c: Adults",
         age_group = "adults")
  
d1c_ad_L <- d1c_ad_0 %>%
  select(study, subid, age_group, ends_with("characterL")) %>%
  rename(character = characterL) %>%
  gather(capacity, response_num, -c(study, subid, age_group, character)) %>%
  mutate(response_num = as.numeric(response_num) - 1) %>% # initially coded as 1-7
  mutate(capacity = recode(gsub("\\.\\.characterL", "", capacity),
                           "angry" = "getting angry",
                           "beliefs" = "holding beliefs",
                           "calm" = "feeling calm",
                           "choices" = "making choices",
                           "communicating" = "communicating with others",
                           "computations" = "doing computations",
                           "conscious" = "being conscious",
                           "depressed" = "feeling depressed",
                           "depth" = "perceiving depth",
                           "desires" = "having desires",
                           "disrespected" = "feeling disrespected",
                           "embarrassed" = "feeling embarrassed",
                           "emo_recog" = "understanding how others are feeling",
                           "fear" = "experiencing fear",
                           "free_will" = "having free will",
                           "goal" = "working toward a goal",
                           "guilt" = "experiencing guilt",
                           "happy" = "feeling happy",
                           "hungry" = "getting hungry",
                           "intentions" = "having intentions",
                           "joy" = "experiencing joy",
                           "love" = "feeling love",
                           "morality" = "telling right from wrong",
                           "nauseated" = "feeling nauseated",
                           "odors" = "detecting odors",
                           "pain" = "experiencing pain",
                           "personality" = "having a personality",
                           "pleasure" = "experiencing pleasure",
                           "pride" = "experiencing pride",
                           "reasoning" = "reasoning about things",
                           "recognizing" = "recognizing someone",
                           "remembering" = "remembering things",
                           "safe" = "feeling safe",
                           "seeing" = "seeing things",
                           "self_aware" = "being self-aware",
                           "self_restraint" = "exercising self-restraint",
                           "sounds" = "detecting sounds",
                           "temperature" = "sensing temperatures",
                           "thoughts" = "having thoughts",
                           "tired" = "feeling tired")) %>%
  distinct()

d1c_ad_R <- d1c_ad_0 %>%
  select(study, subid, age_group, ends_with("characterR")) %>%
  rename(character = characterR) %>%
  gather(capacity, response_num, -c(study, subid, age_group, character)) %>%
  mutate(response_num = as.numeric(response_num) - 1) %>% # initially coded as 1-7
  mutate(capacity = recode(gsub("\\.\\.characterR", "", capacity),
                           "angry" = "getting angry",
                           "beliefs" = "holding beliefs",
                           "calm" = "feeling calm",
                           "choices" = "making choices",
                           "communicating" = "communicating with others",
                           "computations" = "doing computations",
                           "conscious" = "being conscious",
                           "depressed" = "feeling depressed",
                           "depth" = "perceiving depth",
                           "desires" = "having desires",
                           "disrespected" = "feeling disrespected",
                           "embarrassed" = "feeling embarrassed",
                           "emo_recog" = "understanding how others are feeling",
                           "fear" = "experiencing fear",
                           "free_will" = "having free will",
                           "goal" = "working toward a goal",
                           "guilt" = "experiencing guilt",
                           "happy" = "feeling happy",
                           "hungry" = "getting hungry",
                           "intentions" = "having intentions",
                           "joy" = "experiencing joy",
                           "love" = "feeling love",
                           "morality" = "telling right from wrong",
                           "nauseated" = "feeling nauseated",
                           "odors" = "detecting odors",
                           "pain" = "experiencing pain",
                           "personality" = "having a personality",
                           "pleasure" = "experiencing pleasure",
                           "pride" = "experiencing pride",
                           "reasoning" = "reasoning about things",
                           "recognizing" = "recognizing someone",
                           "remembering" = "remembering things",
                           "safe" = "feeling safe",
                           "seeing" = "seeing things",
                           "self_aware" = "being self-aware",
                           "self_restraint" = "exercising self-restraint",
                           "sounds" = "detecting sounds",
                           "temperature" = "sensing temperatures",
                           "thoughts" = "having thoughts",
                           "tired" = "feeling tired")) %>%
  distinct()

d1c_ad <- full_join(d1c_ad_L, d1c_ad_R)
rm(d1c_ad_0, d1c_ad_L, d1c_ad_R)

# clean data: NA

# make wideform
d1c_ad_wide <- d1c_ad %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")

# impute missing values using the mean by character and capacity
d1c_ad_wide_i <- d1c_ad_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")

# STUDY 1d: ADULTS -----
# read in & tidy data
d1d_ad <- read.csv("./anonymized_data/bodyheartmind_study1d.csv") %>%
  mutate(finished_mod = ifelse(is.na(CATCH), 0,
                               ifelse(finished == 1, 1,
                                      0.5))) %>%
  filter(CATCH == 1, # exclude Ps who fail catch trials 
         finished_mod != 0) %>% # exclude Ps who did not complete task
  mutate(yob_correct = as.numeric(
    ifelse(as.numeric(as.character(yob)) > 1900 &
             as.numeric(as.character(yob)) < 2000,
           as.numeric(as.character(yob)), NA)), # correct formatting in yob
    age_approx = 2016 - yob_correct) %>% # calculate approximate age
  filter(age_approx >= 18) %>% # exclude Ps who are younger than 18 years
  select(subid, condition, happy:pride) %>%
  rename(character = condition) %>%
  mutate(study = "Study 1d: Adults",
         age_group = "adults") %>%
  gather(capacity, response_num, -c(study, subid, age_group, character)) %>%
  mutate(response_num = as.numeric(response_num) + 3) %>%
  mutate(capacity = recode(capacity,
                           "angry" = "getting angry",
                           "beliefs" = "holding beliefs",
                           "calm" = "feeling calm",
                           "choices" = "making choices",
                           "communicating" = "communicating with others",
                           "computations" = "doing computations",
                           "conscious" = "being conscious",
                           "depressed" = "feeling depressed",
                           "depth" = "perceiving depth",
                           "desires" = "having desires",
                           "disrespected" = "feeling disrespected",
                           "embarrassed" = "feeling embarrassed",
                           "emo_recog" = "understanding how others are feeling",
                           "fear" = "experiencing fear",
                           "free_will" = "having free will",
                           "goal" = "working toward a goal",
                           "guilt" = "experiencing guilt",
                           "happy" = "feeling happy",
                           "hungry" = "getting hungry",
                           "intentions" = "having intentions",
                           "joy" = "experiencing joy",
                           "love" = "feeling love",
                           "morality" = "telling right from wrong",
                           "nauseated" = "feeling nauseated",
                           "odors" = "detecting odors",
                           "pain" = "experiencing pain",
                           "personality" = "having a personality",
                           "pleasure" = "experiencing pleasure",
                           "pride" = "experiencing pride",
                           "reasoning" = "reasoning about things",
                           "recognizing" = "recognizing someone",
                           "remembering" = "remembering things",
                           "safe" = "feeling safe",
                           "seeing" = "seeing things",
                           "self_aware" = "being self-aware",
                           "self_restraint" = "exercising self-restraint",
                           "sounds" = "detecting sounds",
                           "temperature" = "sensing temperatures",
                           "thoughts" = "having thoughts",
                           "tired" = "feeling tired"),
         character = factor(gsub("_", " ", 
                                 gsub("persistant", "persistent",
                                      as.character(character))),
                            levels = c("adult", "child", "infant", 
                                       "person in a persistent vegetative state",
                                       "fetus", "chimpanzee", "elephant", 
                                       "dolphin", "bear", "dog", "goat", "mouse",
                                       "frog", "blue jay", "fish", "beetle",
                                       "microbe", "robot", "computer", 
                                       "car", "stapler"))) %>%
  distinct()

# clean data: NA

# make wideform
d1d_ad_wide <- d1d_ad %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")

# impute missing values using the mean by character and capacity
d1d_ad_wide_i <- d1d_ad_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")
