# STUDY 3: ADULTS
# read in & tidy data
d3_ad <- read.csv("./anonymized_data/study3_adults_anonymized.csv") %>%
  mutate(study = "Study 3: Adults",
         age_group = "adults") %>%
  mutate_at(vars(ethnicity, religion),
            funs(cat = case_when(grepl("\\,", as.character(.)) ~ "multi",
                                 TRUE ~ as.character(.)))) %>%
  mutate(age = 2018 - as.numeric(as.character(yob)),
         age = ifelse(age > 100, NA, age)) %>%
  gather(capacity, response, be.aware.of.things:smell.things) %>%
  mutate(capacity = gsub("\\.", " ", capacity),
         response_num = recode(response,
                               "NO" = 0,
                               "KINDA" = 0.5,
                               "YES" = 1,
                               .default = NA_real_),
         duration = duration/60) %>%
  select(-summary, -comments) %>%
  rename(character = char) %>%
  mutate(capacity = case_when(
           grepl("sick", capacity) ~ "feel sick...",
           grepl("far away", capacity) ~ "sense...far away",
           TRUE ~ capacity),
         character = factor(gsub("_", " ", as.character(character)),
                            levels = c("elephant", "goat", "mouse", "bird", 
                                       "beetle", "teddy bear", "doll",
                                       "robot", "computer"))) %>%
  filter(!grepl("metal", capacity), !grepl("on and off", capacity)) %>%
  distinct()

# make wideform
d3_ad_wide <- d3_ad %>% 
  # mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, summary_coded, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  # clean data (part 1)
  filter(summary_coded == 1,
         `please choose yes` == 1,
         `please click kinda` == 0.5,
         `please select no` == 0) %>%
  select(-summary_coded, -starts_with("please")) %>%
  column_to_rownames("subid_char")

# clean data (part 2)
d3_ad <- d3_ad %>%
  filter(subid_char %in% rownames(d3_ad_wide),
         !grepl("please", capacity))

# impute missing values using the mean by character and capacity
d3_ad_wide_i <- d3_ad_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")

d3_ad_i <- d3_ad_wide_i %>%
  rownames_to_column("subid_char") %>%
  mutate(subid_char = gsub("_elephant", ".elephant", subid_char),
         subid_char = gsub("_goat", ".goat", subid_char),
         subid_char = gsub("_mouse", ".mouse", subid_char),
         subid_char = gsub("_bird", ".bird", subid_char),
         subid_char = gsub("_beetle", ".beetle", subid_char),
         subid_char = gsub("_teddy_bear", ".teddy bear", subid_char),
         subid_char = gsub("_doll", ".doll", subid_char),
         subid_char = gsub("_robot", ".robot", subid_char),
         subid_char = gsub("_computer", ".computer", subid_char)) %>%
  gather(capacity, response_num, -subid_char) %>%
  separate(subid_char, c("subid", "character"), sep = "\\.") %>%
  left_join(d3_ad %>% distinct(study, subid, age_group, age)) %>%
  mutate_at(vars(subid, character), funs(factor))
