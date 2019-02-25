# STUDY 4: ADULTS
# read in & tidy data
d4_ad <- read.csv("./anonymized_data/study4_adults_anonymized.csv")[-1] %>%
  mutate(study = "Study 4: Adults",
         age_group = "adults",
         character = as.character(character)) %>%
  filter(grepl("beetle", character) | grepl("robot", character)) %>%
  mutate(character = case_when(grepl("beetle", character) ~ "beetle",
                               grepl("robot", character) ~ "robot",
                               TRUE ~ NA_character_),
         subid_char = gsub("beetles", "beetle", subid_char),
         subid_char = gsub("robots", "robot", subid_char)) %>%
  mutate_at(vars(ethnicity, religion),
            funs(cat = case_when(grepl("\\,", as.character(.)) ~ "multi",
                                 TRUE ~ as.character(.)))) %>%
  mutate(age = 2018 - as.numeric(as.character(yob)),
         age = ifelse(age > 100, NA, age)) %>%
  mutate(capacity = gsub("\\.", " ", capacity),
         response_num = recode(response,
                               "NO" = 0,
                               "KINDA" = 0.5,
                               "YES" = 1,
                               .default = NA_real_),
         duration = duration/60) %>%
  select(-summary, -comments) %>%
  filter(!is.na(summary_coded)) %>%
  distinct()

# d4_ad %>% distinct(subid) %>% count()

# make wideform
d4_ad_wide <- d4_ad %>% 
  # mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, summary_coded, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  # clean data (part 1)
  filter(summary_coded == 1) %>%
  filter(`please choose yes` == 1,
         `please click kinda` == 0.5,
         `please select no` == 0) %>%
  select(-summary_coded, -starts_with("please")) %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char))
  
# d4_ad_wide %>% distinct(subid) %>% count()

# clean data (part 2): exclude subs who failed attn checks in EITHER block
d4_ad_wide <- d4_ad_wide %>%
  filter(!subid %in% data.frame(
    d4_ad_wide %>%
      distinct(subid, character) %>%
      count(subid) %>%
      filter(n!=2))$subid)

# d4_ad_wide %>% distinct(subid) %>% count()

d4_ad_wide <- d4_ad_wide %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")

# clean data (part 3)
d4_ad <- d4_ad %>%
  filter(subid_char %in% rownames(d4_ad_wide),
         !grepl("please", capacity))

# d4_ad %>% distinct(subid) %>% count()

# impute missing values using the mean by character and capacity
d4_ad_wide_i <- d4_ad_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")
