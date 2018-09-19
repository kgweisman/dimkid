# STUDY 2: ADULTS
# read in & tidy data
d2_ad <- read.csv("./anonymized_data/study2_adults_anonymized.csv", row.names = "X") %>%
  mutate(study = "Study 2: Adults",
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
           grepl("--", capacity) ~ gsub(" --.*$", "...", capacity),
           grepl("close by or far away", capacity) ~ "sense...far away",
           grepl("understand how somebody else is feeling", capacity) ~
             "understand how someone...feeling",
           TRUE ~ capacity)) %>%
  filter(!grepl("metal", capacity), !grepl("on and off", capacity)) %>%
  distinct()

# make wideform
d2_ad_wide <- d2_ad %>% 
  # mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, summary_coded, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  # clean data (part 1)
  filter(summary_coded == 1,
         `please choose yes` == 1,
         `please click kinda` == 0.5,
         `please select no` == 0) %>%
  select(-summary_coded) %>%
  column_to_rownames("subid_char")

# clean data (part 2)
d2_ad <- d2_ad %>%
  filter(subid_char %in% rownames(d2_ad_wide))

# impute missing values using the mean by character and capacity
d2_ad_wide_i <- d2_ad_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")
