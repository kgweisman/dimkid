# STUDY 2: ADULTS
# read in & tidy data
d2_ad <- read.csv("./anonymized_data/study2_adults_anonymized.csv") %>%
  mutate(study = "Study 2: Adults",
         age_group = "adults") %>%
  mutate_at(vars(ethnicity, religionChild, religionNow), 
            funs(gsub(" +$", "", .))) %>%
  mutate_at(vars(ethnicity, religionChild, religionNow),
            funs(cat = case_when(grepl(" ", as.character(.)) ~ "multi",
                                 TRUE ~ .))) %>%
  select(study, subid:country_selfrep, age_group, ends_with("_cat")) %>%
  rename(character = charName,
         cap_abbrev = capacity,
         response_num = responseNum) %>%
  mutate(capWording = as.character(capWording),
         capacity = case_when(
           grepl("--", capWording) ~ gsub(" --.*$", "...", capWording),
           grepl("close by or far away", capWording) ~ "sense...far away",
           grepl("understand how somebody else is feeling", capWording) ~
             "understand how someone...feeling",
           TRUE ~ capWording),
         character = factor(character)) %>%
  distinct()

# clean data
d2_ad <- d2_ad %>%
  filter(rt >= 250 | is.na(rt))
  
# make wideform
d2_ad_wide <- d2_ad %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")

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
