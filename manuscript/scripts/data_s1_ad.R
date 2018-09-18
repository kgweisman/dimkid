# STUDY 1: ADULTS
# read in & tidy data
d1_ad <- read.csv("../data/adults/us_run-01_2016-06-05_anonymized.csv") %>%
  mutate(study = "Study 1: Adults",
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
           TRUE ~ capWording)) %>%
  distinct()

# clean data
d1_ad <- d1_ad %>%
  filter(rt >= 250 | is.na(rt))
  
# make wideform
d1_ad_wide <- d1_ad %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")
