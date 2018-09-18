# STUDY 1: CHILDREN
# read in & tidy data
d1_79 <- read.csv("../data/children/run-01_2017-07-24_anonymized.csv") %>%
  filter(age >= 7, age < 10,
         character %in% c("beetle", "robot"),
         !grepl("metal", capWording), !grepl("on and off", capWording)) %>%
  select(subid, age, gender, ethnicity, bilingual, languages,
         character, capWording, response, rt, sessionDuration) %>%
  rename(duration = sessionDuration) %>%
  mutate(age_group = "children79") %>%
  mutate(response_num = case_when(
    tolower(response) == "no" ~ 0,
    tolower(response) %in% c("kinda", "kida") ~ 0.5,
    tolower(response) == "yes" ~ 1)) %>%
  mutate(capWording = as.character(capWording),
         capacity = case_when(
           grepl("\\--", capWording) ~ gsub(" \\--.*$", "...", capWording),
           grepl("close by or far away", capWording) ~ "sense...far away",
           grepl("understand how somebody else is feeling", capWording) ~
           "understand how someone...feeling",
           grepl("pleasure", capWording) ~ "feel pleasure...",
           grepl("sick", capWording) ~ "feel sick...",
           grepl("desires", capWording) ~ "have desires...",
           grepl("self-control", capWording) ~ "have self-control...",
           grepl("goals", capWording) ~ "have goals...",
           grepl("personality", capWording) ~ "have a personality...",
           grepl("beliefs", capWording) ~ "have beliefs...",
           TRUE ~ capWording)) %>%
  mutate(ethnicity = as.character(ethnicity),
         ethnicity = case_when(
           grepl("\\;", ethnicity) | 
             grepl("mix", ethnicity) | 
             grepl("\\/", ethnicity) ~ "multi",
           ethnicity == "white, caucasian, or european american" ~ "white",
           ethnicity %in% c("east asian", "other (asian)", 
                            "other: east asian american", 
                            "other: south korean") ~ "east asian",
           TRUE ~ ethnicity)) %>%
  distinct()

# clean data
d1_79 <- d1_79 %>%
  filter(rt >= 250 | is.na(rt))

# make wideform
d1_79_wide <- d1_79 %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")
