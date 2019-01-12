# STUDY 2: YOUNGER CHILDREN
# read in & tidy data
d2_46 <- read.csv("./anonymized_data/study2_children46_anonymized.csv") %>%
  mutate(age = as.numeric(as.character(age))) %>%
  filter(((age >= 4 & age < 7) | is.na(age)),
         # character %in% c("beetle", "robot"),
         !grepl("metal", capWording), 
         !grepl("turned on", capWording)) %>%
  select(subid, age, gender, ethnicity, 
         character, capWording, response, rt, sessionDuration) %>%
  rename(duration = sessionDuration) %>%
  mutate(age_group = "children46") %>%
  mutate(response_num = case_when(
    tolower(response) == "no" ~ 0,
    tolower(response) %in% c("kinda", "kida") ~ 0.5,
    tolower(response) == "yes" ~ 1)) %>%
  mutate(capWording = as.character(trimws(capWording)),
         capacity = case_when(
           # grepl("\\--", capWording) ~ gsub(" \\--.*$", "...", capWording),
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
  mutate(ethnicity = tolower(as.character(ethnicity)),
         ethnicity = gsub("sn", "", ethnicity),
         ethnicity = trimws(ethnicity),
         ethnicity = case_when(
           grepl(" ", ethnicity) |
             (grepl("\\/", ethnicity) & !grepl("hisp", ethnicity)) ~ "multi",
           ethnicity %in% c("a", "chinese", "east asian") ~ "east asian",
           ethnicity %in% c("af", "ethiopian american") ~ "black",
           ethnicity %in% c("c", "cj") ~ "white",
           ethnicity == "h" ~ "hispanic latino",
           ethnicity == "i" ~ "south or southeast asian",
           ethnicity == "me" ~ "middle eastern",
           ethnicity == "na" ~ "native american",
           TRUE ~ ethnicity)) %>%
  distinct()

# clean data
d2_46 <- d2_46 %>%
  filter(rt >= 250 | is.na(rt))

# make wideform
d2_46_wide <- d2_46 %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")

# impute missing values using the mean by character and capacity
d2_46_wide_i <- d2_46_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")

