# STUDY 3: OLDER CHILDREN
# read in & tidy data
d3_79 <- read.csv("./anonymized_data/study3_children79_anonymized.csv") %>%
  mutate(age = as.numeric(as.character(age))) %>%
  filter(((age >= 7 & age < 10) | is.na(age)),
         # character %in% c("beetle", "robot"),
         !grepl("metal", capWording), !grepl("on and off", capWording)) %>%
  select(subid, age, gender, ethnicity, 
         character, capWording, response, rt, sessionDuration) %>%
  rename(duration = sessionDuration) %>%
  mutate(study = "Study 3: Older children",
         age_group = "children79") %>%
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
           TRUE ~ capWording),
         character = factor(gsub("_", " ", as.character(character)),
                            levels = c("elephant", "goat", "mouse", "bird", 
                                       "beetle", "teddy bear", "doll",
                                       "robot", "computer"))) %>%
  mutate(ethnicity = tolower(as.character(ethnicity)),
         ethnicity = case_when(
           grepl("\\;", ethnicity) | 
             grepl("\\&", ethnicity) |
             grepl("mix", ethnicity) | 
             grepl("half", ethnicity) | 
             (grepl("\\/", ethnicity) & !grepl("hisp", ethnicity)) |
             grepl("\\, black", ethnicity) |
             grepl("\\, white", ethnicity) |
             grepl("\\, east", ethnicity) | 
             grepl("\\, south", ethnicity) |
             grepl("\\, philippies", ethnicity) ~ "multi",
           ethnicity == "white, caucasian, or european american" ~ "white",
           ethnicity %in% c("east asian", "other (asian)", 
                            "other: east asian american", 
                            "other: south korean") ~ "east asian",
           ethnicity %in% c("south or southeast asian",
                            "south our southeast asian",
                            "south or southeast asisan",
                            "indian") ~ 
             "south or southeast asian",
           TRUE ~ ethnicity)) %>%
  distinct()

# clean data
d3_79 <- d3_79 %>%
  filter(rt >= 250 | is.na(rt))

# make wideform
d3_79_wide <- d3_79 %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")

# impute missing values using the mean by character and capacity
d3_79_wide_i <- d3_79_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")

d3_79_i <- d3_79_wide_i %>%
  rownames_to_column("subid_char") %>%
  gather(capacity, response_num, -subid_char) %>%
  separate(subid_char, c("subid", "character"), sep = "_") %>%
  left_join(d3_79 %>% distinct(study, subid, age_group, age)) %>%
  mutate_at(vars(subid, character), funs(factor))
