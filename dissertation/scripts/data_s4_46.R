# STUDY 4: CHILDREN
# read in & tidy data
d4_46 <- read.csv("./anonymized_data/study4_children46_anonymized.csv") %>%
  mutate(age = as.numeric(as.character(age_years)),
         character = as.character(character),
         study = "Study 4: Children, 4-6y") %>%
  filter(((age >= 4 & age < 7) | is.na(age)),
         (grepl("beetle", character) | grepl("robot", character)),
         !grepl("metal", capacity), 
         !grepl("turned on", capacity)) %>%
  mutate(character = case_when(grepl("beetle", character) ~ "beetle",
                               grepl("robot", character) ~ "robot",
                               TRUE ~ NA_character_)) %>%
  select(study, subid, age, gender, ethnicity, ethnicity_collapse,
         game, version, character, question, capacity, 
         response, response_num) %>%
  mutate(age_group = "children46") %>%
  mutate(capacity = as.character(trimws(capacity))) %>%
  mutate(ethnicity = tolower(as.character(ethnicity)),
         ethnicity = gsub("sn", "", ethnicity),
         ethnicity = trimws(ethnicity),
         ethnicity = case_when(
           grepl(" ", ethnicity) |
             grepl("\\/", ethnicity) |
             grepl("multi", ethnicity) |
             grepl("mix", ethnicity) ~ "multi",
           ethnicity == "a" ~ "east asian",
           ethnicity == "af" ~ "black",
           ethnicity %in% c("c", "cj") ~ "white",
           ethnicity == "h" ~ "hispanic latino",
           ethnicity == "i" ~ "south or southeast asian",
           ethnicity == "me" ~ "middle eastern",
           ethnicity == "na" ~ "native american",
           TRUE ~ ethnicity)) %>%
  distinct()

# make wideform
d4_46_wide <- d4_46 %>% 
  mutate(subid_char = paste(subid, character, sep = "_")) %>%
  select(subid_char, capacity, response_num) %>%
  spread(capacity, response_num) %>%
  column_to_rownames("subid_char")

# impute missing values using the mean by character and capacity
d4_46_wide_i <- d4_46_wide %>% 
  rownames_to_column("subid_char") %>%
  mutate(subid = gsub("_.*$", "", subid_char),
         character = gsub("^.*_", "", subid_char)) %>%
  group_by(character) %>%
  mutate_at(vars(-c(subid, character, subid_char)),
            funs(replace(., which(is.na(.)), mean(., na.rm = T)))) %>%
  ungroup() %>%
  select(-subid, -character) %>%
  column_to_rownames("subid_char")

d4_46_i <- d4_46_wide_i %>%
  rownames_to_column("subid_char") %>%
  gather(capacity, response_num, -subid_char) %>%
  separate(subid_char, c("subid", "character"), sep = "_") %>%
  left_join(d4_46 %>% distinct(study, subid, age_group, age)) %>%
  mutate_at(vars(subid, character), funs(factor))
