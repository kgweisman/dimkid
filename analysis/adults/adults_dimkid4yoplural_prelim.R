library(tidyverse)
library(psych)

d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/QUALTRICS/dimkid4yo plural/Dimkid (adults_dimkid4yo) - plural_handcoded.csv", header = T) %>%
  mutate(ResponseId = gsub("_", "-", ResponseId))

question_key <- d_raw[1:2,] %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("question") %>%
  rename(question_text = X1) %>%
  mutate(question_text = gsub(" - .*$", "", question_text),
         question_text = gsub("Do you think \\[Field-char_singular\\] ", "",
                              question_text),
         question_text = gsub("metal\\?", "metal", question_text),
         question_text = gsub("off\\?", "off", question_text))

char_key <- d_raw %>%
  select(ResponseId, starts_with("char")) %>%
  filter(!grepl("\\{", ResponseId), !grepl("Response", ResponseId)) %>%
  gather(char_num, char, starts_with("char"))

cap_key <- d_raw %>%
  select(ResponseId, starts_with("cap"), -cap_order_b1) %>%
  filter(!grepl("\\{", ResponseId), !grepl("Response", ResponseId)) %>%
  gather(cap_num, capacity, starts_with("cap"))

demo <- d_raw[-1,] %>%
  filter(!grepl("\\{", StartDate)) %>%
  select(ResponseId, Duration..in.seconds., starts_with("Q")) %>%
  gather(question, response, starts_with("Q")) %>%
  left_join(question_key %>% distinct(question, question_text)) %>%
  mutate(question_text = case_when(
    question == "Q57" ~ "summary",
    question == "Q57_coded" ~ "summary_coded",
    question == "Q58" ~ "yob",
    question == "Q59" ~ "gender",
    question == "Q60" ~ "ethnicity",
    question == "Q61" ~ "religion",
    question == "Q62" ~ "comments",
    TRUE ~ question_text)) %>%
  select(-question) %>%
  spread(question_text, response) %>%
  rename(subid = ResponseId, 
         duration = Duration..in.seconds.)

data <- d_raw[-1,] %>%
  filter(!grepl("\\{", StartDate)) %>%
  select(ResponseId, starts_with("X")) %>%
  gather(question, response, c(starts_with("X"))) %>%
  left_join(question_key %>% distinct(question, question_text)) %>%
  mutate(cap_num = case_when(grepl("please", question_text) ~ 
                               gsub(" - .*$", "", question_text),
                             TRUE ~ 
                               gsub("^.*cap", "cap", 
                                    gsub("\\}", "", question_text)))) %>%
  mutate(char_num = case_when(grepl("131", question) ~ "char1",
                              grepl("155", question) ~ "char2")) %>%
  left_join(cap_key) %>%
  mutate(capacity = case_when(!is.na(capacity) ~ capacity,
                              TRUE ~ cap_num)) %>%
  left_join(char_key) %>%
  select(-c(starts_with("quest"), cap_num, char_num)) %>%
  rename(subid = ResponseId, 
         character = char) %>%
  filter(!is.na(subid), !is.na(character), !is.na(capacity)) %>%
  mutate(subid_char = paste(subid, character, sep = "_"))


d0 <- demo %>%
  full_join(data) %>%
  select(subid, duration, 
         summary, summary_coded, yob, gender, ethnicity, religion, comments,
         subid_char, character, capacity, response) %>%
  mutate(response_num = recode(response,
                               "NO" = 0,
                               "KINDA" = 0.5,
                               "YES" = 1,
                               .default = as.numeric(NA)))

d <- d0

write.csv(d, "./anonymized_data/study3_adults_anonymized.csv")
