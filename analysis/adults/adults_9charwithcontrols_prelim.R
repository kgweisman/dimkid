library(tidyverse)
library(psych)

d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/QUALTRICS/9 characters/Dimkid (adults_many_char with controls)_handcoded.csv", header = T) %>%
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

d0 <- d_raw[-1,] %>%
  filter(!grepl("\\{", StartDate)) %>%
  select(ResponseId, Duration..in.seconds.,
         char, starts_with("X"), starts_with("Q")) %>%
  gather(question, response, c(starts_with("X"), starts_with("Q"))) %>%
  left_join(question_key %>% distinct(question, question_text)) %>%
  mutate(question_text = case_when(
    question == "Q44" ~ "summary",
    question == "Q44_coded" ~ "summary_coded",
    question == "Q45" ~ "yob",
    question == "Q46" ~ "gender",
    question == "Q47" ~ "ethnicity",
    question == "Q48" ~ "religion",
    question == "Q49" ~ "comments",
    TRUE ~ question_text)) %>%
  rename(subid = ResponseId,
         duration = Duration..in.seconds.) %>%
  mutate(subid_char = paste(subid, char, sep = "_")) %>%
  select(-question) %>%
  spread(question_text, response) %>%
  select(subid, char, subid_char, duration,
         summary, summary_coded, yob, gender, ethnicity, religion, comments,
         contains(" "))

d <- d0

write.csv(d, "./anonymized_data/study2_adults_anonymized.csv")