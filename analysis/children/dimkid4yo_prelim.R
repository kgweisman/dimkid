# PRELIMINARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# READ IN DATA ----------------------------------------------------------------

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-05_2018-08-07_anonymized.csv")[-1]

# READ IN QUESTION KEY --------------------------------------------------------

question_key <- read_excel("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/design/dimkid4yo (spring 2018)/dimkid4yo versions SAVE (4yo version spring 2018).xlsx") %>%
  select(`Question:`, `Clarification (opt.):`, starts_with("v")) %>%
  rename(question_text = `Question:`,
         question_clar = `Clarification (opt.):`) %>%
  gather(version, question, starts_with("v")) %>%
  mutate(version = as.numeric(gsub("v", "", version)),
         capacity = gsub("Can ___s ", "", question_text),
         capacity = gsub("\\?", "", capacity))

# TIDY DATA -------------------------------------------------------------------

d1 <- d %>%
  left_join(question_key) %>%
  select(run:question, question_text:capacity, response:general_comments) %>%
  filter(!is.na(character), !is.na(capacity)) %>%
  mutate(location = "preschool")

write.csv(d1, "./anonymized_data/study3_children46_anonymized.csv")
