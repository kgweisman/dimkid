# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(tidyverse)
# library(lme4)
library(jsonlite)
# library(stats)
# library(stringr)

# clear environment
rm(list=ls())

# set up function for reading in data from JSON objects
jsonFormat = function(wd, runName) {
  # NOTE: requires dplyr and jsonlite packages
  # library(jsonlite)
  # library(dplyr)
  
  # set working directory
  setwd(wd)
  
  # gather files
  files = dir(paste(runName, "individual sessions/"))
  
  # make dataframe
  d.raw = data.frame()
  
  for(i in 1:length(files)) {
    
    # read in file
    d.temp <- read.csv(paste0("./", runName, " individual sessions/", files[i])) %>%
      mutate(run = runName) %>%
      mutate_all(funs("as.character")) 
    
    # bind into same dataframe
    d.raw = bind_rows(d.raw, d.temp)
    
  }
  
  return(d.raw)
}

# --- READING IN DATA OBJECTS -------------------------------------------------

# Kid run 05 dimkid4yo (2018-02-xx to ...)
d_run_05 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/data/",
  runName = "run_05 dimkid4yo")

# --- TIDYING -----------------------------------------------------------------

# read in ages
# ages <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/dimkid_participant_ages_2017-01-19.csv") %>%
#   select(-age_formula, -comments) %>%
#   mutate(ethnicityCat1 = 
#            factor(ifelse(is.na(ethnicity), NA,
#                          ifelse(ethnicity == "white, caucasian, or european american",
#                                 "euro-american", "multicultural"))),
#          ethnicityCat2 = 
#            factor(ifelse(is.na(ethnicity), NA,
#                          ifelse(ethnicity == "white, caucasian, or european american",
#                                 "euro-american",
#                                 ifelse(grepl("east asian", ethnicity) |
#                                          grepl("south", ethnicity) |
#                                          grepl("chinese", ethnicity) |
#                                          grepl("filipino", ethnicity),
#                                        "asian", NA)))))

# clean up variables
d_tidy = d_run_05 %>%
  mutate_at(vars(experimenter, location, game, character, version, run),
           funs(factor)) %>%
  mutate_at(vars(age_days, question), funs(as.numeric)) %>%
  mutate(subid = factor(toupper(as.character(subid))),
         subid_char = paste(subid, as.character(character), sep = "_"),
         gender = factor(tolower(gsub("FALSE", "f", gender))),
         response = factor(response,
                           levels = c("no", "kinda", "yes")),
         age_years = age_days/365,
         ethnicity_collapse = case_when(
           grepl(" ", ethnicity) | grepl("///", ethnicity) ~ "multiple",
           TRUE ~ ethnicity),
         ethnicity_collapse = factor(ethnicity_collapse),
         response_num = case_when(
           response == "no" ~ "0",
           response == "kinda" ~ "0.5",
           response == "yes" ~ "1",
           TRUE ~ "NA"),
         response_num = as.numeric(response_num)) %>%
  select(run, subid, 
         age_days, age_years, gender, ethnicity, ethnicity_collapse,
         experimenter, location, 
         game, character, version, subid_char, char_label, 
         question, response, response_num, comments,
         general_comments)

glimpse(d_tidy)

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write to de-identified csv file

# run 05 dimkid4yo
write.csv(d_tidy %>% filter(run == "run_05 dimkid4yo"), "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-05_2018-08-07_anonymized.csv")

# read in
d5 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-05_2018-08-07_anonymized.csv")[-1]        
