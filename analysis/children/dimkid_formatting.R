# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)
library(stringr)

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
  files = dir("individual sessions/")
  
  # make dataframe
  d.raw = data.frame()
  
  for(i in 1:length(files)) {

    # read in file
    d.temp <- read.csv(paste0("./individual sessions/", files[i])) %>%
      mutate(run = runName) %>%
      mutate_each(funs = "as.character")
    
    # bind into same dataframe
    d.raw = bind_rows(d.raw, d.temp)
    
  }
  
  return(d.raw)
}

# --- READING IN DATA OBJECTS -------------------------------------------------

# Kid run 01 (2016-06-28 to ...)
d_run_01 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/data/",
  runName = "run_01")

# --- TIDYING -----------------------------------------------------------------

# read in ages
ages <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/dimkid_participant_ages_2017-01-06.csv") %>%
  select(-age_formula, -comments) %>%
  mutate(ethnicityCat1 = 
           factor(ifelse(is.na(ethnicity), NA,
                         ifelse(ethnicity == "white, caucasian, or european american",
                                "euro-american", "multicultural"))),
         ethnicityCat2 = 
           factor(ifelse(is.na(ethnicity), NA,
                         ifelse(ethnicity == "white, caucasian, or european american",
                                "euro-american",
                                ifelse(grepl("east asian", ethnicity) |
                                         grepl("south", ethnicity) |
                                         grepl("chinese", ethnicity) |
                                         grepl("filipino", ethnicity),
                                       "asian", NA)))))

# clean up variables
d_tidy = d_run_01 %>%
  # full_join(d_run_02) %>%
  mutate(
    run = factor(run),
    subid = toupper(as.character(subid)),
    character = factor(character),
    gender = factor(gender),
    ethnicity = factor(ethnicity),
    bgColor = factor(bgColor),
    hoverTime = as.numeric(hoverTime),
    rt = as.numeric(rt),
    response = factor(response)) %>%
  select(-gender, -ethnicity) %>%
  left_join(ages) %>%
  mutate(subid = factor(subid))

glimpse(d_tidy)

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2017-01-06_anonymized.csv")

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2017-01-06_anonymized.csv")[-1]        
          
