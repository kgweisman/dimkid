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
      mutate(run = runName)
    
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

# clean up variables
d_tidy = d_run_01 %>%
  # full_join(d_run_02) %>%
  mutate(
    run = factor(run),
    subid = factor(subid),
    character = factor(character),
    gender = factor(gender),
    ethnicity = factor(ethnicity),
    bgColor = factor(bgColor),
    hoverTime = as.numeric(hoverTime),
    rt = as.numeric(rt),
    response = factor(response))

glimpse(d_tidy)

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2016-07-27_anonymized.csv")

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/children/run-01_2016-07-27_anonymized.csv")          
          
