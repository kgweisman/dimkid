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
  files = dir("production-results/")
  
  # make dataframe
  d.raw = data.frame()
  
  for(i in 1:length(files)) {
    
    # gather files
    f = files[i]
    jf <- paste0("production-results/", f)
    
    # parse JSON object
    jd <- fromJSON(paste(readLines(jf), collapse=""))
    
    # store relevant variables in dataframe 
    id <- data.frame(
      # run
      run = runName,
      
      # subject-level data: identity
      subid = paste0(runName, "_", i),
      charName = jd$answers$data$allData$character$charName,
      
      # subject-level data: demographics
      country = ifelse(
        is.null(jd$answers$data$allData$country) == TRUE, NA,
        jd$answers$data$allData$country),    
      age = ifelse(
        is.null(jd$answers$data$allData$age) == TRUE, NA,
        jd$answers$data$allData$age),
      gender = ifelse(
        is.null(jd$answers$data$allData$gender) == TRUE, NA,
        jd$answers$data$allData$gender),
      englishNative = ifelse(
        is.null(jd$answers$data$allData$englishNative) == TRUE, NA,
        jd$answers$data$allData$englishNative),
      ethnicity = ifelse(
        is.null(jd$answers$data$allData$ethnicity) == TRUE, NA,
        paste(jd$answers$data$allData$ethnicity, collapse = ', ')),
      education = ifelse(
        is.null(jd$answers$data$allData$education) == TRUE, NA,
        jd$answers$data$allData$education),
      religionChild = ifelse(
        is.null(jd$answers$data$allData$religionChild) == TRUE, NA,
        paste(jd$answers$data$allData$religionChild, collapse = ', ')),
      religionNow = ifelse(
        is.null(jd$answers$data$allData$religionNow) == TRUE, NA,
        paste(jd$answers$data$allData$religionNow, collapse = ', ')),
      job = ifelse(
        is.null(jd$answers$data$allData$job) == TRUE | 
          jd$answers$data$allData$job == "", NA,
        jd$answers$data$allData$job),
      
      # subject-level data: open-ended responses
      comments = ifelse(
        is.null(jd$answers$data$allData$comments) | 
          jd$answers$data$allData$comments == "", NA,
        jd$answers$data$allData$comments),
      
      # trial-level data:
      trialNum = jd$answers$data$allData$trialData$trialNum,
      bgColor = jd$answers$data$allData$trialData$bgColor,
      capacity = jd$answers$data$allData$trialData$capacity,
      capWording = jd$answers$data$allData$trialData$capWording,
      hoverTime = jd$answers$data$allData$trialData$hoverTime,
      rt = jd$answers$data$allData$trialData$rt,
      response = jd$answers$data$allData$trialData$response,
      responseNum = jd$answers$data$allData$trialData$responseNum)
      
    # bind into same dataframe
    d.raw = bind_rows(d.raw, id)
    
  }
  
  return(d.raw)
}

# --- READING IN DATA OBJECTS -------------------------------------------------

# US run 01 (2016-07-05)
d_us_run_01 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/turk/us_run-01/",
  runName = "us_run_01")

# US run 01 (2016-07-05)
d_us_run_01b = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/turk/us_run-01b/",
  runName = "us_run_01b")

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy = full_join(d_us_run_01, d_us_run_01b) %>%
  # full_join(d_us_run_02) %>%
  mutate(
    run = factor(run),
    subid = factor(subid),
    country_selfrep = factor(country),
    country = factor(ifelse(grepl("india", subid) == T, "india", 
                            ifelse(grepl("us", subid) == T, "us",
                                   NA))),
    charName = factor(charName),
    age = as.numeric(age),
    gender = factor(gender),
    ethnicity = factor(ethnicity),
    education = factor(education),
    religionChild = factor(religionChild),
    religionNow = factor(religionNow),
    englishNative = factor(englishNative),
    job = factor(job),
    bgColor = factor(bgColor),
    hoverTime = as.numeric(hoverTime),
    rt = as.numeric(rt),
    response = factor(response))

glimpse(d_tidy)

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-01_2016-06-05_anonymized.csv")

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-01_2016-06-05_anonymized.csv")          
          
# view comments
comments = d %>%
  select(comments, charName, subid) %>%
  distinct() %>%
  filter(comments != "NA")

View(comments)
