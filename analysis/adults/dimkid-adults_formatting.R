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
      
      # subject-level data: duration
      duration = ifelse(
        is.null(jd$answers$data$allData$sessionDuration) == TRUE, NA,
        jd$answers$data$allData$sessionDuration),
      
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
      comments = as.character(
        ifelse(
          is.null(jd$answers$data$allData$comments) |
            jd$answers$data$allData$comments == "", NA,
          jd$answers$data$allData$comments)),
      
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

# US run 02 (2016-07-19, 7-point scale)
d_us_run_02 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/turk/us_run-02/",
  runName = "us_run_02")

# US run 02 (2016-07-19, 7-point scale)
d_us_run_02b = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/turk/us_run-02b/",
  runName = "us_run_02b")

# US run 03 (2016-12-08, 3-point scale, original wording for 'free will' and 'intentions')
d_us_run_03 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/turk/us_run-03/",
  runName = "us_run_03")

# US run 03 (2016-12-08, 3-point scale, original wording for 'free will' and 'intentions')
d_us_run_03b = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/turk/us_run-03b/",
  runName = "us_run_03b")

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy = full_join(d_us_run_01, d_us_run_01b) %>%
  full_join(d_us_run_02) %>%
  full_join(d_us_run_02b) %>%
  full_join(d_us_run_03) %>%
  full_join(d_us_run_03b) %>%
  mutate(
    run = factor(run),
    subid = factor(subid),
    duration = as.numeric(duration),
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

# studies
d1 <- d_tidy %>%
  filter(run %in% c("us_run_01", "us_run_01b")) %>%
  mutate(responseNum = as.numeric(
    ifelse(response == "no", "0",
           ifelse(response == "kinda", "0.5",
                  ifelse(response == "yes", "1", NA)))))

d2 <- d_tidy %>%
  filter(run %in% c("us_run_02", "us_run_02b")) %>%
  mutate(responseNum = as.numeric(as.character(response)))

d3 <- d_tidy %>%
  filter(run %in% c("us_run_03", "us_run_03b")) %>%
  mutate(responseNum = as.numeric(as.character(responseNum)))

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write run 01 to de-identified csv file
write.csv(d1, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-01_2016-06-05_anonymized.csv")

d1 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-01_2016-06-05_anonymized.csv")          

# write run 02 to de-identified csv file
write.csv(d2, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-02_2016-07-19_anonymized.csv")

d2 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-02_2016-07-19_anonymized.csv") 

# write run 03 to de-identified csv file
write.csv(d3, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-03_2016-12-08_anonymized.csv")

d3 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/dimkid/data/adults/us_run-03_2016-12-08_anonymized.csv") 

# view comments
comments_01 = d1 %>%
  select(comments, charName, subid) %>%
  distinct() %>%
  filter(comments != "NA")

comments_02 = d2 %>%
  select(comments, charName, subid) %>%
  distinct() %>%
  filter(comments != "NA")

comments_03 = d3 %>%
  select(comments, charName, subid) %>%
  distinct() %>%
  filter(comments != "NA")


# View(comments_03)
