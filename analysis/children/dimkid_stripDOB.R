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
jsonFormat = function(index) {
  # NOTE: requires dplyr and jsonlite packages
  # library(jsonlite)
  # library(dplyr)
  
  # make dataframe
  d.raw = data.frame()

    # read in file
  d.temp <- read.csv(paste0("./individual sessions/", files[index])) %>%
    mutate_each(funs = "as.character")
  
  # bind into same dataframe
  d.raw = bind_rows(d.raw, d.temp)
  
  return(d.raw)
}

# set working directory
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/data/")

# gather files
files = dir("individual sessions/")

for(i in 1:length(files)) {
  
  # read in and remove dob column
  d <- jsonFormat(i) %>%
    select(trialNum,
           bgColor,
           capacity,
           capWording,
           response,
           responseNum,
           hoverTime,
           rt,
           character,
           charWording,
           subid,
           dateOfTest,
           startTime,
           endTime,
           sessionDuration,
           testingSite,
           experimenter,
           gender,
           ethnicity,
           trialComments,
           sessionComments)

  # get subid
  subid <- d$subid[1]
  
  # name file
  name <- paste("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid/data/individual sessions/", subid, ".csv", sep = "")
  
  # write csv
  write.csv(d, file = name)
}


