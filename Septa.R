
# Main script for SEPTA analysis

#--------------------------------------------------------------------------------------------------

# Goals:
#   Parse and understand SEPTA data (focusing on regional rail lines)
#   Identify weather associated with dates and times of SEPTA data
#   Identify factors that are predictive of delay, including:
    # individual stations,
    # line interactions,
    # day-of-week,
    # holidays, and
    # weather
#
#
# TBD:
#   Are we focusing on a single line or multiple?
#   Are we predicting:
#     whether a train will be late?
#     the probability of it being late?
#     how late it will be?
#     what factors influence whether a train will be late?
#     making recommendations on the schedule regarding lateness?
#   Which models should we use?


#-- Initial setup ---------------------------------------------------------------------------------

# install.packages('tidyverse')
# install.packages('openxlsx')
# install.packages('lubridate')

library(tidyverse)
library(openxlsx)
library(lubridate)

setwd("~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Project")
location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"

# Colorblind-friendly palette with grey:
cbGray <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbBlack <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#-- Data cleaning ---------------------------------------------------------------------------------

# source: Kaggle: https://www.kaggle.com/septa/on-time-performance
otp <- read.csv(paste(location,"otp.csv", sep="/"))
# trainView <- read.csv(paste(location,"trainView.csv", sep="/"))

# SEPTA - On Time Performance Data. 2016-03-23 to 2016-11-06
summary(otp)
head(otp)

# subset data for Paoli/Thorndale line and Fox-Chase

# identify inbound thorndale --> philly trains
thorndale <- otp %>%
  filter(origin == 'Thorndale')

foxchase <- otp %>%
  filter(origin == 'Fox Chase')

### NOTES: need to group by trainID and date

# source: DarkSky API - see "getWeather.R"
weather <- read.xlsx(paste(location,"weather.xlsx",sep="/"),sheetIndex=1)

# convert relevant columns to factors
weather$station <- factor(weather$station)
weather$time <- as_date(weather$time)
weather$summary <- factor(weather$summary)
weather$icon <- factor(weather$icon)
weather$precipType <- factor(weather$precipType)

summary(weather)







