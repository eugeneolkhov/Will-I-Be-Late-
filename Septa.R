
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


#-- Initial setup ---------------------------------------------------------------------------------

# install.packages('tidyverse')
# install.packages('openxlsx')
# install.packages('lubridate')

library(tidyverse)
library(openxlsx)
library(lubridate)

setwd("~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-")
location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"

# Colorblind-friendly palette with grey:
cbGray <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbBlack <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#-- Dataset creation ------------------------------------------------------------------------------

# source: Kaggle: https://www.kaggle.com/septa/on-time-performance
otp <- read.csv(paste(location,"otp.csv", sep="/"))
trainView <- read.csv(paste(location,"trainView.csv", sep="/"))

# Clean up train names
otp$train_id <- gsub('[A-Za-z/. /-]+', '' ,otp$train_id)

# Standardize status (On Time = 0)
otp$status <- gsub(' min', '', otp$status)
otp$status <- gsub('On Time', '0', otp$status)

otp$delay <- as.numeric(otp$status)
otp$status <- NULL


# Review info
# SEPTA - On Time Performance Data. 2016-03-23 to 2016-11-06
summary(otp)
head(otp)

summary(trainView)
head(trainView)

# quick summary - how late are the trains?
delays <- otp %>%
  group_by(delay) %>%
  summarize(n=n()) %>%
  mutate(pct = n/nrow(otp)) %>%
  arrange(desc(pct))


ggplot(delays, aes(delay)) +
  geom_histogram(bins = 90)

ggplot(delays, aes(x=delay, y=cumsum(pct))) +
  geom_line()

ggplot(delays, aes(x=delay, y=cumsum(pct))) +
  geom_line() +
  xlim(0,5)


# note that dates are from 2016-03-23 to 2016-11-06; this was in the midst of septa train recall
# see http://www.septa.org/media/releases/2016/7-15-16.html

# dates get converted to numbers in R:
start <- as.numeric( as_date("2016-03-23") )
# as_date(16883) # "2016-03-23" == 16883

end <- as.numeric( as_date("2016-11-06") )
# as_date(17111) # "2016-11-26" == 17111



### subset data for Paoli/Thorndale line and Fox-Chase

# subset data for Paoli/Thorndale line and Fox-Chase
Thorndale_list <- c('Downingtown', 'Whitford', 'Exton', 'Malvern', 'Paoli', 'Daylesford',
                    'Berwyn', 'Devon', 'Strafford', 'Wayne', 'St. Davids', 'Radnor', 'Villanova',
                    'Rosemont', 'Bryn Mawr', 'Haverford', 'Ardmore', 'Wynnewood', 'Narberth',
                    'Merion', 'Overbrook', '30th Street Station', 'Suburban Station', 'Temple U')
Thorndale_list_out <- c('Downingtown', 'Whitford', 'Exton', 'Malvern', 'Paoli', 'Daylesford',
                        'Berwyn', 'Devon','Strafford', 'Wayne', 'St. Davids', 'Radnor', 'Villanova',
                        'Rosemont', 'Bryn Mawr', 'Haverford', 'Ardmore', 'Wynnewood','Narberth',
                        'Merion', 'Overbrook', '30th Street Station', 'Suburban Station', 'Thorndale')


### Subsetting with OTP data
# Thorndale Inbound
thorndale_in_otp <- otp %>%
  filter(origin == 'Thorndale' | origin == 'Malvern' | origin == 'Frazer Yard') %>%
  filter(next_station %in% Thorndale_list) %>%
  arrange(timeStamp)

# Foxchase Inbound
foxchase_in_otp <- otp %>%
  filter(origin == 'Fox Chase') %>%
  filter(next_station %in% c('30th Street Station','Suburban Station','Jefferson Station',
                             'Temple U','Olney','Wayne Junction', 'Lawndale','Cheltenham',
                             'Ryers')) %>%
  arrange(timeStamp)

# Thorndale Outbound
thorndale_out_otp <- otp %>%
  filter(origin %in% c('Temple U','Suburban Station',
                       '30th Street Station', 'Jefferson Station')) %>%
  filter(next_station %in% Thorndale_list_out) %>%
  arrange(timeStamp)

# Foxchase Outbound
foxchase_out_otp <- otp %>%
  filter(origin == '30th Street Station' | origin == 'Powelton Yard') %>%
  filter(next_station %in% c('30th Street Station','Fox Chase','Suburban Station',
                             'Jefferson Station','Temple U','Olney','Wayne Junction',
                             'Lawndale','Cheltenham','Ryers')) %>%
  arrange(timeStamp)


### Create a new column with nearest hour of timestamp (otp)

# Thorndale Inbound
thorndale_in_otp$timeStamp <- as.POSIXct(thorndale_in_otp$timeStamp, format="%Y-%m-%d %H:%M:%S")
thorndale_in_otp <- mutate(thorndale_in_otp,
                           round_timestamp = round_date(thorndale_in_otp$timeStamp, c("hour")))

# Foxchase Inbound
foxchase_in_otp$timeStamp <- as.POSIXct(foxchase_in_otp$timeStamp, format="%Y-%m-%d %H:%M:%S")
foxchase_in_otp <- mutate(foxchase_in_otp,
                           round_timestamp = round_date(foxchase_in_otp$timeStamp, c("hour")))

# Thorndale Outbound
thorndale_out_otp$timeStamp <- as.POSIXct(thorndale_out_otp$timeStamp, format="%Y-%m-%d %H:%M:%S")
thorndale_out_otp <- mutate(thorndale_out_otp,
                           round_timestamp = round_date(thorndale_out_otp$timeStamp, c("hour")))

# Foxchase Outbound
foxchase_out_otp$timeStamp <- as.POSIXct(foxchase_out_otp$timeStamp, format="%Y-%m-%d %H:%M:%S")
foxchase_out_otp <- mutate(foxchase_out_otp,
                          round_timestamp = round_date(foxchase_out_otp$timeStamp, c("hour")))


# source: DarkSky API - see "getWeather.R"
weather <- read.csv(paste(location, "weather.csv", sep="/"), header=T)
weather <- read.csv("weather.csv")

# convert relevant columns to factors
weather$station <- factor(weather$station)
#weather$time <- as_date(weather$time)
weather$time <- as.POSIXct(weather$time, format = "%m/%d/%y %H:%M")
weather$summary <- factor(weather$summary)
weather$icon <- factor(weather$icon)
weather$precipType <- factor(weather$precipType)

summary(weather)
str(weather)


# Split weather data by station
weather_foxchase <- filter(weather, station == "Fox Chase")
weather_phila <- filter(weather, station == "Philadelphia")
weather_thorndale <- filter(weather, station == "Thorndale")


### Merge each dataset with the weather data

# Thorndale Inbound (otp)
thorndale_in_otp_weather <- thorndale_in_otp %>%
  mutate(time = round_timestamp) %>%
  left_join(weather_thorndale, by = "time") %>%
  arrange(time)


# Thorndale Outbound
thorndale_out_otp_weather <- thorndale_out_otp %>%
  mutate(time = round_timestamp) %>%
  left_join(weather_thorndale, by = "time") %>%
  arrange(time)

# Fox Chase Inbound
foxchase_in_otp_weather <- foxchase_in_otp %>%
  mutate(time = round_timestamp) %>%
  left_join(weather_foxchase, by = "time") %>%
  arrange(time)

# Fox Chase Outbound
foxchase_out_otp_weather <- foxchase_out_otp %>%
  mutate(time = round_timestamp) %>%
  left_join(weather_foxchase, by = "time") %>%
  arrange(time)



### Export RDS
saveRDS(thorndale_in_otp_weather, paste(location,"thorndale_in_weather.RDS", sep="/"))
saveRDS(thorndale_out_otp_weather, paste(location,"thorndale_out_weather.RDS", sep="/"))
saveRDS(foxchase_in_otp_weather, paste(location,"foxchase_in_weather.RDS", sep="/"))
saveRDS(foxchase_out_otp_weather, paste(location,"foxchase_out_weather.RDS", sep="/"))


#-- Data cleaning ---------------------------------------------------------------------------------
location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"

# import saved data
foxchase_in_weather <- readRDS(paste(location,"foxchase_in_weather.RDS", sep="/"))
foxchase_out_weather <- readRDS(paste(location,"foxchase_out_weather.RDS", sep="/"))
thorndale_in_weather <- readRDS(paste(location,"thorndale_in_weather.RDS", sep="/"))
thorndale_out_weather <- readRDS(paste(location,"thorndale_out_weather.RDS", sep="/"))

# Convert Status to Late/Not-Late
thorndale_in_weather <- mutate(thorndale_in_weather,
                               delay = ifelse(thorndale_in_weather$delay > 0, 1,
                                             ifelse(thorndale_in_weather$delay == 999, 999, 0)))

thorndale_out_weather <- mutate(thorndale_out_weather,
                               delay = ifelse(thorndale_out_weather$delay > 0, 1,
                                             ifelse(thorndale_out_weather$delay == 999, 999, 0)))

foxchase_in_weather <- mutate(foxchase_in_weather,
                               delay = ifelse(foxchase_in_weather$delay > 0, 1,
                                             ifelse(foxchase_in_weather$delay == 999, 999, 0)))

foxchase_out_weather <- mutate(foxchase_out_weather,
                               delay = ifelse(foxchase_out_weather$delay > 0, 1,
                                             ifelse(foxchase_out_weather$delay == 999, 999, 0)))

# Modify/Format Variables
thorndale_in_weather <- thorndale_in_weather %>%
  filter(delay != 999) %>%
  mutate(delay = factor(thorndale_in_weather$delay)) %>%
  mutate(day_of_week = factor(weekdays(thorndale_in_weather$round_timestamp))) %>%
  mutate(hour = factor(hour(thorndale_in_weather$round_timestamp))) %>%
  mutate(month = factor(month(thorndale_in_weather$round_timestamp))) %>%
  mutate(origin = factor(as.character(thorndale_in_weather$origin))) %>%
  mutate(next_station = factor(as.character(thorndale_in_weather$next_station)))

thorndale_out_weather <- thorndale_out_weather %>%
  filter(delay != 999) %>%
  mutate(delay = factor(thorndale_out_weather$delay)) %>%
  mutate(day_of_week = factor(weekdays(thorndale_out_weather$round_timestamp))) %>%
  mutate(hour = factor(hour(thorndale_out_weather$round_timestamp))) %>%
  mutate(month = factor(month(thorndale_out_weather$round_timestamp))) %>%
  mutate(origin = factor(as.character(thorndale_out_weather$origin))) %>%
  mutate(next_station = factor(as.character(thorndale_out_weather$next_station)))

foxchase_in_weather <- foxchase_in_weather %>%
  filter(delay != 999) %>%
  mutate(delay = factor(foxchase_in_weather$delay)) %>%
  mutate(day_of_week = factor(weekdays(foxchase_in_weather$round_timestamp))) %>%
  mutate(hour = factor(hour(foxchase_in_weather$round_timestamp))) %>%
  mutate(month = factor(month(foxchase_in_weather$round_timestamp))) %>%
  mutate(origin = factor(as.character(foxchase_in_weather$origin))) %>%
  mutate(next_station = factor(as.character(foxchase_in_weather$next_station)))

foxchase_out_weather <- foxchase_out_weather %>%
  filter(delay != 999) %>%
  mutate(delay = factor(foxchase_out_weather$delay)) %>%
  mutate(day_of_week = factor(weekdays(foxchase_out_weather$round_timestamp))) %>%
  mutate(hour = factor(hour(foxchase_out_weather$round_timestamp))) %>%
  mutate(month = factor(month(foxchase_out_weather$round_timestamp))) %>%
  mutate(origin = factor(as.character(foxchase_out_weather$origin))) %>%
  mutate(next_station = factor(as.character(foxchase_out_weather$next_station)))


# Variable selection
  # Train ID has 94 levels - randomForest only supports up to 53.
model_vars <- c("origin", "next_station", "delay", "day_of_week", "month", "hour",
             "precipIntensity", "precipProbability", "temperature", "windSpeed", "visibility")

thorndale_in_model <- select(thorndale_in_weather, model_vars)
thorndale_out_model <- select(thorndale_out_weather, model_vars)
foxchase_in_model <- select(foxchase_in_weather, model_vars)
foxchase_out_model <- select(foxchase_out_weather, model_vars)

# Split into training/testing set
set.seed(100)
train <- sample(nrow(thorndale_in_model), 0.7*nrow(thorndale_in_model), replace = FALSE)
thorndale_in_train <- thorndale_in_model[train,]
thorndale_in_test <- thorndale_in_model[-train,]

set.seed(101)
train <- sample(nrow(thorndale_out_model), 0.7*nrow(thorndale_out_model), replace = FALSE)
thorndale_out_train <- thorndale_out_model[train,]
thorndale_out_test <- thorndale_out_model[-train,]

set.seed(102)
train <- sample(nrow(foxchase_in_model), 0.7*nrow(foxchase_in_model), replace = FALSE)
foxchase_in_train <- foxchase_in_model[train,]
foxchase_in_test <- foxchase_in_model[-train,]

set.seed(103)
train <- sample(nrow(foxchase_out_model), 0.7*nrow(foxchase_out_model), replace = FALSE)
foxchase_out_train <- foxchase_out_model[train,]
foxchase_out_test <- foxchase_out_model[-train,]
  # Note: Don't forget to crossvalidate on training sets!


rm(thorndale_in_weather, thorndale_out_weather, foxchase_in_weather, foxchase_out_weather,
   thorndale_in_model, thorndale_out_model, foxchase_in_model, foxchase_out_model,
   train)


#-- Models ----------------------------------------------------------------------------------------
# Naive Bayes (+ decision tree for binning?) - Liz
# Random Forest - Eugene
# Autoregression; Neural Net - Andrew
# SVM - Alex
# Logistic (+ regularization?) - Abhay

### Timeline ###
# data by 15 May
# models by 22 May
# CV by 29 May
# present on 5 June

#-- Random Forest  --------------------------------------------------------------------------------
#install.packages("randomForest")
library(randomForest)

# Initial Model
thorndale_in_model <- randomForest(delay ~ ., data = thorndale_in_train, importance = TRUE)
thorndale_in_model

# Predicting on train set
thorndale_in_pred_train <- predict(thorndale_in_model, thorndale_in_train, type = "class")

# Checking classification accuracy
table(thorndale_in_pred_train, thorndale_in_train$delay)


# Predicting on Validation set
thorndale_in_pred_test <- predict(thorndale_in_model, thorndale_in_test, type = "class")

# Checking classification accuracy
mean(thorndale_in_pred_test == thorndale_in_test$delay)
prop.table(table(thorndale_in_pred_test,thorndale_in_test$delay))

# importance
importance(thorndale_in_model)


#-- SVM -------------------------------------------------------------------------------------------
#install.packages("e1071")
library(e1071)

thorndale_in_model <- svm(delay ~ ., data = thorndale_in_train,
                          type = "C-classification", scale = TRUE,
    cost = 1, # CV
    epsilon = 0.1, # CV
      kernel = "radial", # needs CV
      gamma = 1 / ncol(data), # for all except linear; needs CV
      # degree = 3, # for kernel = polynomial; needs CV
      coef0 = 0, # for all kernels except polynomial & sigmoid; needs CV
    class.weights = NULL, # probably don't need to weight classes
    shrinking = TRUE,
    cross = 0,
    fitted = TRUE)

# Predicting on train set
thorndale_in_pred_train <- predict(thorndale_in_model, thorndale_in_train, type = "class")

# Checking classification accuracy
table(thorndale_in_pred_train, thorndale_in_train$delay)


# Predicting on Validation set
thorndale_in_pred_test <- predict(thorndale_in_model, thorndale_in_test, type = "class")

# Checking classification accuracy
mean(thorndale_in_pred_test == thorndale_in_test$delay)
prop.table(table(thorndale_in_pred_test,thorndale_in_test$delay))

# importance
importance(thorndale_in_model)
