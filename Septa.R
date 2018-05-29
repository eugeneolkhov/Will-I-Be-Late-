
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
# install.packages('skimr')
# install.packages('xray')
# install.packages('caret')

library(tidyverse)
library(openxlsx)
library(lubridate)
library(skimr)
library(xray)
library(caret)

setwd("~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-")
location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"

# Colorblind-friendly palette with grey:
cbGray <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbBlack <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#-- Dataset creation ------------------------------------------------------------------------------

# source: Kaggle: https://www.kaggle.com/septa/on-time-performance
otp <- read.csv(paste(location,"otp.csv", sep="/"))
# trainView <- read.csv(paste(location,"trainView.csv", sep="/"))

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
#skim(otp) # takes a while!
anomalies(otp)

# summary(trainView)
# head(trainView)
# skimr::skim(trainView)
# xray::anomalies(trainView)

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

rm(otp, start, end, delays, weather, weather_foxchase, weather_phila, weather_thorndale,
   foxchase_in_otp, foxchase_out_otp,
   foxchase_in_otp_weather, foxchase_out_otp_weather,
   Thorndale_list, Thorndale_list_out,
   thorndale_in_otp, thorndale_out_otp,
   thorndale_in_otp_weather, thorndale_out_otp_weather)

#-- Data cleaning ---------------------------------------------------------------------------------
location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"

# import saved data
foxchase_in_weather <- readRDS(paste(location,"foxchase_in_weather.RDS", sep="/"))
foxchase_out_weather <- readRDS(paste(location,"foxchase_out_weather.RDS", sep="/"))
thorndale_in_weather <- readRDS(paste(location,"thorndale_in_weather.RDS", sep="/"))
thorndale_out_weather <- readRDS(paste(location,"thorndale_out_weather.RDS", sep="/"))

# stack data
thorndale_data <- rbind(thorndale_in_weather, thorndale_out_weather)
foxchase_data <- rbind(foxchase_in_weather, foxchase_out_weather)
stack_data <- rbind(thorndale_data, foxchase_data)


rm(thorndale_in_weather, thorndale_out_weather, foxchase_in_weather, foxchase_out_weather)
#rm(thorndale_data, foxchase_data)


# Convert Status to Late/Not-Late
late <- 0 # threshold for lateness (late if > 'late')
thorndale_data <- mutate(thorndale_data,
                         delay = ifelse(thorndale_data$delay > late, 1,
                                        ifelse(thorndale_data$delay == 999, 999, 0)))

foxchase_data <- mutate(foxchase_data,
                        delay = ifelse(foxchase_data$delay > late, 1,
                                       ifelse(foxchase_data$delay == 999, 999, 0)))


stack_data <- mutate(stack_data,
                     delay = ifelse(stack_data$delay > late, 1,
                                    ifelse(stack_data$delay == 999, 999, 0)))




# Modify/Format Variables
thorndale_data <- thorndale_data %>%
  filter(delay != 999) %>%
  mutate(delay = factor(delay)) %>%
  mutate(day_of_week = factor(weekdays(round_timestamp))) %>%
  mutate(hour = factor(hour(round_timestamp))) %>%
  mutate(month = factor(month(round_timestamp))) %>%
  mutate(origin = factor(as.character(origin))) %>%
  mutate(next_station = factor(as.character(next_station)))

foxchase_data <- foxchase_data %>%
  filter(delay != 999) %>%
  mutate(delay = factor(delay)) %>%
  mutate(day_of_week = factor(weekdays(round_timestamp))) %>%
  mutate(hour = factor(hour(round_timestamp))) %>%
  mutate(month = factor(month(round_timestamp))) %>%
  mutate(origin = factor(as.character(origin))) %>%
  mutate(next_station = factor(as.character(next_station)))

stack_data <- stack_data %>%
  filter(delay != 999) %>%
  mutate(delay = factor(delay)) %>%
  mutate(day_of_week = factor(weekdays(round_timestamp))) %>%
  mutate(hour = factor(hour(round_timestamp))) %>%
  mutate(month = factor(month(round_timestamp))) %>%
  mutate(origin = factor(as.character(origin))) %>%
  mutate(next_station = factor(as.character(next_station)))


# Variable selection
  # Train ID has 94 levels - randomForest only supports up to 53.
model_vars <- c("delay", "origin", "next_station", "day_of_week", "month", "hour",
             "precipIntensity", "precipProbability", "temperature", "windSpeed", "visibility")

thorndale_model <- thorndale_data %>%
  arrange(train_id, timeStamp)  %>%
  select(model_vars)

foxchase_model <- foxchase_data %>%
  arrange(train_id, timeStamp)  %>%
  select(model_vars)

model_data <- stack_data %>%
  arrange(train_id, timeStamp)  %>%
  select(model_vars)

rm(late, foxchase_model, thorndale_model, model_vars, foxchase_data, thorndale_data, stack_data)

#-- Impute NAs ----------------------------------------------------------------------------------

# do we have NAs we need to fix?
#summary(thorndale_model)
#summary(foxchase_model) # windspeed
summary(model_data) # windspeed


# find mean between hour pre and post NA value
lagk <- function(x, k) {
  # x is column; k is lag#
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] )
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}
      # lagk can lag forwards or backwards! (forwards gives 'pre', backwards gives 'post')
      # x<-1:3;
      # (cbind(x, lagk(x, 1), lagk(x,-1)))
      #      x  1 -1
      # [1,] 1 NA  2
      # [2,] 2  1  3
      # [3,] 3  2 NA

# for columns in continuous:
# impute <- cbind(x, lagk (x, 1), lagk(x, -1)
# names(impute) <- c('x', 'pre', 'post')
# if x.isNull {
#   x <- mean(pre, post)
# }


# create list of data
data <- list(model_data)
# data <- list(thorndale_data, foxchase_data, model_data)

# identify columns to impute
continuous <- "windSpeed"
# continuous <- c("precipIntensity", "precipProbability", "temperature", "windSpeed", "visibility")


for (j in length(data)) {
  for (i in length(continuous)) {

    # extract column
    x <- data[[j]][continuous[i]][[1]]
      # data[j] returns a list, data[[j]] returns the value of the list (i.e., the data frame)
      # data[[j]][continuous[i]] is similar to dataframe$colname
      # data[[j]][continuous[i]][[1]] returns the value of the first column (i.e., the vector);
        # as before, [1] would return the dataframe column

    # create df with column + lag1 + lag-1
    impute <- as.data.frame(cbind(x, lagk(x, 1), lagk(x, -1)))
    names(impute) <- c('x','pre','post')

    # impute and replace na value
    impute <- impute %>%
      mutate(y = if_else(is.na(x), (impute$pre+impute$post)/2, x))

    # replace in original data
    data[[j]][continuous[i]][[1]] <- impute$y  # or impute[['y']]

  } # end for - continuous
} # end for - data

#summary(thorndale_data)
#summary(foxchase_data)
summary(model_data)

rm(data, continuous)

#-- Drop NAs --------------------------------------------------------------------------------------

#summary(foxchase_data)
summary(model_data)

#foxchase_data <- foxchase_data[complete.cases(foxchase_data),]
model_data <- model_data[complete.cases(model_data),]

#summary(foxchase_data)
summary(model_data)


#-- Create Dummy Variables and Scale --------------------------------------------------------------
library(tidyverse)
library(caret)

delay <- model_data$delay

# break out factors
categorical <- model_data %>%
  select(hour, day_of_week, month, origin, next_station)
dummies <- model.matrix(~ .,data = categorical)

# scale continuous data
continuous <- model_data %>%
  select(precipIntensity, precipProbability, temperature, windSpeed, visibility)
scaled <- scale(continuous)
summary(scaled)

model_data <- as.data.frame(cbind(delay, cbind(dummies, scaled)))

rm(categorical, continuous, dummies, scaled, delay)

#-- Save model data -------------------------------------------------------------------------------

### Export RDS
saveRDS(model_data, paste(location,"model_data.rds", sep="/"))



#--- Split Train/Test -----------------------------------------------------------------------------

location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"

# import saved data
model_data <- readRDS(paste(location,"model_data.rds", sep="/"))

# if you need the class labels to be a labelled factor
model_data$delay <- factor(model_data$delay, labels = c("on.time","late"), levels = c(1, 2))

# Split into training/testing set
set.seed(100)
# index <- createDataPartition(model_data$delay, p = .7, list = FALSE, times = 1)
index <- sample(nrow(model_data), 0.7*nrow(model_data), replace=F)

model_train <- model_data[ index,]
model_test  <- model_data[-index,]

### Note: Don't forget to crossvalidate on training sets!


# create mini set for fast testing
set.seed(9876)
pct <- .02 # can incrementally increase this
a <- sample(nrow(model_train), pct * nrow(model_train), replace = FALSE)
b <- sample(nrow(model_test), pct * nrow(model_test), replace = FALSE)

test_train <- model_train[a,]
test_test <- model_test[b,]

rm(index, a,b)

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



#-- SVM -------------------------------------------------------------------------------------------

