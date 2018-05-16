
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

# SEPTA - On Time Performance Data. 2016-03-23 to 2016-11-06
summary(otp)
head(otp)

summary(trainView)
head(trainView)

# quick summary - how late are the trains?
delays <- trainView %>%
  group_by(status) %>%
  summarize(n=n()) %>%
  mutate(pct = n/sum(n)) %>%
  arrange(desc(pct))

delays$status <- as.numeric(delays$status)

delays

ggplot(delays, aes(x=status, y=pct)) +
  geom_bar(stat="identity")

# those spikes are real (in the data, once we convert the factor to a numeric) -- why do they exist?


# note that dates are from 2016-03-23 to 2016-11-06; this was in the midst of septa train recall
# see http://www.septa.org/media/releases/2016/7-15-16.html

# dates get converted to numbers in R:
start <- as.numeric( as_date("2016-03-23") )
# as_date(16883) # "2016-03-23" == 16883

end <- as.numeric( as_date("2016-11-06") )
# as_date(17111) # "2016-11-26" == 17111



### subset data for Paoli/Thorndale line and Fox-Chase

# Identify stations on each line
Thorndale_list <- c('Downingtown', 'Whitford', 'Exton', 'Malvern', 'Paoli', 'Daylesford', 'Berwyn',
                    'Devon', 'Strafford', 'Wayne', 'St. Davids', 'Radnor', 'Villanova', 'Rosemont',
                    'Bryn Mawr', 'Haverford', 'Ardmore', 'Wynnewood', 'Narberth', 'Merion',
                    'Overbrook', '30th Street Station', 'Suburban Station', 'Temple U')

FoxChase_list <- c('Temple U', 'Olney', 'Wayne Junction', 'Lawndale', 'Cheltenham', 'Ryers')


# identify inbound trains (into Philly)
thorndale_in <- trainView %>%
  filter(source == 'Thorndale' | source == 'Malvern') %>%
  filter(next_station %in% Thorndale_list) %>%
  arrange(timeStamp0)

foxchase_in <- trainView %>%
  filter(source == 'Fox Chase') %>%
  filter(next_station %in% FoxChase_list) %>%
  arrange(timeStamp0)


# identify outbound trains (leaving Philly)
thorndale_out <- trainView %>%
  filter(dest == 'Temple U' & source == 'Thorndale') %>%
  arrange(timeStamp0)

foxchase_out <- trainView %>%
  filter(dest == 'Fox Chase' & source == '30th St') %>%
  arrange(timeStamp0)


## Create a new column with nearest hour of timestamp

#Thorndale Inbound
thorndale_in$timeStamp0 <- as.POSIXct(thorndale_in$timeStamp0, format="%Y-%m-%d %H:%M:%S")
thorndale_in <- mutate(thorndale_in,
                       round_timestamp = round_date(thorndale_in$timeStamp0, c("hour")))

#Thorndale Outbound
thorndale_out$timeStamp0 <- as.POSIXct(thorndale_out$timeStamp0, format="%Y-%m-%d %H:%M:%S")
thorndale_out <- mutate(thorndale_out,
                       round_timestamp = round_date(thorndale_out$timeStamp0, c("hour")))

#Fox Chase Inbound
foxchase_in$timeStamp0 <- as.POSIXct(foxchase_in$timeStamp0, format="%Y-%m-%d %H:%M:%S")
foxchase_in <- mutate(foxchase_in,
                       round_timestamp = round_date(foxchase_in$timeStamp0, c("hour")))

#Fox Chase Outbound
foxchase_out$timeStamp0 <- as.POSIXct(foxchase_out$timeStamp0, format="%Y-%m-%d %H:%M:%S")
foxchase_out <- mutate(foxchase_out,
                       round_timestamp = round_date(foxchase_out$timeStamp0, c("hour")))



### Load Weather data
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

# summary(weather)
# str(weather)


# Split weather data by station
weather_foxchase <- filter(weather, station == "Fox Chase")
weather_phila <- filter(weather, station == "Philadelphia")
weather_thorndale <- filter(weather, station == "Thorndale")


### Merging each dataset with the weather data

# Thorndale Inbound
thorndale_in_weather <- thorndale_in %>%
  mutate(time = round_timestamp) %>%
  left_join(weather_thorndale, by = "time") %>%
  arrange(time)

# Thorndale Outbound
thorndale_out_weather <- thorndale_out %>%
  mutate(time = round_timestamp) %>%
  left_join(weather_phila, by = "time") %>%
  arrange(time)

# Fox Chase Inbound
foxchase_in_weather <- foxchase_in %>%
  mutate(time = round_timestamp) %>%
  left_join(weather_foxchase, by = "time") %>%
  arrange(time)

# Fox Chase Outbound
foxchase_out_weather <- foxchase_out %>%
  mutate(time = round_timestamp) %>%
  left_join(weather_phila, by = "time") %>%
  arrange(time)


### Export RDS
saveRDS(thorndale_in_weather, "thorndale_in_weather.RDS")
saveRDS(thorndale_out_weather, "thorndale_out_weather.RDS")
saveRDS(foxchase_in_weather, "foxchase_in_weather.RDS")
saveRDS(foxchase_out_weather, "foxchase_out_weather.RDS")


#-- Data cleaning ---------------------------------------------------------------------------------
# import saved data
foxchase_in_weather <- readRDS(paste(location,"foxchase_in_weather.RDS", sep="/"))
foxchase_out_weather <- readRDS(paste(location,"foxchase_out_weather.RDS", sep="/"))
thorndale_in_weather <- readRDS(paste(location,"thorndale_in_weather.RDS", sep="/"))
thorndale_out_weather <- readRDS(paste(location,"thorndale_out_weather.RDS", sep="/"))

### NOTES: need to group by trainID and date




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

#-- SVM -------------------------------------------------------------------------------------------
library(e1071)

model <- svm(delay ~ ., scale = TRUE,
    cost = 1, # CV
    epsilon = 0.1, # CV
      kernel = "radial", # needs CV
      gamma = if (is.vector(x)) 1 else 1 / ncol(x), # for all except linear; needs CV
      degree = 3, # for kernel = polynomial; needs CV
      coef0 = 0, # for all kernels except polynomial & sigmoid; needs CV
      nu = 0.5, # CV
    class.weights = NULL, # probably don't need to weight classes
    shrinking = TRUE,
    cross = 0,
    fitted = TRUE)

predict(model, testdata)
