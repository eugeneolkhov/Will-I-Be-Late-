# install.packages('darksky')
# install.packages('tidyverse')
# install.packages('lubridate')
# install.packages('openxlsx')

library(darksky)
library(tidyverse)
library(lubridate)
library(openxlsx)

location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"
setwd("~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-")

#-- API setup -------------------------------------------------------------------------------------

# set up API key -
# https://darksky.net/dev/account
# The easiest way to accomplish this is to set it in the '.Renviron' file in your home directory.
#install.packages("usethis")
usethis::edit_r_environ()
# in the file that opens, type: DARKSKY_API_KEY = < your_key_here >
# save and restart R


#-- Set up locations and dates --------------------------------------------------------------------

# test <- get_forecast_for(43.2672, -70.8617, "2013-05-06T12:00:00-0400", add_headers=TRUE)

# get forecast for stations:
# Philadelphia ,Thornedale, Fox Chase

stations <- c("Philadelphia","Thorndale")
latitudes <- c(39.954129, 39.992838)
longitudes <- c(-75.16682624816895, -75.763693)

stations <- "Fox Chase"
latitudes <- 40.075953
longitudes <- -75.083592

# need weather from 2016-03-23 to 2016-11-06
# dates get converted to numbers in R:
start <- as.numeric( as_date("2016-03-23") )
# as_date(16883) # "2016-03-23" == 16883

end <- as.numeric( as_date("2016-11-06") )
# as_date(17111) # "2016-11-26" == 17111

dates <- c(16883:17111) %>%  # create vector from 16883 to 17111
  as_date() %>%             # convert to date
  as.character() %>%        # convert to string
  paste0("T12:00:00-0400")

# It's probably not smart to try calling the API on 228 dates all at once.
# Let's create a few sub-ranges we can combine later

dates1 <- dates[1:7]
dates2 <- dates[8:50]
dates3 <- dates[51:100]
dates4 <- dates[101:150]
dates5 <- dates[151:200]
dates6 <- dates[201:229]


#-- Get weather info ------------------------------------------------------------------------------
# load function to call the DarkSky API for our dates and locations
source("callAPI.R")

# this will make lots of API calls.  If you call > 1,000 per day, it costs money!
  # one call is one station per day

weather1 <- callAPI(stations, latitudes, longitudes, dates1)
weather2 <- callAPI(stations, latitudes, longitudes, dates2)
weather3 <- callAPI(stations, latitudes, longitudes, dates3)
weather4 <- callAPI(stations, latitudes, longitudes, dates4)
weather5 <- callAPI(stations, latitudes, longitudes, dates5)
weather6 <- callAPI(stations, latitudes, longitudes, dates6)


#-- Combine and save data -------------------------------------------------------------------------


# read in previously scraped data
weather0 <- read.csv(paste(location, "weather.old.csv", sep="/"), header=T)
  # Note: renamed as "old" so we have a backup

# combine scraped data
weather <- union_all(weather1, weather2) %>%
  union_all(weather3) %>%
  union_all(weather4) %>%
  union_all(weather5) %>%
  union_all(weather6)

# check to make sure we have same columns
names(weather0) == names(weather)
# names(weather0)
# names(weather)

# check types to make sure we have same types to combine
str(weather0)
str(weather)

# convert variables to combine
weather0$time <- as.POSIXct(weather0$time)

weather$summary <- as.factor(weather$summary)
weather$icon <- as.factor(weather$icon)
weather$precipType <- as.factor(weather$precipType
                                )
# combine old and new
weather <- union_all(weather0, weather)


# # convert to appropriate formats
# weather$station <- factor(weather$station)
# weather$time <- as_date(weather$time)
# weather$summary <- factor(weather$summary)
# weather$icon <- factor(weather$icon)
# weather$precipType <- factor(weather$precipType)

# save to file
write.csv(weather, paste(location, "weather.csv", sep="/"), col.names = T, row.names=F)

