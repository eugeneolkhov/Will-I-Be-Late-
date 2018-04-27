#install.packages("darksky")

library(darksky)
library(tidyverse)
library(lubridate)
library(xlsx)

location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/Data"

#-- API setup -------------------------------------------------------------------------------------

# set up API key -
# https://darksky.net/dev/account
# The easiest way to accomplish this is to set it in the '.Renviron' file in your home directory.
#install.packages("usethis")
usethis::edit_r_environ()
# in the file that opens, type: DARKSKY_API_KEY = < your_key_here >
# save and restart R


#-- Get weather info ------------------------------------------------------------------------------

# test <- get_forecast_for(43.2672, -70.8617, "2013-05-06T12:00:00-0400", add_headers=TRUE)

# get forecast for stations:
# Philadelphia:
# Thornedale:
# ...

stations <- c("Philadelphia","Thorndale")
latitudes <- c(39.954129, 39.992838)
longitudes <- c(-75.16682624816895, 75.763693)

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
dates2 <- dates[8:100]
dates3 <- dates[101:229]



# load function to call the DarkSky API for our dates and locations
source("callAPI.R")

# this will make lots of API calls.  If you call > 1,000 per day, it costs money!
  # one call is one station per day

# weather1 <- callAPI(stations, latitudes, longitudes, dates1)
# weather2 <- callAPI(stations, latitudes, longitudes, dates2)
weather3 <- callAPI(stations, latitudes, longitudes, dates3)

# join all of the weather subsets together
weather <- union_all(weather1, weather2)
weather <- union_all(weather, weather3)

# convert relevant columns to factors
weather$names <- factor(weather$names)
weather$time <- factor(weather$time)
weather$summary <- factor(weather$summary)
weather$icon <- factor(weather$icon)
weather$precipType <- factor(weather$precipType)

# save to file
write.xlsx2(weather, paste(location,"weather.xlsx", sep="/"), col.names = T)

