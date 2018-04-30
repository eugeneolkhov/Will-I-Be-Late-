callAPI <- function(stations, lats, lons, dates) {
# function to call API to get data for specified locations and times

  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(darksky, dplyr)

  # create empty frame
  weather <- data.frame()

  for (i in 1:length(stations)) {
    for (j in 1:length(dates)) {

      # for each location (i), get the weather for each date (j),
      hourly <- get_forecast_for(lats[i], lons[i], dates[j], add_headers=TRUE)$hourly

      # replicate the location name so we have 1/hour for the date(j)
      station <- rep(stations[i], nrow(hourly))

      # combine the name with the data
      addName <- cbind(station,hourly)

      # add rows to overall data, matching by column name
      weather <- union_all(weather,addName)

      # remove the old data
      rm(hourly, station, addName)

    } # end location for
  } # end date for

  return(weather)

} # end function
