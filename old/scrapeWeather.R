scrapeWeather <- function(startDate, endDate, delay) {

  # Package manager
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(xml2)


  #Parse html pages for data
  simpleScraper <- function(doc, delay = 0) {
    # reads data from supplied doc
    # args:
      # doc - document of html code from supplied web page
      # delay - built-in delay to slow calls to avoid DOS attack mitigation



    # Package manager
    if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
    pacman::p_load_gh("trinker/sentimentr")
    pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)

    sec = 0
    if(delay < 0) warning("delay was less than 0: set to 0")
    if(delay > 0) sec = max(0, delay + runif(1, -1, 1))

    #Remove all white space
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)

    ### identify data to return

    # date should be calculated by scraping arg
    # date <- doc %>%
    #   html_nodes("") %>%
    #   html_text()

    time <- doc %>%
      html_nodes("show-metars") %>%
      html_text()
    dataline <- doc %>%
      html_nodes("wx-value") %>%
      html_text()

    # temp <- doc %>%
    # dewpt <- doc %>%
    # humidity <- doc %>%
    # pressure <- doc %>%
    # visibility <- doc %>%
    # windDir <- doc %>%
    # windSpd <- doc %>%
    # gustSpd <- doc %>%
    # precip <- doc %>%
    # events <- doc %>%
    # conditions <- doc %>%

    title <- doc %>%
      html_nodes("#cm_cr-review_list .a-color-base") %>%
      html_text()

    author <- doc %>%
      html_nodes(".review-byline .author") %>%
      html_text()

    date <- doc %>%
      html_nodes("#cm_cr-review_list .review-date") %>%
      html_text() %>%
      gsub(".*on ", "", .)

    # ver.purchase <- doc%>%
    #   html_nodes(".review-data.a-spacing-mini") %>%
    #   html_text() %>%
    #   grepl("Verified Purchase", .) %>%
    #   as.numeric()

    # format <- doc %>%
    #   html_nodes(".review-data.a-spacing-mini") %>%
    #   html_text() %>%
    #   gsub("Color: |\\|.*|Verified.*", "", .)
    # #if(length(format) == 0) format <- NA

    stars <- doc %>%
      html_nodes("#cm_cr-review_list  .review-rating") %>%
      html_text() %>%
      str_extract("\\d") %>%
      as.numeric()

    comments <- doc %>%
      html_nodes("#cm_cr-review_list .review-text") %>%
      html_text()

    # helpful <- doc %>%
    #   html_nodes(".cr-vote-buttons .a-color-secondary") %>%
    #   html_text() %>%
    #   str_extract("[:digit:]+|One") %>%
    #   gsub("One", "1", .) %>%
    #   as.numeric()

    df <- data.frame(title, author, date, stars, comments, stringsAsFactors = F)

    return(df)
  }



  # Set up holding data frame
  weather_all <- NULL


  # Loop through specified number of pages
  for(date in startDate:endDate){

    # parse day, month, year from date


    # URL of product to be reviewed (specified by prod_code)
    url <- paste0("https://www.wunderground.com/history/airport/KPHL/",year,"/",month,"/",day,
                  "/DailyHistory.html?&reqdb.zip=&reqdb.magic=&reqdb.wmo=")

    # read entire text of given web page
    doc <- read_html(url)

    dateCol <- rep(date, 24)  # create column of 24 'date' for each hour of scraped data

    # use amazon_scraper script to clean and parse the different parts of the review
    weather <- scrapeWeather(doc, delay)        # contains the parsed reviews from page_num
    weather_all <- rbind(weather_all, cbind(dateCol, weather)) # aggregated reviews from all pages
  }

  return(weather_all)
}
