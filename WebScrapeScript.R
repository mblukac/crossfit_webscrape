### Crossfit Web Scraping Project ###
### Source tutorial: https://www.datacamp.com/community/tutorials/r-web-scraping-rvest?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com

# General-purpose data wrangling
library(dplyr)
#install.packages("tidyr")
library(tidyr)
# Parsing of HTML/XML files  
#install.packages("rvest")
library(rvest)    
# String manipulation
#install.packages("stringr")
library(stringr)   
# Verbose regular expressions
#install.packages("rebus")
library(rebus)     
# Eases DateTime manipulation
#install.packages("lubridate")
library(lubridate)
# Text mining package
#install.packages("tm")
library(tm)
# Graphics ftw
#install.packages("ggplot2")
library(ggplot2)
# date manipulation
#install.package("zoo")
library(zoo)



# Webscraping Function -----------------------------------------------------------------------------
get_wods <- function(date_start, date_end){
  url <- "https://crossfit.com/workout/"
  
  a <- seq(as.Date(date_start), as.Date(date_end), "days")
  a <- as.data.frame(a)
  a <- separate(a, a, c("year", "month", "day"), sep = "-")
  a$new_url <- paste0(url, a$year, "/", a$month, "/", a$day, sep = "")
  
  wod <- list()
  html <- list()
  doc_id <- c()
  wod_text <- list()
  for(i in 1:length(a$new_url)){
    html[[i]] <- read_html(a$new_url[i])
    wod[[i]] <- html_nodes(html[[i]], xpath = paste0("//*[@id=\"", a$year[i], a$month[i], a$day[i], 
                                                     "\"]/div/div[2]/div", sep = ""))
    wod_text[[i]] <- html_text(wod[[i]])
    doc_id[i] <- i
  }
  
  wod_text <- do.call(rbind.data.frame, wod_text)
  wod_text <- cbind(wod_text, a$year, a$month, a$day)
  
  wod_data <- wod_text[, c(1, 3, 4, 5)]
  names(wod_data) <- c("text", "year", "month", "day")
  wod_data <- cbind(doc_id, wod_data)
  
  wod_data$text <- as.character(wod_data$text)
  return(wod_data)
}


wods <- get_wods(date_start = "2013/01/01", date_end = "2018/01/01")

# Data Cleaning -----------------------------------------------------------------------------------
clean_corpus <- function(corpus){
  corpus <- tolower(corpus)
  corpus <- removePunctuation(corpus)
  corpus <- removeNumbers(corpus)
  corpus <- removeWords(corpus, c(stopwords("en")))
  corpus <- stripWhitespace(corpus)
  return(corpus)
}
clean_wods <- wods
clean_wods$text <- clean_corpus(clean_wods$text)




# Exercises -------------------------------------------------------------------------------------------
rest <- mapply(grepl, "rest day", clean_wods$text)
run <- mapply(grepl, "run", clean_wods$text)
row <- mapply(grepl, "row", clean_wods$text)
bike <- mapply(grepl, "bike", clean_wods$text)
rope <- mapply(grepl, "unders", clean_wods$text) 

wods_classified <- cbind(clean_wods, 
                   rest = as.numeric(rest), 
                   run = as.numeric(run),
                   row = as.numeric(row),
                   bike = as.numeric(bike),
                   rope = as.numeric(rope))

wods_classified$monthf <- factor(wods_classified$month, labels=c("Jan","Feb","Mar","Apr","May","Jun",
                                                                 "Jul","Aug","Sep","Oct","Nov","Dec"),
                                 ordered=TRUE)
wods_classified$dmy <- dmy(paste0(wods_classified$day, "-", wods_classified$month, "-", wods_classified$year))
wods_classified$weekday <- as.POSIXlt(wods_classified$dmy)$wday
wods_classified$weekday <- factor(wods_classified$weekday,
                                  levels=rev(0:6),
                                  labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
                                  ordered=TRUE)
wods_classified$yearmonth <- as.yearmon(wods_classified$dmy)
wods_classified$yearmonth <- factor(wods_classified$yearmonth)

wods_classified$week <- as.numeric(format(wods_classified$dmy,"%U"))

wods_cleaned <- ddply(wods_classified,.(yearmonth), transform, monthweek = 1 + week - min(week))
wods_cleaned$cardio <- wods_cleaned$run + wods_cleaned$row + wods_cleaned$bike + wods_cleaned$rope

# Visualization of Cardio --------------------------------------------------------------------------------
## Calendar Example : https://www.r-bloggers.com/ggplot2-time-series-heatmaps/
wods_cleaned %>%
  filter(year %in% c("2013", "2014", "2015", "2016", "2017")) %>%
    ggplot(aes(x = monthweek, y = weekday, fill = cardio)) + 
      geom_tile(colour = "white") + 
      facet_grid(year ~ monthf) + 
      scale_fill_gradient(low = "yellow", high = "red") +
      ggtitle("Crossfit: Cardio Exercise") + 
      xlab("Week of Month") + 
      ylab("") +
      scale_x_continuous(breaks = c(1, 3, 5)) + 
      theme_classic() +
      theme(
        legend.position = "none"
      )
ggsave("Heatmap_CardioCalendar.png", width = 10)

