library(tidyverse)
library(qdap)
library(quanteda)
library(lubridate)

setwd("/Users/muhammetozkaraca/Desktop/ESOC-Replication/Week-3/Assignment")
tweet_data <- read.csv("ira_tweets_csv_hashed.csv")
tweet_data$tweet_time <- as.Date(tweet_data$tweet_time)

### Exercise 1

# Create a panel of the following variables at the day level (English language tweets only):

tweet_eng <- tweet_data[tweet_data$tweet_language == "en", ]
tweet_eng$tweet_time <- as.Date(tweet_eng$tweet_time)
tweet_eng$blm <- as.numeric(str_detect(tweet_eng$tweet_text, regex("BLM|Black Lives Matter", 
                                                                   ignore_case = T)))

df$blm_tweet <- as.numeric(str_detect(df$tweet_text, regex('Black Lives Matter|BLM', ignore_case = T)))

## Number of tweets & Average engagement metrics (replies, likes, quotes, and retweets)
## Number of tweets mentioning either “BLM” or “Black Lives Matter”

panel_data <- tweet_eng %>%
  group_by(tweet_time) %>%
  summarise(
    n = n(),
    quote_mean = mean(quote_count, na.rm = TRUE),
    reply_mean = mean(reply_count, na.rm = TRUE),
    like_mean = mean(like_count, na.rm = TRUE),
    retweet_mean = mean(retweet_count, na.rm = TRUE),
    blm_tweets = sum(blm)) %>%
  arrange(tweet_time) %>%
  as.data.frame()

## How many observations do you have?
nrow(panel_data)

panel_balanced <- panel_data %>%
  right_join(data.frame(tweet_time = seq(min(tweet_data$tweet_time), max(tweet_data$tweet_time), "day")))

panel_balanced[is.na(panel_balanced)] <- 0
panel_balanced <- panel_balanced[order(panel_balanced$tweet_time),]

nrow(panel_balanced)

## Run the following event study:

"
Yt = B0 + β1T + β2Xt + β3XtTt + ϵ

where Yt is the outcome variable at time t, Xt is an indicator that equals 0 during
the pre-treatment period and 1 post-treatment, and XtTt is an interaction term. Set
the pre/post window as 30 days. Your code should loop over each of the 3 events as
well as each of the 6 variables in your tweets dataset (tweet count, four engagement
metrics, tweets about BLM). Try not to use a pre-made package to run this model,
but rather set up each variable yourself.
"

events <- data.frame(event_date = c("2015-08-19", "2015-07-13", "2016-07-05"))

events$event_date <- as.Date(events$event_date)

panel_balanced$Date <- panel_balanced$tweet_time

# create function to loop over each event:
time_window <- 30
for(i in 1:3){
  # set event date
  event_date <- events$event_date[i]
  # subset by if date is within 14 days of event
  ddf <- panel_balanced
  ddf$diff <- ddf$Date - event_date
  ddf$window <- ifelse(((ddf$diff < time_window)&(ddf$diff >= -time_window)), 1,0)
  ddf <- filter(ddf, window == 1)
  # add the needed variables to run our model:
  ddf$T <- 1:nrow(ddf)
  ddf$X <- ifelse(ddf$diff >= 0,1,0)
  ddf$XT <- ifelse(ddf$diff >= 0, 1:nrow(ddf)/2, 0)
  # loop over every variable
  # and print summary
  for(x in 2:7){
    model <- lm(paste(colnames(ddf)[x], " ~ T + X + XT"), data=ddf)
    print(paste(colnames(ddf)[x]))
    print(summary(model))
  }
}

# As the replication data could not be accessed, the second part of the exercise was not completed. 



















