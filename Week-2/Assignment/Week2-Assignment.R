library(tidyverse)

setwd("/Users/muhammetozkaraca/Desktop/ESOC-Replication/Week-2/Assignment")
tweet_data <- read.csv("ira_tweets_csv_hashed.csv")
colnames(tweet_data)

# How many tweets are there in total?
length(tweet_data$tweet_text) -> 1826345

# How many English and non-English language tweets are there?
eng_tweets <- tweet_data %>%
  filter(tweet_language == "en") %>%
  count()

noneng_tweets <- tweet_data %>%
  filter(tweet_language != "en") %>%
  count()

# How many users have self-reported locations? What are the top 5 self-reported 
# locations among these accounts?
user_location <- tweet_data %>%
  filter(user_reported_location != "")

length(user_location$user_reported_location) -> 1503154

top_5 <- user_location %>%
  group_by(user_reported_location) %>%
  summarise(count = n())

top_5[order(top_5$count, decreasing = TRUE),]  

# How many times do the words “Black Lives Matter” or “BLM” appear in the tweets?
library(stringr)
tweet_data %>%
  str_detect("Black Lives Matter", tweet_data$tweet_text)
  filter(tweet_text %in% "Black Lives Matter")

  str_contains("hello", "Hel")
  
# How many times do the tweets mention or reply to the official Twitter accounts of either Sputnik News or Russia Today (RT)?




