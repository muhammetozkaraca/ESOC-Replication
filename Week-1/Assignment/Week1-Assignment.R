library(tidyverse)
library(ggplot2)
library(lubridate)

data <- list.files(pattern="*.csv")
dataset <- lapply(data, read.csv)

gtd_data <- dataset[[1]] %>%
  unite("Date", "iday":"iyear", remove = TRUE, sep = "-")
gtd_data$Date <- as.Date(gtd_data$Date, "%d-%m-%Y")


ira_data <- dataset[[2]] %>%
  distinct()
ira_data$Date <- as.Date(ira_data$Date, "%Y-%m-%d")


islamist_groups_data <- dataset[[3]] 

df <- tibble(year = c("2015", "2016", "2017", "2018"))
russian_holidays_data <- dataset[[4]] %>%
  unite("date", "Month":"Day", remove = TRUE, sep = "-") %>%
  merge(df) %>%
  unite("Date", c("date","year"), remove = TRUE, sep = "-")

russian_holidays_data$Date <- lubridate::mdy(russian_holidays_data$Date)
russian_holidays_data$holiday <- 1


gtd_data <- gtd_data %>%
  filter(Date > as.Date("2014-12-31")) %>%
  filter(Date < as.Date("2018-7-1")) %>%
  filter(country_txt == "Russia") %>%
  mutate(isl_attack = case_when(
    gname %in% islamist_groups_data$islamist_groups ~ 1,
    TRUE ~ 0
  ))

data_final <- ira_data %>%
  filter(Date < as.Date("2018-7-1")) %>%
  left_join(gtd_data, by = c("Date")) %>%
  left_join(russian_holidays_data, by = c("Date")) %>%
  mutate(event_occured = case_when(
    country_txt == "Russia"  ~ 1,
    TRUE ~ 0
  )) %>%
  select(Date, tweet_count_all, tweet_count_islam, tweet_count_blm, 
         event_occured, isl_attack, holiday, Religious, Public, Political) 

data_final$isl_attack[is.na(data_final$isl_attack)] <- 0
data_final$holiday[is.na(data_final$holiday)] <- 0
data_final$Religious[is.na(data_final$Religious)] <- 0
data_final$Public[is.na(data_final$Public)] <- 0
data_final$Political[is.na(data_final$Political)] <- 0

# How many total observations are there in the final panel? -> 1295
count(data_final)

# On how many days were there a terrorist or islamist terrorist event in Russia?
# terror attack days; -> 121
data_final %>%
  filter(event_occured == 1) %>%
  count()

# isl. terror attack days; -> 36
data_final %>%
  filter(isl_attack == 1) %>%
  count()

# How many holiday days were there in total from January 1 2015 to June 30 2018? -> 75
data_final %>%
  filter(holiday == 1) %>%
  count()

# How many days with public holidays, religious holidays, and political holidays? ->
# religious -> 16, Political -> 0, Public -> 43

data_final %>%
  filter(Religious == 1) %>%
  count()

data_final %>%
  filter(Political == 1) %>%
  count()

data_final %>%
  filter(Public == 1) %>%
  count()





