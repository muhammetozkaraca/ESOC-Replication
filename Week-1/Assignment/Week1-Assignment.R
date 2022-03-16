library(tidyverse)
library(ggplot2)
library(lubridate)


setwd("/Users/muhammetozkaraca/Desktop/ESOC-Replication/Week-1/Assignment/Week1_csvfiles")

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

# Write a function that calculates descriptive statistics (N, mean, min, median, max, and S.D.). 
# Then, loop over each of the three tweet count variables in the data to generate those descriptives and
# output a 3 x 7 dataframe of these with the variable name.


my.summary <- function(x,...){
  c(N=length(x),
    mean=mean(x, ...),
    min=min(x, ...),
    median=median(x, ...),
    max=max(x,...), 
    S.D. = sd(x,...))
}

summary_df <- as.data.frame(apply(data_final[2:4], 2, my.summary)) # note that the second argument is called margin and denotes whether the manipulation is performed on rows or columns. For rows, it is 1, and for columns, it is 2.
summary_df


# Try different plots to visualize the distribution of each tweet count variable. 
# Which type of graph is most effective in your opinion, and why?


tweets <- data_final[, 1:4] %>%
  pivot_longer(tweet_count_all:tweet_count_blm, names_to = "Tweets", values_to = "Count")

ggplot(tweets, aes(x = Tweets, y = Count, color = Tweets)) + 
  geom_boxplot() +
  coord_flip() +
  theme_minimal()


# What do you notice about the distribution of these variables? Can you think of
# any problems that this may cause for analysis? What could you do to remedy these
# problems? Try them and report on whether they worked.


# Create a time-series plot of these tweet count variables in a way that you think is
# meaningful. Explain what considerations you thought through and what the figure shows.

ggplot(tweets, aes(x = Date, y = Count, color = Tweets)) + 
  geom_line() +
  theme_minimal()






