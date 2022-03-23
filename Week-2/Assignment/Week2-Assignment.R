library(tidyverse)
library(tm)
library(qdap)
library(tidytext)
library(quanteda)
library(lubridate)

setwd("/Users/muhammetozkaraca/Desktop/ESOC-Replication/Week-2/Assignment")
tweet_data <- read.csv("ira_tweets_csv_hashed.csv")

### Part 1 - Pre-processing and Visualizing Twitter Takedown Data

# How many tweets are there in total?
length(tweet_data$tweet_text) -> 1826345

# How many English and non-English language tweets are there?

eng_tweets <- tweet_data %>%
  filter(tweet_language == "en") %>%
  count()
## eng tweets 596227

noneng_tweets <- tweet_data %>%
  filter(tweet_language != "en") %>%
  count()
## non-eng tweets 1230118

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

length(grep(c("Black Lives Matter|BLM"), tweet_data$tweet_text, ignore.case = T)) -> 1578

# How many times do the tweets mention or reply to the official Twitter accounts of 
# either Sputnik News or Russia Today (RT)?

sum(str_detect(tweet_data$tweet_text, "@SputnikInt|@RT_com")) -> 942

### Part 2 - Creating a Document Term Matrix

#### Using only English-language tweets, create a document term matrix with unigrams (single words). 
#### Be sure to:

##### Apply the Porter Stemmer

eng_tweet <- tweet_data %>%
  filter(tweet_language == "en") 

stemmed <- ptstem_words(eng_tweet$tweet_text, algorithm = "porter")

##### Discard punctuation, capitalization, and word order

eng_tweet$tweet_text <- removePunctuation(eng_tweet$tweet_text) # discard punctuation
eng_tweet$tweet_text <- tolower(eng_tweet$tweet_text) # discard capitalization

##### Remove stop words from this list:

words_to_remove <- read.csv("stop-words.csv")
removed_text <- eng_tweet %>%
  unnest_tokens(word, tweet_text) %>%
  filter(!(word %in% words_to_remove$a))


### The below code, which is a replicate for the above steps is taken from the original solution set. 

tweetclean <- function(i){
  text <- i
  text <- gsub("RT", "", text)
  # cleaning
  text <- tolower(text) #convert to lowercase
  text <- gsub("https\\S*|http\\S*", "", text) #remove URLs
  text <- gsub("pic.twitter\\S*", "", text) #remove picture links
  text <- gsub("[^\x01-\x7F]", " ", text) #remove emojis
  text <- gsub("\\b\\d+\\b", "", text) #remove standalone numbers
  text <- gsub("[[:digit:]]+", "", text) #remove all numbers
  text <- gsub("@\\w+ *", "", text) #remove mentions
  text <- gsub ("#\\w+ *", "", text) # remove hashtags
  text <- gsub("&amp", "", text) # remove & from html
  text <- gsub("[[:punct:] ]+", " ", text) # remove all punctuation
  # Porter stemmer
  text <- SnowballC::wordStem(text)
  text
}
eng_tweet$tweet_text <- tweetclean(eng_tweet$tweet_text)

stop_words <- readLines("http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")


##### How many total words appear across all English-language tweets? 95.089
##### Create a figure of the top 20 words and the number of times they appear in the data. 

corpus <- corpus(eng_tweet$tweet_text)
dtm <- dfm(corpus, remove=stop_words, verbose=TRUE)

quanteda.textstats::textstat_frequency(dtm)[1:20] %>%
  ggplot(aes(x = reorder(feature,frequency), y = frequency, label = frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  geom_text(nudge_y=200, size = 3)


##### Also create a word cloud of the top terms. Do the same for the top hashtags used.

textplot_wordcloud(dtm, rotation=0.25, min_size=.75, max_size=3,max_words=1000,)

##### Do the same for the top hashtags used.

hashtags_corpus <- corpus(eng_tweet$hashtags)
hashtags_dtm <- dfm(tags_corpus, remove = c("[", "]", ",", "'", "#", ""))

quanteda.textstats::textstat_frequency(hashtags_dtm)[1:20] %>%
  ggplot(aes(x = reorder(feature,frequency), y = frequency, label = frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  geom_text(nudge_y=200, size = 3)


textplot_wordcloud(hashtags_dtm, rotation=0.25, min_size=.75, max_size=3,max_words=1000,)

### Part 3 - Dictionary Methods

##### Load the following lists of positive and negative words from Neal Caren:
###### https://raw.githubusercontent.com/nealcaren/quant-text-fall-2014/master/positive.csv
###### https://raw.githubusercontent.com/nealcaren/quant-text-fall-2014/master/negative.csv

positive <- readLines("https://raw.githubusercontent.com/nealcaren/quant-text-fall-2014/master/positive.csv")
negative <- readLines("https://raw.githubusercontent.com/nealcaren/quant-text-fall-2014/master/negative.csv")

df$positive <- rowSums(dtm[,which(featnames(dtm) %in% pos_words)])
df$negative <- rowSums(dtm[,which(featnames(dtm) %in% neg_words)])
df$pos_neg_ratio <- df$positive - df$negative

positive_tweets <- eng_tweet %>%
  filter(tweet_text %in% positive)


####### Count the number of positive and negative words for each tweet using these dictionaries, as
####### well as the difference between the two scores. Report the summary statistic of 
####### the sentiment of IRA tweets.

eng_tweet$positive_tweets <- rowSums(dtm[,which(featnames(dtm) %in% positive)])
eng_tweet$negative_tweets <- rowSums(dtm[,which(featnames(dtm) %in% negative)])
eng_tweet$pos_neg_ratio <- eng_tweet$positive_tweets - eng_tweet$negative_tweets

eng_tweet %>%
  select(positive_tweets, negative_tweets, pos_neg_ratio) %>%
  summary()

####### Plot a time-series of the average tweet sentiment per month.

eng_tweet$tweet_time <- as.Date(eng_tweet$tweet_time)

eng_tweet %>%
  group_by(month=floor_date(tweet_time, "month")) %>%
  mutate(average_sentiment = mean(pos_neg_ratio)) %>%
  select(month, average_sentiment) %>%
  distinct() %>%
  right_join(all_months) %>%
  replace_na(list(average_sentiment = 0)) %>%
  ggplot(aes(x=month, y = average_sentiment)) +
  geom_line() +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


### Part 4 - Topic Models
####### a) What is Latent Dirichlet Allocation (LDA) and how does it work?

####### b) Identify the 300 most common unigrams and create a N x 300 document term matrix where 
####### the columns count the unigrams and the rows are the tweets. Run a LDA on this DTM and 
####### print the top 10 words for each topic.

topwords <- quanteda.textstats::textstat_frequency(dtm)
dtm <- dtm[,which(featnames(dtm) %in% topwords$feature[1:300])]
dtm <- dtm[rowSums(dtm)>0,]
K <- 10
topicModel <- topicmodels::LDA(dtm, K, control=list(seed = pi))
topicmodels::terms(topicModel, 10)

####### c) What is your best guess of the different topics that appear in the tweets pushed out by 
####### IRA trolls based on the frequent words?