install.packages("jsonlite")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidytext")
install.packages("textdata")
install.packages("sentimentr")

library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidytext)
library(textdata)
library(sentimentr)


tweet_df <- stream_in(file("twittersample.json"))

custom_tweet_df <- data.frame(id = tweet_df$id, 
                              created_at = tweet_df$created_at, 
                              lang = tweet_df$lang, 
                              text = tweet_df$text, 
                              screen_name = tweet_df$user$screen_name, 
                              name = tweet_df$user$name, 
                              favourites = tweet_df$user$favourites_count, 
                              statuses = tweet_df$user$statuses_count, 
                              followers = tweet_df$user$followers_count, 
                              friends = tweet_df$user$friends_count)

?POSIXct

clean_tweets_df <- custom_tweet_df %>% mutate(created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))


#clean_tweets_df <- clean_tweets_df %>% mutate(text = gsub("http://* | https://*", "", text))


clean_tweets_df <- clean_tweets_df %>% mutate(text = str_remove(text, "http://* | https://*")) %>% 
  mutate(text = str_remove(text, "RT ")) 

words_in_tweets_df <- clean_tweets_df %>% 
  dplyr::select(text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  #anti_join(data.frame(profanity_list) %>%
  filter(!word %in% c("rt", "t.co", profanity_list))

words_in_tweets_df %>% 
  count(word, sort = T) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count", y = "Words",
       title = "Top words tweeted in 2019 Met Gala")

get_sentiments("afinn")

#Afinn sentiment analysis
afinn_word_counts <- words_in_tweets_df %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = if_else(value>0, 'positive', 'negative')) %>%
  count(word, value, sentiment, sort = T) %>%
  ungroup()

afinn_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y = "Sentiment", x = "Words",
       title = "Afinn Sentiment Analysis on Tweets")+
  coord_flip()
  
  profanity_list <- unique(tolower(lexicon::profanity_alvarez))

#Bing Sentiment Analysis
get_sentiments("bing")  
  bing_word_counts <- words_in_tweets_df %>%
    inner_join(get_sentiments(("bing"))) %>%
    count(word, sentiment, sort = T) %>%
    ungroup()

    
  bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word,n)) %>%
    ggplot(aes(word, n, fill = sentiment))+
    geom_col(show.legend = F)+
    facet_wrap(~sentiment, scales = "free_y")+
    labs(y = "Sentiment", x = "Words",
         title = "Bing Sentiment Analysis on Tweets")+
    coord_flip()

#NRC Sentiment Analysis
  get_sentiments("nrc")
  nrc_word_counts <- words_in_tweets_df %>%
    inner_join(get_sentiments(("nrc"))) %>%
    count(word, sentiment, sort = T) %>%
    ungroup()
  
  
  nrc_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word,n)) %>%
    ggplot(aes(word, n, fill = sentiment))+
    geom_col(show.legend = F)+
    facet_wrap(~sentiment, scales = "free_y")+
    labs(y = "Sentiment", x = "Words",
         title = "NRC Sentiment Analysis on Tweets")+
    coord_flip()
