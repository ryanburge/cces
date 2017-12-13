library(rtweet)
library(tidytext)
library(tidyverse)
library(wordcloud2)

appname <- "rtweet_tokens_22"

## api key (example below is not a real key)
key <- "1JLTAtjvVI4b8fDTMo06Xrmvh"

## api secret (example below is not a real key)
secret <- "j8DhqlciCUNRD8g4CeLbPTiiouvSpbWV8kzYvUMXESrF3lIuG3"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)


rt <- search_tweets(
  "#rstats", n = 180, include_rts = FALSE
)

rt2 <- rt %>% select(status_id, created_at, user_id, screen_name, text, source, reply_to_status_id, reply_to_user_id, reply_to_screen_name, favorite_count, retweet_count)

tidy <- rt2 %>% 
  unnest_tokens(word, text)

cleaned <- tidy %>%
  anti_join(stopwordslangs)

count <- cleaned %>% count(word, sort = TRUE)

wordcloud2(count)