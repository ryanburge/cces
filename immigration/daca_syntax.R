library(wordcloud)
library(wordcloud2)
library(tidytext)
library(janitor)
library(tidyverse)
library(extrafont)

daca <- read_csv("https://raw.githubusercontent.com/ryanburge/cces/master/immigration/daca.csv") %>% clean_names()

## Simple Word Cloud

tidy <- daca %>% unnest_tokens(word, statement_content)
tidy <- tidy %>% count(word, sort = TRUE)
tidy <- tidy %>% anti_join(stop_words)
tidy <- tidy %>% arrange(-n) %>% na.omit()
wordcloud2(tidy)

## Trump Mentions 

tidy <- daca %>% unnest_tokens(word, statement_content)
trump <- tidy %>% filter(word == "trump" | word == "Trump")


tgraph <- trump %>% group_by(religious_body) %>% count() 

ggplot(tgraph, aes(x=reorder(religious_body,n), y=n )) + 
  geom_col(fill = "firebrick4", color = "black") + coord_flip() +
  labs(y= "Number of Mentions", x ="Religious Group" , title = "Which DACA Statements Mention Trump by Name?") +
  flip_bar_rb() + theme(plot.title = element_text(family = "Product Sans", size = 40, vjust =0, face = "bold"))

ggsave(file="daca_trump_mentions_new.png", type = "cairo-png", width = 21, height = 12)

## Mapping Sentiment Bar Chart
sentiment <- tidy %>% group_by(religious_body) %>% inner_join(get_sentiments("afinn")) %>% select(religious_body, word, score)
sengraph <- sentiment %>% group_by(religious_body) %>% summarise(total = sum(score)) %>% arrange(total)

sengraph %>% 
  mutate(sentiment = ifelse(total < -1, "Negative", "Positive")) %>% 
  ggplot(., aes(x=reorder(religious_body, total), y = total, fill = sentiment )) + geom_col(color = "black")+ coord_flip() +   scale_fill_manual(values=c("firebrick1", "dodgerblue1")) +
  labs(y= "Overall Sentiment Score", x ="Religious Group" , title = "Which DACA Statements Were the Most Positive?") +
  flip_bar_rb() + theme(plot.title = element_text(family = "Product Sans", size = 40, vjust =0, face = "bold")) + theme(axis.text.y = element_text(family = "Product Sans", size =24, hjust = 1))

ggsave(file="daca_sentiment_group_new.png", type = "cairo-png", width = 20, height = 12)

## Comparison Word Cloud
tidy %>% inner_join(bing) %>% 
  filter(word != "trump") %>% 
  count(word, sentiment) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("firebrick1", "dodgerblue1"),
                   max.words = 100)


t <- read_csv("https://raw.githubusercontent.com/ryanburge/cces/master/immigration/trump.csv") %>% clean_names()

t <- t %>% unnest_tokens(word, statement_content)
t <- t %>% count(word, sort = TRUE)
t <- t %>% anti_join(stop_words)
t <- t %>% arrange(-n) %>% na.omit()


nt <- read_csv("https://raw.githubusercontent.com/ryanburge/cces/master/immigration/notrump.csv") %>% clean_names()

nt <- nt %>% unnest_tokens(word, statement_content)
nt <- nt %>% count(word, sort = TRUE)
nt <- nt %>% anti_join(stop_words)
nt <- nt %>% arrange(-n) %>% na.omit()
