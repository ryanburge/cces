
library(tidyverse)
library(janitor)
library(extrafont)
library(wordcloud2)

count <- read_csv("D://cces/freeresponse/freecount.csv")

count %>% 
  ggplot(., aes(x=reorder(relig, -count), y=count)) + geom_col(fill = "hotpink4", color = "black") +
  labs(x = "Religious Tradition Categories", y = "'Something Else' Instances", title = "What Traditions are the Most Likely to Choose 'Something Else'?", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=32, family="KerkisSans")) +  
  theme(plot.title = element_text(face="bold")) 


ggsave(file="D:/cces/freeresponse/general_bar_graph.png", type = "cairo-png", width = 17, height = 12)


other <- read_csv("D://cces/freeresponse/baptist.csv")

baptist <- other %>% filter(denom == "Baptist") %>% arrange(-count) %>% rename(word = relig, freq = count) %>%  select(-denom) %>% filter(freq < 10)

wordcloud2(data = baptist)

nd <- other %>% filter(denom == "ND") %>% arrange(-count) %>% rename(word = relig, freq = count) %>%  select(-denom) 

wordcloud2(data = nd)

luth <- other %>% filter(denom == "Lutheran") %>% arrange(-count) %>% rename(word = relig, freq = count) %>%  select(-denom) %>% filter(freq < 18)

wordcloud2(data = luth)


meth <- other %>% filter(denom == "Methodist") %>% arrange(-count) %>% rename(word = relig, freq = count) %>%  select(-denom) %>% filter(freq < 10)

wordcloud2(data = meth)


pent <- other %>% filter(denom == "Pentecostal") %>% arrange(-count) %>% rename(word = relig, freq = count) %>%  select(-denom) %>% filter(freq < 10)

wordcloud2(data = pent)