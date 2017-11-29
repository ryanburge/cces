
library(tidyverse)
library(extrafont)
library(haven)
library(car)
library(janitor)
library(labelled)
source("D://cces/ggthemes.R")


cces16 <- read_dta("D://cces/data/cces16.dta")

john <- cces16 %>% 
  filter(CC16_410a ==3) %>% 
  count(religpew, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n))  %>% 
  mutate(type = c("Johnson Voters")) %>% 
  mutate(religpew = to_factor(religpew))
  

all <- cces16 %>% count(religpew, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("All Respondents"))  %>% 
  mutate(religpew = to_factor(religpew))

graph <- bind_rows(john, all) %>% select(-n) 

Palette <- c("gray48", "goldenrod1")

graph %>% 
  filter(pct > .01) %>% 
  ggplot(.,aes(x=reorder(religpew, pct), y=pct, fill = type)) + 
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = scales::percent) + coord_flip() +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values = Palette) +
  labs(x= "Religious Tradition", y = "Percent of Each Sample", title = "Are Libertarian Voters Different than the Population?", caption = "Data: CCES 2016", subtitle = "1,829 Johnson Voters (3.4% of the Sample)")+ theme_rb()

ggsave(file="lib_voters.png", type = "cairo-png", width = 20, height =12)

