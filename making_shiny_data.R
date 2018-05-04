library(fst)
library(tidyverse)
library(car)
library(janitor)
library(extrafont)
library(shiny)

cces16 <- read.fst("C://cces16.fst")

trad <- cces16 %>% select(V101, evangelical, mainline, bprot, catholic, jewish, other, none) %>%
  gather(reltrad, x1, evangelical:none) %>%
  filter(x1 ==1) %>% select(V101, reltrad) %>%
  left_join(cces16)


trad <- trad %>%
  mutate(reltrad = recode(reltrad, "'evangelical'= 'Evangelical';
                          'mainline'= 'Mainline';
                          'catholic'= 'Catholic';
                          'jewish'= 'Jewish';
                          'bprot' = 'Black Protestant';
                          'other' = 'Other Faith';
                          'none' = 'No Faith'"))

trad <- trad %>% 
  mutate(age = 2016 -birthyr)

trad <- trad %>%
  mutate(vote16 = recode(trad$CC16_410a,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA"))



shiny <- trad %>% select(vote16, reltrad, age)

write.csv(shiny, "shiny.csv")

