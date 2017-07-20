library(tidyverse)
library(car)
library(forcats)
library(extrafont)

cces16 <- read_dta("D:/cces/data/cces.dta")

cces16 %>% 
  mutate(attend = 7 - pew_churatd) %>% 
  group_by(educ) %>% 
  count(attend) %>% 
  na.omit() %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(attend >0) %>% 
  mutate(ed = factor(educ), attend = factor(attend)) %>% 
  mutate(attend = recode(attend, "0= 'Do not Know';
                                        1= 'Never';
                                        2= 'Seldom';
                                        3= 'Yearly';
                                        4= 'Monthly';
                                        5= 'Weekly';
                                        6= 'More than Weekly'")) %>% 
  mutate(ed = recode(ed, "1= 'Less than HS';
                              2='HS Grad'; 
                              3= 'Some College'; 
                              4= 'Associates'; 
                              5= 'Bachelors';
                              6= 'Graduate'")) %>% 
  ungroup(educ) %>% 
  mutate(attend = fct_inorder(attend), ed = fct_inorder(ed)) %>% 
ggplot(., aes(x=ed, y = weight, fill = attend)) + geom_col(position = "dodge") +  
  theme(axis.title.y = element_blank()) + 
  ylab("Percent of Votes Cast") + xlab("Level of Education") +
  theme(legend.position="bottom") +
  ggtitle("Educational Level and Church Attendance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))  + 
  scale_y_continuous(labels = scales::percent) + theme(legend.title=element_blank()) 

ggsave(file="educ_attend.png", type = "cairo-png", width = 24, height =12)
