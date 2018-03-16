library(tidyverse)
library(extrafont)
library(haven)
library(car)
library(janitor)
library(labelled)
source("D://cces/ggthemes.R")

cces16 <- read_dta("D://cces/data/cces16.dta")


vote <- cces16 %>% 
  mutate(race2 = recode(race, "1=1; 3=2; else=0")) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  mutate(vote16 = as.numeric(CC16_410a)) %>% 
  mutate(vote16 = Recode(vote16,"1='Trump';
                         2='Clinton';
                         3='Johnson';
                         4='Stein';
                         5= 'Other';
                         6= 'Not Vote';
                         7= 'Not Sure';
                         8= 'McMullin'; else = NA"))  %>% 
  filter(complete.cases(vote16)) %>% 
  filter(race2 !=0) %>% 
  group_by(race2) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(race2) %>% 
  mutate(race2 = as.numeric(race2)) %>% 
  mutate(race2 = recode(race2, "1= 'White Evangelical'; 2 ='Hispanic Evangelical'"))  




vote$vote16 <- factor(vote$vote16, levels = c("Trump", "Clinton", "Johnson", "Other", "Stein", "McMullin", "Not Vote", "Not Sure"))


vote %>% 
  filter(pct > .0025) %>% 
  ggplot(., aes(1, pct)) + geom_col(aes(fill= fct_rev(vote16)), colour = "black") + 
  facet_grid(race2 ~ .)  + 
  coord_flip() +
  scale_fill_manual(values=c("darkgrey", "forestgreen", "purple", "goldenrod1", "dodgerblue3", "firebrick1" )) +
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
  ylab("Percent of Votes Cast") + xlab("") +
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  +  
  scale_y_continuous(labels = scales::percent) +flip_bar_rb()


ggsave(file="D://cces/hispanic/vote16.png", type = "cairo-png", width = 15, height = 10)



  
  
