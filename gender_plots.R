library(janitor)
library(ludbridate)
library(scales)
library(ggplot2)
library(dplyr)

mvote <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==1) %>%
  count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(label = c("Male"))

fvote <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==2) %>%
  count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>%  
  mutate(label = c("Female"))

vote <- bind_rows(mvote,fvote) %>% select(vote16, weight, label)

vote$vote16<-Recode(vote$vote16,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA")

vote$vote16 <- factor(vote$vote16, levels=unique(vote$vote16))



vote %>% filter(vote16 == "Donald Trump" | vote16 == "Hillary Clinton" | vote16 == "Gary Johnson" | vote16 == "Jill Stein") %>% ggplot(., aes(1, weight)) + geom_col(aes(fill=fct_rev(vote16)), colour = "black") + coord_flip() + facet_grid(label ~.)  +
  theme(legend.position = "bottom") + guides(fill = guide_legend(reverse=TRUE)) +
  scale_x_discrete(labels = c("")) + xlab("") + 
  ylab("Percent of Sample") + theme(legend.title=element_blank())  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + ggtitle("Gender Differences in White Evangelical Protestants") +
  labs(caption = "Data from CCES 2016") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("chartreuse1", "gold1", "dodgerblue3", "firebrick1"))

ggsave(file="gender_breakdown.png", type = "cairo-png", width = 20, height =12)



## Broken Down by Attendance
male <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==1 & complete.cases(pew_churatd)) %>%
  group_by(pew_churatd) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Male"))

female <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==2 & complete.cases(pew_churatd)) %>%
  group_by(pew_churatd) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Female"))

total <- bind_rows(male,female) %>% select(-vote16, -n)

total$attend <- as.factor(total$pew_churatd)

total %>% filter(attend !=7) %>% 
  ggplot(., aes(x=fct_rev(attend), y=weight)) + 
  geom_col(aes(fill=label), position = "dodge") + 
  scale_x_discrete(labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Gender Differences Among White Evangelicals", x= "Church Attendance", y= "% of Two Party Vote for Trump", caption = "Date Source: CCES 2016") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + labs(fill="") + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="gender_breakdown_attend.png", type = "cairo-png", width = 20, height =12)


## Breakdown by Education

male <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==1 & complete.cases(educ)) %>%
  group_by(educ) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Male"))

female <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==2 & complete.cases(educ)) %>%
  group_by(educ) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Female"))

total <- bind_rows(male,female) %>% select(-vote16, -n)

total$educ <- as.factor(total$educ)


total %>%  
  ggplot(., aes(x=educ, y=weight)) + 
  geom_col(aes(fill=label), position = "dodge") + 
  scale_x_discrete(labels = c("Less than HS", "HS Graduate", "Some College", "2 Yr Degree", "4 Yr Degree", "Postgraduate")) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Gender Differences Among White Evangelicals", x= "Education", y= "% of Two Party Vote for Trump", caption = "Date Source: CCES 2016") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + labs(fill="") + 
  theme(text=element_text(size=24, family="KerkisSans"))

ggsave(file="gender_breakdown_educ.png", type = "cairo-png", width = 20, height =12)

## Age Breakdown

cces16 <- cces16 %>% mutate(age = 2017-birthyr)
cces16$agecut <- cut(cces16$age, breaks =5)


male <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==1 & complete.cases(agecut)) %>%
  group_by(agecut) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Male"))


female <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==2 & complete.cases(agecut)) %>%
  group_by(agecut) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Female"))

total <- bind_rows(male,female) %>% select(-vote16, -n)

total$agecut <- as.factor(total$agecut)


total %>%  
  ggplot(., aes(x=agecut, y=weight)) + 
  geom_col(aes(fill=label), position = "dodge") + 
  scale_x_discrete(labels = c("18-35", "36-51", "52-67", "68-83", "84-100")) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Gender Differences Among White Evangelicals", x= "Age", y= "% of Two Party Vote for Trump", caption = "Date Source: CCES 2016") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + labs(fill="") + 
  theme(text=element_text(size=24, family="KerkisSans"))

ggsave(file="gender_breakdown_age.png", type = "cairo-png", width = 20, height =12)

## Marital Status Breakdown



male <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==1 & complete.cases(marstat)) %>%
  group_by(marstat) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Male"))


female <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==2 & complete.cases(marstat)) %>%
  group_by(marstat) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Female"))

total <- bind_rows(male,female) %>% select(-vote16, -n)

total$marstat <- as.factor(total$marstat)

total %>%  
  ggplot(., aes(x=marstat, y=weight)) + 
  geom_col(aes(fill=label), position = "dodge") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete(labels = c("Married", "Separated", "Divorced", "Widowed", "Single", "Domestic Partner")) +
  labs(title = "Gender Differences Among White Evangelicals", x= "Marital Status", y= "% of Two Party Vote for Trump", caption = "Date Source: CCES 2016") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + labs(fill="") + 
  theme(text=element_text(size=24, family="KerkisSans"))

ggsave(file="gender_breakdown_married.png", type = "cairo-png", width = 20, height =12)

