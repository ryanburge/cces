library(janitor)
library(ludbridate)
library(scales)
library(ggplot2)
library(dplyr)

male <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==1 & complete.cases(pew_churatd)) %>%
  group_by(pew_churatd) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Male"))

female <-  cces16 %>% filter(white==1 & evangelical ==1 & complete.cases(vote16) & gender ==2 & complete.cases(pew_churatd)) %>%
  group_by(pew_churatd) %>% count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% filter(vote16 ==1) %>% 
  mutate(label = c("Female"))

total <- bind_rows(male,female) %>% select(-vote16, -n)

total %>% filter(attend !=7) %>% 
  ggplot(., aes(x=fct_rev(attend), y=weight)) + 
  geom_col(aes(fill=label), position = "dodge") + 
  scale_x_discrete(labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Gender Differences Among White Evangelicals", x= "Church Attendance", y= "% of Two Party Vote for Trump", caption = "Date Source: CCES 2016") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + labs(fill="") + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="gender_breakdown.png", type = "cairo-png", width = 20, height =12)

