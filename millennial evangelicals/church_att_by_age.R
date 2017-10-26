library(tidyverse)
library(car)
library(haven)
library(janitor)

cces16 <- read_dta("D://cces/data/cces16.dta")

attend <- cces16 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:100 =6")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 

attend <- attend %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65 and Over'"))
  


attend %>% 
  ggplot(., aes(x = mean, y = age2))  +
  geom_point(shape=21, size =4, aes(fill = factor(age2)), show.legend = FALSE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper), height=.125, size = 1) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF")) + 
  theme(text=element_text(size=28, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Religious Attendance", y ="", title = "Average Church Attendance of White, Born Again Protestants", caption = "Data: CCES 2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Few Times A Year", "Monthly", "Weekly", "Weekly+")) 


ggsave(file="attendance_by_age_ci.png", type = "cairo-png", width = 15, height = 10)
