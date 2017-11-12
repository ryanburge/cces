library(tidyverse)
library(car)
library(haven)
library(janitor)
library(extrafont)


cces16 <- read_dta("D://cces/data/cces16.dta")

a1 <- cces16 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  mutate(weekly = recode(attend, "5:6=1; else=0")) %>% 
  group_by(age2) %>% 
  count(weekly, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(weekly ==1) %>% 
  mutate(group = c("Entire Sample"))

a2 <- cces16 %>% mutate(age = 2017 - birthyr) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  mutate(weekly = recode(attend, "5:6=1; else=0")) %>% 
  group_by(age2) %>% 
  count(weekly, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(weekly ==1) %>% 
  mutate(group = c("Evangelicals"))

a3 <- cces16 %>% mutate(age = 2017 - birthyr) %>% 
  filter(pew_bornagain ==2 & religpew ==1) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  mutate(weekly = recode(attend, "5:6=1; else=0")) %>% 
  group_by(age2) %>% 
  count(weekly, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(weekly ==1) %>% 
  mutate(group = c("Mainline"))

a4 <- cces16 %>% mutate(age = 2017 - birthyr) %>% 
  filter(religpew ==2) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  mutate(weekly = recode(attend, "5:6=1; else=0")) %>% 
  group_by(age2) %>% 
  count(weekly, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(weekly ==1) %>% 
  mutate(group = c("Catholics"))
 
bar <- bind_rows(a1, a2, a3, a4) %>% select(age2, pct, group) %>% ungroup(age2) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'"))

ggplot(bar,aes(x=group, y=pct, fill = factor(age2))) + 
  geom_col(aes(fill=age2), color = "black", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")  +
  labs(x="Religious Tradition", y="Percentage of each Tradition", title="Who Attends Church Weekly?", caption="Data from CCES 2016") + 
  theme(legend.position="bottom") + labs(fill="") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=40, family="KerkisSans")) + 
  theme(plot.title = element_text(face="bold")) 

ggsave(file="D://lifecycle/bar_graph_weekly.png", type = "cairo-png", width = 15, height = 15)

