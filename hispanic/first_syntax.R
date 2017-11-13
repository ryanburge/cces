library(tidyverse)
library(car)
library(haven)
library(janitor)
library(extrafont)


cces16 <- read_dta("D://cces/data/cces16.dta")

cces16 <- cces16 %>% 
  mutate(vote16 = as.numeric(CC16_410a)) %>% 
  mutate(vote16 = recode(vote16, "1='Republican';
                    2='Democrat';
                    3='Libertarian';
                    4='Green';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'McMullin'; else = NA"))

vote16 <- cces16 %>% 
  mutate(race2 = recode(race, "1=1; 3=2; else=0")) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(vote16 == "Republican" | vote16 == "Democrat") %>% 
  filter(complete.cases(vote16)) %>% 
  filter(race2 >0) %>% 
  group_by(race2) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(race2) %>% 
  mutate(race2 = as.numeric(race2)) %>% 
  mutate(race2 = recode(race2, "1= 'White Evangelical'; 2 ='Hispanic Evangelical'")) %>% 
  mutate(year = c(2016)) %>% 
  rename(party = vote16)


vote$vote16 <- factor(vote$vote16, levels = c("Trump", "Clinton", "Johnson", "Other", "Stein", "McMullin", "Not Vote", "Not Sure"))


vote %>% 
  filter(pct > .0025) %>% 
  ggplot(., aes(1, pct)) + geom_col(aes(fill= fct_rev(vote16)), colour = "black") + 
  facet_grid(race2 ~ .)  + 
  coord_flip() +
  scale_fill_manual(values=c("darkgrey", "forestgreen", "purple", "goldenrod1", "dodgerblue3", "firebrick1" )) +
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  # scale_fill_manual(values=c("darkgrey", "forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  +  
  scale_y_continuous(labels = scales::percent)  +  
  theme(plot.title = element_text(face="bold"))

ggsave(file="D://cces/hispanic/vote16.png", type = "cairo-png", width = 15, height = 6)

cces12 <- read_dta("D://cces/data/cces12.dta")

cces12 <- cces12 %>% 
  mutate(vote12 = as.numeric(CC410a)) %>% 
  mutate(vote12 = recode(vote12, "1='Democrat';
                         2='Republican';
                         3='Johnson';
                         4='Stein';
                         5= 'Other';
                         6= 'Not Vote';
                         7= 'Not Sure';
                         8= 'McMullin'; else = NA"))

vote12 <- cces12 %>% 
  mutate(race2 = recode(race, "1=1; 3=2; else=0")) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(vote12)) %>% 
  filter(vote12 == "Republican" | vote12 == "Democrat") %>% 
  filter(race2 >0) %>% 
  group_by(race2) %>% 
  count(vote12, wt = weight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(race2) %>% 
  mutate(race2 = as.numeric(race2)) %>% 
  mutate(race2 = recode(race2, "1= 'White Evangelical'; 2 ='Hispanic Evangelical'")) %>% 
  mutate(year = c(2012)) %>% 
  rename(party = vote12)



cces08 <- read_dta("D://cces/data/cces2008.dta")

cces08 <- cces08 %>% 
  mutate(vote08 = as.numeric(CC410)) %>% 
  mutate(vote08 = recode(vote08, "1='Republican';
                         2='Democrat';
                         3='Johnson';
                         4='Stein';
                         5= 'Other';
                         6= 'Not Vote';
                         7= 'Not Sure';
                         8= 'McMullin'; else = NA"))

vote08 <- cces08 %>% 
  mutate(race2 = recode(V211, "1=1; 3=2; else=0")) %>% 
  filter(V215 ==1 & V219 ==1) %>% 
  filter(complete.cases(vote08)) %>% 
  filter(vote08 == "Republican" | vote08 == "Democrat") %>% 
  filter(race2 >0) %>% 
  group_by(race2) %>% 
  count(vote08, wt = V201) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(race2) %>% 
  mutate(race2 = as.numeric(race2)) %>% 
  mutate(race2 = recode(race2, "1= 'White Evangelical'; 2 ='Hispanic Evangelical'")) %>% 
  mutate(year = c(2008)) %>% 
  rename(party = vote08)

voteplot <- bind_rows(vote08, vote12, vote16) %>% mutate(year = as.factor(year))



voteplot %>% 
  ggplot(., aes(year, pct)) + geom_col(aes(fill= party), colour = "black") + 
  facet_grid(race2 ~ .)  + 
  coord_flip() +
  scale_fill_manual(values=c("dodgerblue3", "firebrick1", "purple", "goldenrod1", "dodgerblue3", "firebrick1" )) +
  theme(axis.title.y = element_blank()) + 
  # theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
  ylab("Percent of Two Party Vote") + 
  theme(legend.position="bottom") +
  ggtitle("Past Three Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + 
  # scale_fill_manual(values=c("darkgrey", "forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  +  
  scale_y_continuous(labels = scales::percent)  +  
  theme(plot.title = element_text(face="bold"))

ggsave(file="D://cces/hispanic/three_elects.png", type = "cairo-png", width = 15, height = 12)



attend <- cces16 %>% 
  mutate(race2 = recode(race, "1=1; 3=2; else=0")) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race2 >0) %>% 
  group_by(age2, race2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  ungroup(race2) %>% 
  mutate(race2 = as.numeric(race2)) %>% 
  mutate(race2 = recode(race2, "1= 'White Evangelical'; 2 ='Hispanic Evangelical'")) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'"))


attend %>% 
  # filter(type == "White Mainline") %>% 
  ggplot(., aes(x = mean, y = age2, group = race2, label = race2))  +
  geom_point(shape=21, size =4, aes(fill = factor(race2)), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(race2)), height=0, size = 1, show.legend = FALSE) + 
  # scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey", "red", "green")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + 
  # scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) + 
  theme(text=element_text(size=42, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Religious Attendance", y ="", title = "Comparing Church Attendance", caption = "Data: CCES 2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))  +  
  theme(plot.title = element_text(face="bold"))  

ggsave(file="D://cces/hispanic/attendance_by_age_ci_facet.png", type = "cairo-png", width = 15, height = 15)