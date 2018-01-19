library(tidyverse)
library(extrafont)
library(haven)
library(car)
library(janitor)
library(labelled)
source("D://cces/ggthemes.R")

cces16 <- read_dta("D://cces/data/cces16.dta")

cath_pid <- cces16 %>% 
  filter(religpew == 2) %>% 
  filter(pid7 <8) %>% 
  count(pid7, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(label = c("Catholics"))

all_pid <- cces16 %>% 
  filter(pid7 <8) %>% 
  count(pid7, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(label = c("Entire Sample"))

pid <- bind_rows(cath_pid, all_pid)

pal <- c("darkorchid4", "gray48")

pid %>% 
  ggplot(., aes(x=pid7, y=pct, group = label, fill = label)) + 
  geom_col(position = "dodge", color = "black") + bar_rb() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  labs(x= "Party Identification", y = "Percent of the Population", title = "Are Catholics A Microcosm of the Country?", caption = "Data: CCES 2016")

ggsave(file="D://cces/catholics/pid7.png", type = "cairo-png", width = 18, height = 15)


cath_race <- cces16 %>% 
  filter(religpew == 2) %>% 
  count(race, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(race = to_factor(race)) %>% 
  mutate(label = c("Catholics"))


all_race <- cces16 %>% 
  count(race, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(race = to_factor(race)) %>% 
  mutate(label = c("Entire Sample"))

race <- bind_rows(cath_race, all_race)



ggplot(race, aes(1, pct)) + geom_col(aes(fill= fct_rev(race)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  scale_y_continuous(labels = scales::percent) + facet_grid(label ~ .)  +
  flip_bar_rb() +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  labs(x= "", y = "Percent of the Population", title = "Racial Breakdown of Catholics and Entire Population")  + 
  scale_fill_brewer(palette = "Spectral")

ggsave(file="D://cces/catholics/race.png", type = "cairo-png", width = 18, height = 10)



cath_ab <- cces16 %>% 
  filter(religpew == 2) %>% 
  filter(CC16_332a <3) %>% 
  count(CC16_332a, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(label = c("Catholics")) 


all_ab <- cces16 %>% 
  filter(CC16_332a <3) %>% 
  count(CC16_332a, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(label = c("Entire Sample"))

ab <- bind_rows(cath_ab, all_ab) %>% mutate(issue = c("Pro- Choice"))%>% filter(CC16_332a ==1)

pal <- c("darkorchid4", "gray48")


a1 <- ab %>% 
  filter(CC16_332a ==1) %>% 
  ggplot(., aes(x= CC16_332a, y = pct, group = label, fill = label)) + geom_col(color = "black", position = position_dodge(width=0.45), width=0.4) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  bar_rb() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "", y = "Percent of Sample", title = "Pro-Choice on Abortion")



cath_gay <- cces16 %>% 
  filter(religpew == 2) %>% 
  filter(CC16_335 <3) %>% 
  count(CC16_335, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(label = c("Catholics"))


all_gay <- cces16 %>% 
  filter(CC16_335 <3) %>% 
  count(CC16_335, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(label = c("Entire Sample"))

gay <- bind_rows(cath_gay, all_gay) %>% mutate(issue = c("Pro Gay Marriage")) %>% filter(CC16_335 ==1)

tog <- bind_rows(ab, gay) %>% select(pct, label, issue) 

tog %>% 
  ggplot(., aes(x= label, y = pct, fill = label)) + geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) + 
  bar_rb() +
  labs(x= "", y = "Percent of Sample", title = "Social Issues", caption = "Data: CCES 2016") + facet_grid(.~ issue) + theme(legend.position="none")

ggsave(file="D://cces/catholics/social_issues.png", type = "cairo-png", width = 10, height = 10)


cath_att <- cces16 %>% 
  filter(religpew == 2) %>% 
  filter(pew_churatd <6) %>% 
  count(pew_churatd, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(pew_churatd = to_factor(pew_churatd)) %>% 
  mutate(label = c("Catholics"))

all_att <- cces16 %>% 
  filter(pew_churatd <6) %>% 
  count(pew_churatd, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(pew_churatd = to_factor(pew_churatd)) %>% 
  mutate(label = c("Entire Sample"))

all <- bind_rows(cath_att, all_att)

pal <- c("darkorchid4", "gray48")

all %>% 
  ggplot(., aes(x=pew_churatd, y=pct, group = label, fill = label)) + 
  geom_col(position = "dodge", color = "black") + bar_rb() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  labs(x= "Church Attendance", y = "Percent of the Population", title = "Are Catholics A Microcosm of the Country?", caption = "Data: CCES 2016", subtitle = "Excluding Those Who Never Attend")

ggsave(file="D://cces/catholics/attend_nonever.png", type = "cairo-png", width = 18, height = 15)
