library(tidyverse)
library(car)
library(haven)

cces16 <- read_dta("D://cces/data/cces16.dta")


cces16$CC16_410a <- as.numeric(cces16$CC16_410a)
cces16$vote16<-Recode(cces16$CC16_410a,"1='Trump';
                    2='Clinton';
                    3='Johnson';
                    4='Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'McMullin'; else = NA")


vote <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "1:35 =1 ; else =0")) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(vote16)) %>% 
  group_by(age2) %>%
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = 'Under 35';
                       0= 'Over 35'")) %>% 
  mutate(vote16 = as_factor(vote16))

vote$age2_f <- factor(vote$age2, levels = c('Under 35', 'Over 35'))
vote$vote16 <- fct_relevel(vote$vote16, "Trump", "Clinton", "Johnson", "Other", "McMullin", "Stein", "Not Vote", "Not Sure")

vote %>% 
  filter(weight > .01) %>% 
  ggplot(., aes(1, weight)) + geom_col(aes(fill= fct_rev(vote16)), colour = "black") + coord_flip() +
  theme(axis.title.y = element_blank())  + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  labs(x= "Vote Choice in 2016", y="Percent of Votes Cast", title = "White, Born Again Protestants - Millennial vs. All Other Ages") + 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c( "darkgrey","purple", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  # scale_fill_manual(values=c("firebrick1", "darkgrey", "goldenrod1", "dodgerblue3", "forestgreen",  "gray", "pink", "purple")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(age2_f ~ .)  +  
  scale_y_continuous(labels = scales::percent) +  
  theme(plot.title = element_text(face="bold")) 

ggsave(file="vote16_millennials.png", type = "cairo-png", width = 15, height = 6)


cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "1:35 =1 ; else =0")) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_332a)) %>% 
  group_by(age2) %>%
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(abort = as.numeric(CC16_332a)) %>% 
  filter(abort == 1) %>%
  mutate(abort = recode(abort, "1= 'Pro-Choice';
                         2= 'Pro-Life'")) %>% 
  mutate(age2 = recode(age2, "1 = 'Under 35';
                       0= 'Over 35'")) %>% 
  mutate(issue = c("Abortion")) %>% 
  select(age2, abort, weight) 


abunder <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age <= 35) %>% 
  filter(CC16_410a <=2) %>% 
  group_by(CC16_410a) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_332a)) %>% 
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(vote = as.numeric(CC16_410a), abort = as.numeric(CC16_332a)) %>% 
  mutate(vote = recode(vote, "1= 'Trump';
                       2= 'Clinton'")) %>% 
  mutate(abort = recode(abort, "1= 'Pro-Choice';
                        2= 'Pro-Life'")) %>% 
  ungroup(CC16_410a) %>% 
  mutate(label = c("Under 35")) %>% 
  select(vote, abort, weight, label)

abover <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age > 35) %>% 
  filter(CC16_410a <=2) %>% 
  group_by(CC16_410a) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>%
  filter(complete.cases(CC16_332a)) %>% 
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(vote = as.numeric(CC16_410a), abort = as.numeric(CC16_332a)) %>% 
  mutate(vote = recode(vote, "1= 'Trump';
                       2= 'Clinton'")) %>% 
  mutate(abort = recode(abort, "1= 'Pro-Choice';
                        2= 'Pro-Life'")) %>% 
  ungroup(CC16_410a) %>% 
  mutate(label = c("Over 35")) %>% 
  select(vote, abort, weight, label)

abort <- bind_rows(abunder, abover)

cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "1:35 =1 ; else =0")) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  group_by(age2) %>% 
  mutate(gay = as.numeric(CC16_335)) %>% 
  count(gay, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  filter(gay == 1) %>% 
  mutate(gay = recode(gay, "1= 'Favor';
                        2= 'Oppose'")) %>% 
  mutate(age2 = recode(age2, "1 = 'Under 35';
                              0= 'Over 35'")) %>% 
  select(age2, gay, weight) %>% 
ggplot(., aes(x=reorder(age2, -weight), y=weight, group = gay)) + geom_col(position = "dodge") + labs(x= "Age" , y = "Percent in Favor of Gay Marriage") + 
  scale_y_continuous(labels = scales::percent)



gayunder <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age <= 35) %>% 
  filter(CC16_410a <=2) %>% 
  group_by(CC16_410a) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  count(CC16_335, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(vote = as.numeric(CC16_410a), gay = as.numeric(CC16_335)) %>% 
  mutate(vote = recode(vote, "1= 'Trump';
                       2= 'Clinton'")) %>% 
  mutate(gay = recode(gay, "1= 'Favor';
                        2= 'Oppose'")) %>% 
  ungroup(CC16_410a) %>% 
  mutate(label = c("Under 35")) %>% 
  select(vote, gay, weight, label)

gayover <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age > 35) %>% 
  filter(CC16_410a <=2) %>% 
  group_by(CC16_410a) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  count(CC16_335, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(vote = as.numeric(CC16_410a), gay = as.numeric(CC16_335)) %>% 
  mutate(vote = recode(vote, "1= 'Trump';
                       2= 'Clinton'")) %>% 
  mutate(gay = recode(gay, "1= 'Favor';
                      2= 'Oppose'")) %>% 
  ungroup(CC16_410a) %>% 
  mutate(label = c("Over 35")) %>% 
  select(vote, gay, weight, label)

gay <- bind_rows(gayunder, gayover)


### Overall Sample 

ab <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "1:35 =1 ; else =0")) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_332a)) %>% 
  group_by(age2) %>%
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(abort = as.numeric(CC16_332a)) %>% 
  filter(abort == 1) %>%
  mutate(abort = recode(abort, "1= 'Pro-Choice';
                        2= 'Pro-Life'")) %>% 
  mutate(age2 = recode(age2, "1 = 'Under 35';
                       0= 'Over 35'")) %>% 
  mutate(issue = c("Abortion")) %>% 
  select(age2, abort, weight, issue) 


gay <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "1:35 =1 ; else =0")) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  group_by(age2) %>% 
  mutate(gay = as.numeric(CC16_335)) %>% 
  count(gay, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  filter(gay == 1) %>% 
  mutate(gay = recode(gay, "1= 'Favor';
                        2= 'Oppose'")) %>% 
  mutate(age2 = recode(age2, "1 = 'Under 35';
                              0= 'Over 35'")) %>% 
  select(age2, gay, weight) %>% 
  mutate(issue = c("Gay Marriage")) 

plot <- bind_rows(ab, gay) %>% select(age2, weight, issue)


ggplot(plot, aes(x=fct_rev(age2), y=weight, fill = age2)) + geom_col(position = "dodge") + labs(x= "Age Group" , y = "Percent in Favor ", caption = "Date Source: CCES 2016" , title = "White Evangelical Protestants") + 
  scale_y_continuous(labels = scales::percent) + facet_wrap(~ issue, ncol =2 )  + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + labs(fill="") + 
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.position="none")

ggsave(file="entire_sample_abort_gay_facet.png", type = "cairo-png", width = 15, height = 10)



### Broken Down by Vote Choice 

ab <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "1:35 =1 ; else =0")) %>% 
  mutate(vote = recode(CC16_410a, "1=1; 2=2; else=0")) %>% 
  filter(vote >0) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>%  
  filter(complete.cases(CC16_332a)) %>% 
  group_by(age2, vote) %>%
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(age2, vote) %>% 
  mutate(age2 = as.numeric(age2), vote = as.numeric(vote)) %>% 
  mutate(abort = as.numeric(CC16_332a)) %>% 
  filter(abort == 1) %>%
  mutate(vote = recode(vote, "1= 'Trump';
                        2= 'Clinton'")) %>% 
  mutate(age2 = recode(age2, "1 = 'Under 35';
                       0= 'Over 35'")) %>% 
  mutate(issue = c("Abortion")) %>% 
  select(age2, weight, issue, vote) 


gay <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "1:35 =1 ; else =0")) %>% 
  mutate(vote = recode(CC16_410a, "1=1; 2=2; else=0")) %>% 
  filter(vote >0) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  group_by(age2, vote) %>% 
  count(CC16_335, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(age2, vote) %>% 
  mutate(gay = as.numeric(CC16_335), vote = as.numeric(vote)) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  filter(gay == 1) %>% 
  mutate(vote = recode(vote, "1= 'Trump';
                      2= 'Clinton'")) %>% 
  mutate(age2 = recode(age2, "1 = 'Under 35';
                       0= 'Over 35'")) %>% 
  mutate(issue = c("Gay Marriage")) %>% 
  select(age2, weight, issue, vote) 

plot <- bind_rows(ab, gay) %>% select(age2, weight, issue, vote)


plot %>%  filter(issue == "Abortion") %>% 
  ggplot(., aes(x=fct_rev(age2), y=weight, fill = age2)) + geom_col(position = "dodge") + labs(x= "Age Group" , y = "Percent in Favor ", caption = "Date Source: CCES 2016", title = "Abortion") + 
  scale_y_continuous(labels = scales::percent) + facet_wrap(~ vote, ncol =2 )  + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + labs(fill="") + 
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.position="none")

ggsave(file="abortion_facet.png", type = "cairo-png", width = 15, height = 10)


plot %>%  filter(issue == "Gay Marriage") %>% 
  ggplot(., aes(x=fct_rev(age2), y=weight, fill = age2)) + geom_col(position = "dodge") + labs(x= "Age Group" , y = "Percent in Favor ", caption = "Date Source: CCES 2016", title = "Gay Marriage") + 
  scale_y_continuous(labels = scales::percent) + facet_wrap(~ vote, ncol =2 )  + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + labs(fill="") + 
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.position="none")

ggsave(file="gay_facet.png", type = "cairo-png", width = 15, height = 10)

