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
  ylab("Percent of Two Party Vote") + xlab("") +
  ggtitle("Hispanic Evangelicals Are Less Republican") +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  +  
  scale_y_continuous(labels = scales::percent)  +  flip_bar_rb() +
  theme(plot.title = element_text(face="bold", size = 44))

ggsave(file="D://cces/hispanic/three_elects.png", type = "cairo-png", width = 15, height = 12)




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
  filter(race ==3) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(vote16 == "Republican" | vote16 == "Democrat") %>% 
  filter(complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Hispanic Evangelicals")) %>% 
  mutate(year = c(2016)) %>% 
  rename(party = vote16)


vote16a <- cces16 %>% 
  filter(race ==3) %>% 
  filter(vote16 == "Republican" | vote16 == "Democrat") %>% 
  filter(complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("All Hispanics")) %>% 
  mutate(year = c(2016)) %>% 
  rename(party = vote16)



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
  filter(race == 3) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(vote12)) %>% 
  filter(vote12 == "Republican" | vote12 == "Democrat") %>% 
  count(vote12, wt = weight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Hispanic Evangelicals")) %>% 
  mutate(year = c(2012)) %>% 
  rename(party = vote12)

vote12a <- cces12 %>% 
  filter(race == 3) %>% 
  filter(complete.cases(vote12)) %>% 
  filter(vote12 == "Republican" | vote12 == "Democrat") %>% 
  count(vote12, wt = weight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("All Hispanics")) %>% 
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
  filter(V211 == 3) %>% 
  filter(V215 ==1 & V219 ==1) %>% 
  filter(complete.cases(vote08)) %>% 
  filter(vote08 == "Republican" | vote08 == "Democrat") %>% 
  count(vote08, wt = V201) %>% 
  mutate(pct = prop.table(n)) %>%
  mutate(year = c(2008)) %>% 
  mutate(group = c("Hispanic Evangelicals")) %>% 
  rename(party = vote08)

vote08a <- cces08 %>%
  filter(V211 == 3) %>% 
  filter(complete.cases(vote08)) %>% 
  filter(vote08 == "Republican" | vote08 == "Democrat") %>% 
  count(vote08, wt = V201) %>% 
  mutate(pct = prop.table(n)) %>%
  mutate(year = c(2008)) %>% 
  mutate(group = c("All Hispanics")) %>% 
  rename(party = vote08)

voteplot <- bind_rows(vote08, vote12, vote16, vote08a, vote12a, vote16a) %>% mutate(year = as.factor(year))

voteplot %>% 
  ggplot(., aes(year, pct)) + geom_col(aes(fill= party), colour = "black") + 
  facet_grid(group ~ .)  + 
  coord_flip() +
  scale_fill_manual(values=c("dodgerblue3", "firebrick1", "purple", "goldenrod1", "dodgerblue3", "firebrick1" )) +
  theme(axis.title.y = element_blank()) + 
  ylab("Percent of Two Party Vote") + xlab("") +
  ggtitle("Hispanic Evangelicals vs. All Hispanics") +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  +  
  scale_y_continuous(labels = scales::percent)  +  flip_bar_rb() +
  theme(plot.title = element_text(face="bold", size = 44))

ggsave(file="D://cces/hispanic/three_elects_evan_vs_not.png", type = "cairo-png", width = 15, height = 12)