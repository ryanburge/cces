ba_relig <- cces16 %>% 
  filter(pew_bornagain ==1) %>% 
  count(religpew, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(religpew = recode(religpew, "1 = 'Protestant';
                                      2 = 'Catholic';
                                      3 = 'Mormon';
                                      4 = 'Orthodox';
                                      5 = 'Jewish';
                                      6 = 'Muslim';
                                      7 = 'Buddhist';
                                      8 = 'Hindu';
                                      9 = 'Atheist';
                                      10 = 'Agnostic';
                                      11 = 'Nothing in Particular';
                                      12 = 'Something Else';
                                      else = 99"))


ba_relig %>% 
  filter(religpew != "99") %>% 
  mutate(pct = round(pct, 3)) %>% 
  ggplot(., aes(x= reorder(religpew, pct), y = pct, fill = pct)) + geom_col(color = "black") +
  coord_flip() +
  flip_bar_rb() +
  scale_fill_gradient(low = "#fffbd5", high = "#b20a2c") +
  theme(legend.position="none") + scale_y_continuous(labels = scales::percent) +
  labs(x = "Religious Family", y = "Percent of Born-Again Sample", title = "How Do Born-Agains Identify?", caption = "Data: CCES (2016)") +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  theme(plot.title = element_text(size =36)) 


ggsave(file="ba_by_religpew.png", type = "cairo-png", width = 15, height = 12)


a <- cces16 %>% 
  filter(pew_bornagain ==1 & religpew != 1) %>% 
  filter(CC16_410a <3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = "Born-Again Not Protestants")

b <- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(CC16_410a <3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = "Born-Again Protestants")

c <- bind_rows(a,b) %>%
  mutate(pct = round(pct, 3)) %>% 
  mutate(pres = recode(CC16_410a, "1 = 'Donald Trump'; 2 = 'Hillary Clinton'"))

c %>%  
  ggplot(., aes(x=pres, y=pct)) + geom_col(aes(fill = pres), color = "black") + facet_grid(.~ group) +
  bar_rb() +
  theme(legend.position="none") + scale_y_continuous(labels = scales::percent) +
  labs(x = "Candidate", y = "Percent of Two Party Vote", title = "Does the Distinction Matter?", caption = "Data: CCES (2016)") +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  theme(plot.title = element_text(size =36)) +
  scale_fill_manual(values=c("firebrick1", "dodgerblue3"))
 
ggsave(file="ba_vote_choice.png", type = "cairo-png", width = 15, height = 12)


a <- cces16 %>% 
  filter(pew_bornagain ==1 & religpew != 1) %>% 
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  count(attend, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = "Born-Again Not Protestants")

b <- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  count(attend, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = "Born-Again Protestants")

c <- bind_rows(a,b) %>%
  mutate(pct = round(pct, 3)) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3='Yearly';4='Monthly';5= 'Weekly';6= 'Weekly+'; 7 = 'Do Not Know'"))

c$attend <- factor(c$attend, levels = c("Do Not Know", "Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))


c %>%  
  ggplot(., aes(x=attend, y=pct)) + geom_col(aes(fill = attend), color = "black") + facet_grid(.~ group) +
  bar_rb() +
  theme(legend.position="none") + scale_y_continuous(labels = scales::percent) +
  labs(x = "Self Reported Attendance", y = "", title = "Does the Distinction Matter?", caption = "Data: CCES (2016)") +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .5), size = 8, family = "Product Sans") +
  theme(plot.title = element_text(size =36)) +
  scale_fill_npg()

ggsave(file="ba_attend.png", type = "cairo-png", width = 15, height = 12)






