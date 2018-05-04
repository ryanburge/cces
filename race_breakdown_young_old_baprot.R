young <- cces16 %>% 
  filter(baprot ==1) %>% 
  mutate(age = 2016- birthyr) %>% 
  mutate(young = recode(age, "1:34=1; else=0")) %>% 
  filter(young ==1) %>% 
  count(race,  wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Under 35"))
  
  
old <- cces16 %>% 
  filter(baprot ==1) %>% 
  mutate(age = 2016- birthyr) %>% 
  mutate(old = recode(age, "66:100=1; else=0")) %>% 
  filter(old ==1) %>% 
  count(race,  wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Over 65"))  

  
total <- bind_rows(young, old) %>% 
  mutate(race = recode(race, "1 = 'White';
                              2 = 'Black';
                              3 = 'Hispanic';
                              4 = 'Asian';
                              5 = 'Native American';
                              6 = 'Mixed'; 
                              7 = 'Other';
                              8 = 'Middle Eastern';
                              else = 99"))

total %>% 
  ggplot(., aes(1, pct)) + geom_col(aes(fill= race), colour = "black") + 
  facet_grid(group ~ .)  + 
  coord_flip() +
  scale_fill_npg() +
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  ylab("Percent of Population") + xlab("") +
  ggtitle("Racial Breakdown of Born-Again Protestants") +
  labs(caption = "Data: CCES 2016") +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  +  
  scale_y_continuous(labels = scales::percent)  +  flip_bar_rb() +
  theme(plot.title = element_text(face="bold", size = 44)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave(file="D://cces/demo_ba_prots.png", type = "cairo-png", width = 18, height = 10)

young <- cces16 %>% 
  filter(religpew ==1 | religpew ==2) %>% 
  mutate(age = 2016- birthyr) %>% 
  mutate(young = recode(age, "1:35=1; else=0")) %>% 
  filter(young ==1) %>% 
  count(race,  wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Under 35"))

