
library(car)

cces16$vote16 <- as.numeric(cces16$CC16_410a)

cces16$vote16<-Recode(cces16$vote16,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA")


g1 <- cces16 %>% 
  filter(religpew ==1 & pew_bornagain ==1 & complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(vote16 == "Donald Trump") %>% 
  mutate(rac = c("All BA Prots")) %>% 
  select(rac, pct)

cces16$rac <- to_factor(cces16$race)

graph <- cces16 %>% 
  filter(religpew ==1 & pew_bornagain ==1 & complete.cases(vote16)) %>% 
  group_by(rac) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% filter(vote16 == "Donald Trump") %>% 
  select(rac, pct)

g2 <- bind_rows(g1, graph) %>% arrange(-pct)


g2 %>% 
  filter(rac == "White" | rac == "Black" | rac == "All BA Prots" | rac == "Hispanic" | rac == "Asian") %>% 
  ggplot(., aes(x=reorder(rac, -pct), y=pct)) + geom_col(fill = "firebrick3", color = "black")  +
  labs(x = "Race", y = "Trump's Vote Share", title = "Racial Disparities in Voting Among Born Again Protestants", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=32, family="KerkisSans")) +  
  scale_y_continuous(labels = scales::percent)

ggsave(file="D:/cces/matching/bar_graph.png", type = "cairo-png", width = 15, height = 12)
