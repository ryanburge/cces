cces16 <- cces16 %>% 
  mutate(race2 = as.numeric(race)) %>% 
  mutate(race2 = recode(race2, "1= 'White'; 3 = 'Hispanic'; else =99"))

a<- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(race2 != 99) %>% 
  group_by(race2) %>% 
  count(CC16_331_1) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_331_1 ==1) %>% 
  rename(issue = CC16_331_1) %>% 
  mutate(group = c("Pathway to Citizenship"))
  

b<- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(race2 != 99) %>% 
  group_by(race2) %>% 
  count(CC16_331_2) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_331_2 ==1) %>% 
  rename(issue = CC16_331_2) %>% 
  mutate(group = c("Increase Border Patrol"))

c<- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(race2 != 99) %>% 
  group_by(race2) %>% 
  count(CC16_331_3) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_331_3 ==1) %>% 
  rename(issue = CC16_331_3) %>% 
  mutate(group = c("DREAM Act"))


all <- bind_rows(a,b,c) %>% select(race2, pct, group)

all %>% 
  mutate(pct = round(pct, 3)) %>% 
  ggplot(., aes(x=race2, y = pct, fill = race2)) + geom_col(color = "black") +
  facet_grid(.~ group) +
  bar_rb() +
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.position="none") +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x = "Race", y = "Percent", title = "Support for Immigration", subtitle = "Among Evangelical Protestants", caption = "Data: CCES 2016") +
  scale_fill_brewer(palette = "") 

ggsave(file="D://cces/hispanic/immigration.png", type = "cairo-png", width = 15, height = 12)

  
