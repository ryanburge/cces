


white <- gss %>% 
  filter(evangelical ==1) %>% 
  filter(race ==1) %>% 
  group_by(year) %>% 
  summarise(mean = mean(childs, na.rm = TRUE)) %>% 
  mutate(group = c("White Evangelicals"))

black <- gss %>% 
  filter(blackprot ==1) %>% 
  filter(race ==2) %>% 
  group_by(year) %>% 
  summarise(mean = mean(childs, na.rm = TRUE)) %>% 
  mutate(group = c("Black Evangelicals"))

hispanic <- gss %>% 
  filter(evangelical ==1) %>% 
  filter(hispanic !=1) %>% 
  group_by(year) %>% 
  summarise(mean = mean(childs, na.rm = TRUE)) %>% 
  mutate(group = c("Hispanic Evangelicals"))

asian <- gss %>% 
  mutate(asian = recode(racecen1, "4:10=1; else=0")) %>% 
  filter(evangelical ==1) %>% 
  filter(asian ==1) %>% 
  group_by(year) %>% 
  summarise(mean = mean(childs, na.rm = TRUE)) %>% 
  mutate(group = c("Asian Evangelicals")) 

race <- bind_rows(white, black, hispanic, asian)

race %>% 
  filter(year >= 2000) %>% 
  ggplot(., aes(x = year, y= mean, group = group, color = group, label = group)) + geom_smooth(se = FALSE) + long_rb() +
  labs(x= "Year", y = "Mean # of Children", title = "Birth Rates Among Evangelicals", caption = "Data: GSS 2000-2016")

ggsave(file="D://cces/hispanic/birth_rate_race.png", type = "cairo-png", width = 20, height = 15)