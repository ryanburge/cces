states <- read_csv("C:/Users/Ryan Burge/Desktop/states.csv")

v16 <- cces16 %>% filter(pew_bornagain ==1 & race ==1 & religpew ==1 & complete.cases(CC16_410a)) %>% 
  group_by(inputstate) %>% count(CC16_410a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% na.omit() %>% filter(CC16_410a ==1)  %>% 
  mutate(pct = weight*100, year = c(2016)) %>% select(-n, -weight, -CC16_410a) %>% filter(inputstate != 11) %>% bind_cols(states)

v12 <- cces12 %>% filter(pew_bornagain ==1 & race ==1 & religpew ==1 & complete.cases(CC410a)) %>% 
  group_by(inputstate) %>% count(CC410a, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n)) %>% na.omit() %>% filter(CC410a ==2)  %>% 
  mutate(pct = weight*100, year = c(2012)) %>% select(-n, -weight, -CC410a) %>% filter(inputstate != 11) %>% bind_cols(states)

v08 <- evan08 %>% 
  group_by(V206) %>% count(CC410, wt = V201) %>% 
  mutate(weight = prop.table(n)) %>% na.omit() %>% filter(CC410 ==1)  %>% 
  mutate(pct = weight*100, year = c(2008)) %>% select(-n, -weight, -CC410) %>% rename(inputstate = V206) %>% bind_cols(states)

map <- bind_rows(v08, v12, v16) %>% ungroup(inputstate) %>%  select(-inputstate)



ggplot(map, aes(x=year, y=pct/100, fill =year)) + geom_line(color = "firebrick1") + facet_geo(~state) +
  scale_x_continuous(breaks = c(2008, 2012, 2016), labels = function(x) paste0("'", substr(x, 3, 6))) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "White Evangelicals for the Republican Candidate",
       caption = "Data Source: CCES 2008-2016",
       x = "Year",
       y = "% for Republican Candidate") +
  theme(strip.text.x = element_text(size = 6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme_grey(base_size = 16)

ggsave(file="geo_facet.png", type = "cairo-png", width = 20, height =12)
