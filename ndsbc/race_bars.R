nd <- cces16 %>% 
  filter(religpew_protestant ==3) %>% 
  count(race) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Nondenominational"))

sbc <- cces16 %>% 
  filter(religpew_baptist ==1) %>% 
  count(race) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Southern Baptist"))

umc <- cces16 %>% 
  filter(religpew_methodist ==1) %>% 
  count(race) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("United Methodist"))

com <- bind_rows(nd, sbc, umc) %>% 
  mutate(race = recode(race, "1 = 'White'; 2 = 'Black'; 3 = 'Hispanic'; 4 = 'Asian'; 5 = 'Native American'; 6= 'Mixed'; 7 = 'Other'; 8 = 'Middle Eastern' ")) %>% 
  mutate(race = as.factor(race)) %>% 
  filter(pct > .03)

com %>% 
  ggplot(., aes(1, pct)) + geom_col(aes(fill= race), colour = "black") + coord_flip() +
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  scale_y_continuous(labels = scales::percent) + facet_grid(group ~ .)  +
  flip_bar_rb() +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  labs(x= "", y = "", title = "A Racial Breakdown of Denominations", subtitle = "Only Racial Groups that are Greater than 3% of the Denomination are Displayed Below", caption = "Data: CCES 2016")  + 
  scale_fill_brewer(palette = "Set1")


ggsave(file="D://cces/ndsbc/racial_flip.png", type = "cairo-png", width = 21, height = 15)

