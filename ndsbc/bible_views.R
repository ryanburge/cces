bible <- gss %>% 
  filter(denom ==14 | denom == 70 | denom == 22) %>% 
  filter(bible <4) %>% 
  group_by(denom) %>% 
  filter(year > 2007) %>%  
  count(bible) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(denom) %>% 
  mutate(denom = recode(denom, "14 = 'Southern Baptist'; 22 = 'United Methodist'; 70 = 'Nondenominational'")) %>% 
  mutate(bible = recode(bible, "1= 'Literal'; 2= 'Inspired'; 3 = 'Fables'")) 

bible <- bible %>% 
  mutate(denom = as.factor(denom)) %>% 
  mutate(denom = fct_relevel(denom, "Southern Baptist","Nondenominational", "United Methodist" ))

bible <- bible %>% 
  mutate(pct = round(pct, 3))

bible %>% 
  ggplot(., aes(x=denom, y = pct, fill = bible)) + geom_col(color = "black", position = "dodge") + flip_bar_rb() +
  theme(axis.text.x = element_text(family = "Product Sans", size =24)) +
  scale_y_continuous(labels = scales::percent) + coord_flip() + 
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "Religious Groups", y = "Percent of Each Denomination", title = "View of the Bible in 3 Traditions", caption = "Data: GSS 2008-2016") +
  theme(plot.title = element_text(size=64)) 

ggsave(file="D://cces/ndsbc/bible_flip.png", type = "cairo-png", width = 18, height = 15)
