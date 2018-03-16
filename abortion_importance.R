
a1 <- cces16 %>% 
  filter(gender ==1) %>% 
  filter(CC16_301b <6) %>% 
  count(CC16_301b, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(gender = c("Male"))  %>% 
  mutate(abort = to_factor(CC16_301b))


a2 <- cces16 %>% 
  filter(gender ==2) %>% 
  filter(CC16_301b <6) %>%
  count(CC16_301b, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(gender = c("Female"))  %>% 
  mutate(abort = to_factor(CC16_301b))

agraph <- bind_rows(a1, a2)

agraph %>% 
  ggplot(., aes(x=abort, y=pct, group =gender, label = gender, fill= gender)) + geom_col(position = "dodge", color = "black") +bar_rb()  +  
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Importance of Abortion", y= "Percent of Sample", title = "How Important is Abortion to You?", caption = "Data: CCES 2016") +
  # theme(axis.text.x = element_text(family = "IBM Plex Serif", size =16, angle = 45, hjust = 1))
  
  ggsave(file="D://cces/abortion_gender.png", type = "cairo-png", width = 20, height = 15)




a3 <- cces16 %>% 
  filter(gender ==1) %>% 
  filter(CC16_301b <6) %>% 
  filter(CC16_332f ==1) %>% 
  count(CC16_301b, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(gender = c("Male"))  %>% 
  mutate(abort = to_factor(CC16_301b)) %>% 
  mutate(pos = "Pro-Life")


a4 <- cces16 %>% 
  filter(gender ==2) %>% 
  filter(CC16_301b <6) %>%
  filter(CC16_332f ==1) %>% 
  count(CC16_301b, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(gender = c("Female"))  %>% 
  mutate(abort = to_factor(CC16_301b)) %>% 
  mutate(pos = "Pro-Life")

a5 <- cces16 %>% 
  filter(gender ==1) %>% 
  filter(CC16_301b <6) %>% 
  filter(CC16_332a ==1) %>% 
  count(CC16_301b, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(gender = c("Male"))  %>% 
  mutate(abort = to_factor(CC16_301b)) %>% 
  mutate(pos = "Pro-Choice")


a6 <- cces16 %>% 
  filter(gender ==2) %>% 
  filter(CC16_301b <6) %>%
  filter(CC16_332a ==1) %>% 
  count(CC16_301b, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(gender = c("Female"))  %>% 
  mutate(abort = to_factor(CC16_301b)) %>% 
  mutate(pos = "Pro-Choice")



agraph <- bind_rows(a3, a4, a5, a6)

agraph %>% 
  ggplot(., aes(x=abort, y=pct, group =gender, label = gender, fill= gender)) + geom_col(position = "dodge", color = "black") +bar_rb()  +  
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Importance of Abortion", y= "Percent of Sample", title = "How Important is Abortion to You?", caption = "Data: CCES 2016") + facet_grid(.~pos)
# theme(axis.text.x = element_text(family = "IBM Plex Serif", size =16, angle = 45, hjust = 1))

ggsave(file="D://cces/abortion_gender_by_position.png", type = "cairo-png", width = 20, height = 15)