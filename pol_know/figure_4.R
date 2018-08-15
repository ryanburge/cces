age_know <-cces16 %>% 
  mutate(age = 2016 - birthyr) %>% 
  group_by(reltrad, age) %>% 
  mean_ci(know) %>% 
  filter(reltrad == "evangelical" | reltrad == "catholic" | reltrad == "none" | reltrad == "mainline") %>% 
  ungroup(reltrad)

age_know <- age_know %>% 
  mutate(reltrad = recode(reltrad, "3 = 'Black Protestant';
                          'catholic' = '   Catholic   ';
                          'evangelical' = '   Evangelical   ';
                          'mainline' = '   Mainline   ';
                          5 = 'Jewish';
                          'none' = '   No Faith   ';
                          6 = 'Other Faith';
                          8 = 'Entire Sample'")) 


age_know2 <-cces16 %>% 
  mutate(age = 2016 - birthyr) %>% 
  group_by(age) %>% 
  mean_ci(know) %>% 
  mutate(reltrad = "   Entire Sample   ")
  
age_know <- bind_rows(age_know, age_know2)

font_add_google("Oswald", "font")
showtext_auto()

age_know %>% 
  ggplot(., aes(x = age, y = mean, group = reltrad)) +
  # geom_line(colour="black") +
  # geom_point(shape = 21, colour="black", aes(fill=reltrad), size=1, stroke=1) +
  geom_smooth(aes(color= reltrad)) +
  scale_x_continuous(limits=c(18,85)) +
  scale_y_continuous(limits=c(1.65,5)) +
  theme_minimal() +
  labs(x = "Age", y = "Political Knowledge", title = "Relationship Between Age and Political Knowledge", caption = "Data: CCES (2016)") +
  theme(text=element_text(size=64, family="font")) +
  scale_color_d3() +
  theme(legend.position = "bottom") + 
  theme(legend.title=element_blank())

ggsave("pol_know/figure4.png", type = "cairo-png")

