lgbtevan <- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 ) %>% 
  count(pid7, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(pid7 !=8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(type = c("LGB + Evangelical")) %>% 
  select(-n)
  
lgbt <- cces16 %>% 
  # filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 ) %>% 
  count(pid7, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(pid7 !=8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(type = c("LGB")) %>% 
  select(-n)

evan <- cces16 %>% 
  filter(evangelical ==1) %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(pid7, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(pid7 !=8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(type = c("Evangelical")) %>% 
  select(-n)


pid7 <- bind_rows(lgbtevan,lgbt, evan)

theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 40, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =18, angle = 45, hjust  =1)
)
  
}

Palette <- c("gray48", "black", "gray87")

pid7 <- pid7 %>% mutate(pct = round(pct,3))

cces16 %>% 
  group_by(new) %>% 
  filter(pid7 !=8) %>%
  summarise(mean = mean(pid7))
   

pid7 %>% 
  filter(pid7 != "Skipped") %>% 
  filter(pid7 != "Not Asked") %>% 
  ggplot(.,aes(x=pid7, y=pct, fill = type)) + 
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  scale_fill_manual(values = Palette) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 6, family = "IBM Plex Serif") +
  labs(x= "Party Identification", y = "Percent of Each Sample", title = "The Political Partisanship of LGBT Evangelicals", caption = "Data: CCES 2016", subtitle = "Mean Party Identification: Evangelical = 4.52, LGB = 2.64, LGB Evangelical = 3.25")+ theme_rb()

ggsave(file="D://cces/gay_evangelical/pid7_2016_new.png", type = "cairo-png", width = 20, height =12)





cces16 %>% 
  group_by(new) %>% 
  # filter(new != NA) %>% 
  summarise(mean = mean(pid7))



