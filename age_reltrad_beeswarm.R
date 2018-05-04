cces16 <- cces16 %>% 
  mutate(reltrad = recode(reltrad, "'bprot' = 'Black Protestant';
                                    'catholic' = 'Catholic';
                                    'evangelical' = 'Evangelical';
                                    'mainline' = 'Mainline';
                                    'jewish' = 'Jewish';
                                    'none' = 'No Faith';
                                    'other' = 'Other Faith'")) 

bar_rb <- function(base_size = 25, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 54, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =32),
       axis.title.y =  element_text(family = "Product Sans", size =32), 
       axis.text.x = element_text(family = "Product Sans", size =24), 
       legend.text=element_text(size=36)
)
}

cces16 %>% 
  group_by(reltrad) %>% 
  mutate(age = 2016 - birthyr) %>% 
  summarise(mean = mean(age, na.rm = TRUE))

test <- cces16 %>% sample_frac(.10)

cces16 %>% 
  mutate(age = 2016 - birthyr) %>% 
  filter(reltrad != "NA") %>% 
  ggplot(., aes(x= factor(reltrad), y = age, color=reltrad)) + geom_quasirandom() + bar_rb() + 
  theme(legend.position="none") +
  scale_color_aaas() +
  labs(x= "Denomination", y = "Age", title = "The Age Distribution of Each Tradition", caption = "Data: CCES 2016", subtitle = "Average Age Indicated at the Top of Each Plot") +
  annotate("text", x= 1, y = 98, label = "48.9", size = 8, family = "Product Sans") +
  annotate("text", x= 2, y = 98, label = "49.0", size = 8, family = "Product Sans") +
  annotate("text", x= 3, y = 98, label = "51.1", size = 8, family = "Product Sans") +
  annotate("text", x= 4, y = 98, label = "52.9", size = 8, family = "Product Sans") +
  annotate("text", x= 5, y = 98, label = "54.9", size = 8, family = "Product Sans") +
  annotate("text", x= 6, y = 98, label = "43.5", size = 8, family = "Product Sans") +
  annotate("text", x= 7, y = 98, label = "44.0", size = 8, family = "Product Sans") 


ggsave(file="D://cces/reltrad_beeswarm_age.png", type = "cairo-png", width = 21, height = 15)

cces16 %>% 
  mutate(age = 2016 - birthyr) %>% 
  mutate(age2 = recode(age, "18:30 =1; 31:39 =2 ; 40:50=3; 51:60=4; 61:70=5;71:100=6; else =99")) %>% 
  group_by(reltrad) %>% 
  count(age2) %>% 
  mutate(pct = prop.table(n))  %>% 
  filter(age2 ==1)








