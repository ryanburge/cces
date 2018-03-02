nd <- cces16 %>% 
  mutate(age = 2016  - birthyr) %>% 
  filter(religpew_protestant ==3) %>% 
  mutate(group = c("Nondenominational"))
  
sbc <- cces16 %>% 
  mutate(age = 2016  - birthyr) %>% 
  filter(religpew_baptist ==1) %>% 
  mutate(group = c("Southern Baptist"))
  
umc <- cces16 %>% 
  mutate(age = 2016  - birthyr) %>% 
  filter(religpew_methodist ==1) %>% 
  mutate(group = c("United Methodist"))

umc %>% summarise(mean= mean(age, na.rm = TRUE))
sbc %>% summarise(mean= mean(age, na.rm = TRUE))
nd %>% summarise(mean= mean(age, na.rm = TRUE))


com <- bind_rows(nd, sbc, umc)
  
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
  
com %>% 
  ggplot(., aes(x= factor(group), y = age, color=group)) + geom_quasirandom() + bar_rb() + 
  theme(legend.position="none") +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Denomination", y = "Age", title = "The Age Distribution of Each Denomination", caption = "Data: CCES 2016", subtitle = "Average Age: Nondenominational = 49.6, Southern Baptist = 52.4, United Methodist = 56.4")


ggsave(file="D://cces/ndsbc/beeswarm_age.png", type = "cairo-png", width = 21, height = 15)

  