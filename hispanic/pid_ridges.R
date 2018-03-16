



pid7<- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(pid7 <8) %>% 
  group_by(race) %>% 
  summarise(mean = mean(pid7),
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  ungroup(race) %>% 
  mutate(race2 = to_factor(race))



pid7 %>% 
  # filter(age2 != "80 and Over") %>% 
  ggplot(., aes(x = mean, y = reorder(race2, -mean), group = race2, label = race2))  +
  geom_point(shape=21, size =7, aes(fill = factor(race2)), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(race2)), height=0, size = 3, show.legend = FALSE) + 
  labs(x = "Self Described Party Identification", y ="", title = "Comparing Party ID Among Born Again Protestants", caption = "Data: CCES 2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6.5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +    
  mean_rb() + theme(legend.position="none")

ggsave(file="D://cces/hispanic/pid_by_race.png", type = "cairo-png", width = 20, height = 15)



mean_rb <- function(base_size = 25, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.x =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 44, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =24),
       axis.title.y =  element_text(family = "Product Sans", size =24), 
       axis.text.x = element_text(family = "Product Sans", size =24), 
       legend.text=element_text(size=36)
)
  
}




cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(pid7 <8) %>%
  mutate(pid7 = as.numeric(pid7), race = to_factor(race)) %>% 
  ggplot(., aes(x = pid7, y = race)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("dodgerblue3", "gray", "firebrick3")) +
  mean_rb() +
  scale_x_continuous(limits = c(-.5,9), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "Self Described Party Identification", y ="", title = "Distribution of Party ID", caption = "Data: CCES 2016", subtitle = "Among Born Again Protestants")


ggsave(file="D://cces/hispanic/ridge.png", type = "cairo-png", width = 20, height = 15)