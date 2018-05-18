cces16 <- cces16 %>% 
  mutate(choice = recode(CC16_332a, "1=1; 2=0; else =99")) %>% 
  mutate(rape = recode(CC16_332b, "1=0; 2=1; else=99")) %>% 
  mutate(week20 = recode(CC16_332c, "1=0; 2=1; else=99")) %>% 
  mutate(insurance = recode(CC16_332d, "1=0; 2=1; else=99")) %>% 
  mutate(fedfunds = recode(CC16_332e, "1=0; 2=1; else=99")) %>% 
  mutate(prohibit = recode(CC16_332f, "1=0; 2=1; else=99")) 


a1 <- cces16 %>% 
  filter(choice != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(choice), 
            sd = sd(choice), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Abortion - Any Reason"))

a2 <- cces16 %>% 
  filter(choice != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(rape), 
            sd = sd(rape), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Abortion - No Reason"))

a3 <- cces16 %>% 
  filter(choice != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(week20), 
            sd = sd(week20), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Abortion - After 20 Wks."))

a4 <- cces16 %>% 
  filter(choice != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(insurance), 
            sd = sd(insurance), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Ins. Co. Cover Abortion"))

a5 <- cces16 %>% 
  filter(choice != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(fedfunds), 
            sd = sd(fedfunds), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Fed. Funds for Abortion"))

ab_graph <- bind_rows(a1, a3, a4, a5) %>% 
  rename(group = new) %>% 
  mutate(issue = as.factor(issue))

Palette <- c("gray48", "black", "gray87")


mean_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.x =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 64, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =24), 
       legend.text=element_text(size=36)
)
  
}



ab_graph %>% 
  filter(group != "NA") %>% 
  ggplot(., aes(y=mean, x= fct_reorder(issue, mean), color = group)) +
  geom_point(position=position_dodge(width=0.75), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.75), size = 1) +
  coord_flip() +
  mean_rb() +
  labs(title = "Support for Abortion Rights", x = "Abortion Questions", y = "<- Less Support for Abortion : Greater Support for Abortion ->") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = Palette) 

ggsave(file="D://cces/gay_evangelical/abortion_four_scenarios.png", type = "cairo-png", width = 20, height =12, dpi = 100)
