cces16 <- cces16 %>% 
  mutate(ed2 = recode(educ, "1:2 = 'High School or Less'; 3:4 = 'Some College'; 5 = 'Bachelors Degree'; 6 = 'Post-Graduate'")) %>% 
  mutate(ed3 = recode(educ, "1:2 = 1; 3:4 = 2; 5 = 3; 6 = 4")) %>% 
  


b1 <- cces16 %>% 
    group_by(educ) %>% 
    mean_ci(meet, wt = commonweight_vv_post) %>% 
    mutate(act = "Pol. Meeting") 
  
b2 <- cces16 %>% 
    group_by(educ) %>%
    mean_ci(sign, wt = commonweight_vv_post) %>% 
    mutate(act = "Campaign Sign")  
   
  
b3 <- cces16 %>% 
    group_by(educ) %>%
    mean_ci(vol, wt = commonweight_vv_post) %>% 
    mutate(act = "Campaign Vol.") 
   
  
b4 <- cces16 %>% 
    group_by(educ) %>%
    mean_ci(money, wt = commonweight_vv_post) %>% 
    mutate(act = "Donate Money") 

b5 <- cces16 %>% 
    group_by(educ) %>%
    mean_ci(blood, wt = commonweight_vv_post) %>% 
    mutate(act = "Donate Blood") 
    
  
all <- bind_rows(b1, b2, b3, b4, b5)

all %>% 
  ggplot(., aes(x= educ, y = mean, fill = factor(educ))) +
  geom_col(color = "black") +
  facet_wrap(.~act) + 
  theme_minimal() +
  theme(legend.position = "none") +   
  scale_y_continuous(labels = scales::percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  scale_fill_d3(palette = "category20") + 
  theme(text=element_text(size=64, family="font")) +
  theme(axis.text.x = element_text(family = "font", size =34, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "font", size =34)) +
  labs(y = "Percent Engaging in Each Activity", x = "", title = "Education and Social Activities", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(face= "bold", size = 56)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("No HS", "HS Diploma", "Some College", "Associates", "Bachelors", "Post-Grad"))

ggsave("pol_act/ed_act.png", type = "cairo-png")



  
