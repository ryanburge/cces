
attend <- cces16 %>% 
  mutate(race2 = recode(race, "1=1; 3=2; else=0")) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race2 >0) %>% 
  group_by(age2, race2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  ungroup(race2) %>% 
  mutate(race2 = as.numeric(race2)) %>% 
  mutate(race2 = recode(race2, "1= 'White Evangelical'; 2 ='Hispanic Evangelical'")) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'"))


attend %>% 
  filter(age2 != "80 and Over") %>% 
  ggplot(., aes(x = mean, y = age2, group = race2, label = race2))  +
  geom_point(shape=21, size =4, aes(fill = factor(race2)), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(race2)), height=0, size = 1, show.legend = FALSE) + 
  labs(x = "Religious Attendance", y ="", title = "Comparing Church Attendance", caption = "Data: CCES 2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))  +  
  mean_rb()

ggsave(file="D://cces/hispanic/attendance_by_age_ci_facet.png", type = "cairo-png", width = 20, height = 15)
