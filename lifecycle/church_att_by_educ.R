c1 <- cces16 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(college = recode(educ, "5:6=1; else=0")) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  group_by(age2, college) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se)%>% 
  mutate(group = c("Entire Sample"))



c1 <- c1 %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'")) %>% 
  ungroup(college) %>% 
  mutate(college = as.numeric(college)) %>% 
  mutate(college = recode(college, "0= 'Less than a BA'; 1 = '4 Year Degree'"))

  

c2 <- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(college = recode(educ, "5:6=1; else=0")) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  group_by(age2, college) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(group = c("Evangelicals"))


c2 <- c2 %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'")) %>% 
  ungroup(college) %>% 
  mutate(college = as.numeric(college)) %>% 
  mutate(college = recode(college, "0= 'Less than a BA'; 1 = '4 Year Degree'"))



c3 <- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(college = recode(educ, "5:6=1; else=0")) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  group_by(age2, college) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se)  %>% 
  mutate(group = c("Mainline"))


c3 <- c3 %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'")) %>% 
  ungroup(college) %>% 
  mutate(college = as.numeric(college)) %>% 
  mutate(college = recode(college, "0= 'Less than a BA'; 1 = '4 Year Degree'"))



c4 <- cces16 %>% 
  filter(religpew ==2) %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(college = recode(educ, "5:6=1; else=0")) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  group_by(age2, college) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se)  %>% 
  mutate(group = c("Catholics"))


c4 <- c4 %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'")) %>% 
  ungroup(college) %>% 
  mutate(college = as.numeric(college)) %>% 
  mutate(college = recode(college, "0= 'Less than a BA'; 1 = '4 Year Degree'"))

c5 <- cces16 %>% 
  filter(religpew ==3) %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(college = recode(educ, "5:6=1; else=0")) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  group_by(age2, college) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se)  %>% 
  mutate(group = c("Mormons"))


c5 <- c5 %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'")) %>% 
  ungroup(college) %>% 
  mutate(college = as.numeric(college)) %>% 
  mutate(college = recode(college, "0= 'Less than a BA'; 1 = '4 Year Degree'"))

c6 <- cces16 %>% 
  filter(religpew ==3) %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(college = recode(educ, "5:6=1; else=0")) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  group_by(age2, college) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se)  %>% 
  mutate(group = c("Jewish"))


c6 <- c6 %>% 
  ungroup(age2) %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'")) %>% 
  ungroup(college) %>% 
  mutate(college = as.numeric(college)) %>% 
  mutate(college = recode(college, "0= 'Less than a BA'; 1 = '4 Year Degree'"))



cc <- bind_rows(c1, c2, c3, c4, c5, c6)

cc %>% 
  # filter(type == "White Mainline") %>% 
  ggplot(., aes(x = mean, y = age2, group = college, label = college))  +
  geom_point(shape=21, size =4, aes(fill = factor(college)), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(college)), height=0, size = 1, show.legend = FALSE) + 
  # scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey", "red", "green")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + 
  # scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) + 
  theme(text=element_text(size=42, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Religious Attendance", y ="", title = "Does a College Degree Change Attendance?", caption = "Data: CCES 2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))  +  
  theme(plot.title = element_text(face="bold")) +  facet_grid(group~.)  

ggsave(file="D://cces/lifecycle/attendance_by_college_ci_facet.png", type = "cairo-png", width = 13, height = 18)


