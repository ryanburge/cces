library(tidyverse)
library(car)
library(haven)
library(janitor)
library(extrafont)

cces14 <- read_dta("D://cces/data/cces14.dta")

evanm <- cces14 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(child18 ==1 & marstat ==1) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = c("Wht. Evangelical")) %>% 
  mutate(status = c("Married w/Kids"))

evans <- cces14 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(child18 ==2 & marstat !=1) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = c("Wht. Evangelical")) %>% 
  mutate(status = c("Not Married w/o Kids"))

marry <- bind_rows(evanm, evans)

marry <- marry %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'"))


marry %>% 
  # filter(type == "White Mainline") %>% 
  ggplot(., aes(x = mean, y = age2))  +
  geom_point(shape=21, size =4, aes(fill = factor(age2)), show.legend = FALSE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(age2)), height=0, size = 1, show.legend = FALSE) + 
  scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) + 
  theme(text=element_text(size=28, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Religious Attendance", y ="", title = "Average Church Attendance by White, Born Again Protestants", caption = "Data: CCES 2014", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) + facet_grid(status~.) +  
  theme(plot.title = element_text(face="bold"))

ggsave(file="D://cces/lifecycle/attendance_by_married_kids_ci_facet_2014.png", type = "cairo-png", width = 15, height = 15)



evanm <- cces14 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(marstat ==1) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = c("Wht. Evangelical")) %>% 
  mutate(status = c("Married"))

evans <- cces14 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(marstat !=1) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = c("Wht. Evangelical")) %>% 
  mutate(status = c("Not Married"))

marry <- bind_rows(evanm, evans)

marry <- marry %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'"))



marry %>% 
  # filter(type == "White Mainline") %>% 
  ggplot(., aes(x = mean, y = age2))  +
  geom_point(shape=21, size =4, aes(fill = factor(age2)), show.legend = FALSE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(age2)), height=0, size = 1, show.legend = FALSE) + 
  scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) + 
  theme(text=element_text(size=28, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Religious Attendance", y ="", title = "Average Church Attendance by White, Born Again Protestants", caption = "Data: CCES 2014", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) + facet_grid(status~.) +  
  theme(plot.title = element_text(face="bold"))

ggsave(file="D://cces/lifecycle/attendance_by_married_ci_facet_2014.png", type = "cairo-png", width = 15, height = 15)



evan1 <- cces14 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(child18 ==1 & marstat ==1) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = c("Wht. Evangelical")) %>% 
  mutate(status = c("Married w/Kids"))

evan2 <- cces14 %>% mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = recode(age, "18:25=1; 26:30=2; 31:44=3; 45:54=4; 55:64=5; 65:80 =6; 81:100=7")) %>%  
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else=99")) %>% 
  filter(attend < 10) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(child18 ==2 & marstat ==1) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(attend),
            sd = sd(attend), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = c("Wht. Evangelical")) %>% 
  mutate(status = c("Married w/o Kids"))


marry14 <- bind_rows(evan1, evan2)

marry14 <- marry14 %>% 
  mutate(age2 = as.numeric(age2)) %>% 
  mutate(age2 = recode(age2, "1 = '18-25'; 2 = '26-30'; 3 = '31-44'; 4 = '45-54'; 5 = '55-64'; 6 = '65-80'; 7 = '80 and Over'"))%>% mutate(year = c("2014"))


marry %>% 
  # filter(type == "White Mainline") %>% 
  ggplot(., aes(x = mean, y = age2, group = status, label = status))  +
  geom_point(shape=21, size =4, aes(fill = factor(status)), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(status)), height=0, size = 1, show.legend = FALSE) + 
  # scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey", "red", "green")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + 
  # scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) + 
  theme(text=element_text(size=42, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Religious Attendance", y ="", title = "Average Church Attendance by White, Born Again Protestants", caption = "Data: CCES 2014", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))  +  
  theme(plot.title = element_text(face="bold"))


ggsave(file="D://cces/lifecycle/attendance_by_married_without_kids_2014.png", type = "cairo-png", width = 20, height = 15)


marry <- bind_rows(marry14, marry16)


marry %>% 
  # filter(type == "White Mainline") %>% 
  ggplot(., aes(x = mean, y = age2, group = status, label = status))  +
  geom_point(shape=21, size =4, aes(fill = factor(status)), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(status)), height=0, size = 1, show.legend = FALSE) + 
  # scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey", "red", "green")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + 
  # scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey")) + 
  theme(text=element_text(size=42, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Religious Attendance", y ="", title = "Average Church Attendance by White, Born Again Protestants", caption = "Data: CCES 2014 + 2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))  +  facet_grid(year~.) + 
  theme(plot.title = element_text(face="bold"))

ggsave(file="D://cces/lifecycle/compare_2014_2016.png", type = "cairo-png", width = 20, height = 15)
