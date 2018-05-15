ucc <- cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(religpew_congreg ==1) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(church = "United Church of Christ")

epis <- cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(religpew_protestant ==7) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(church = "Episcopalian")


abc <- cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(religpew_baptist ==2) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(church = "American Baptist")


elca <- cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(religpew_lutheran ==1) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(church = "Evan. Lutheran Church of America")


ml <- bind_rows(ucc, abc, epis, elca)

ml$attend <- factor(ml$attend, levels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))


ml1 <- ml %>% 
  na.omit() %>% 
  ggplot(., aes(x= attend, y = mean, fill = church)) + geom_col(color = "black") + facet_grid(. ~ church)  + 
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") +
  bar_rb() + theme(legend.position="none") + scale_fill_npg() +
  labs(y= "% of Two Party Vote for Trump", x = "Church Attendance", title = "Mainline Traditions", caption = "Data: CCES 2016", subtitle = "") +
  scale_y_continuous(labels = scales::percent)


## Evangeilcal ####

sbc <- cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(religpew_baptist ==1) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(church = "Southern Baptist")

nd <- cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(religpew_protestant ==3) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(church = "Non-Denominational")


pente <- cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(religpew_protestant ==6) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(church = "Pentecostal")


mosyn <- cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(religpew_lutheran ==2 | religpew_lutheran ==3) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(church = "Lutheran - MO or WI")


evan <- bind_rows(sbc, nd, pente, mosyn)

evan$attend <- factor(evan$attend, levels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))


evan1 <- evan %>% 
  na.omit() %>% 
  ggplot(., aes(x= attend, y = mean, fill = church)) + geom_col(color = "black") + facet_grid(. ~ church)  + 
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") +
  bar_rb() + theme(legend.position="none") + scale_fill_npg() +
  labs(y= "% of Two Party Vote for Trump", x = "Church Attendance", title = "Evangelical Traditions", caption = "Data: CCES 2016", subtitle = "") +
  scale_y_continuous(labels = scales::percent)


a <- ml1 + evan1 + plot_layout(ncol =1)

ggsave(file="D://cces/compare_ml_evan_vote_attend.png", type = "cairo-png", width = 21, height = 15, a)


bap <- bind_rows(sbc, abc)

bap$attend <- factor(bap$attend, levels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))


bap1 <- bap %>% 
  na.omit() %>% 
  ggplot(., aes(x= attend, y = mean, fill = church)) + geom_col(color = "black") + facet_grid(. ~ church)  + 
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") +
  bar_rb() + theme(legend.position="none") + scale_fill_npg() +
  labs(y= "% of Two Party Vote for Trump", x = "Church Attendance", title = "Baptist Traditions", caption = "Data: CCES 2016", subtitle = "") +
  scale_y_continuous(labels = scales::percent)

luth <- bind_rows(elca, mosyn)

luth$attend <- factor(luth$attend, levels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))

luth1 <- luth %>% 
  na.omit() %>% 
  ggplot(., aes(x= attend, y = mean, fill = church)) + geom_col(color = "black") + facet_grid(. ~ church)  + 
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") +
  bar_rb() + theme(legend.position="none") + scale_fill_npg() +
  labs(y= "% of Two Party Vote for Trump", x = "Church Attendance", title = "Lutheran Traditions", caption = "Data: CCES 2016", subtitle = "") +
  scale_y_continuous(labels = scales::percent)


b <- bap1 + luth1 + plot_layout(ncol =2)

ggsave(file="D://cces/compare_ml_evan_vote_atten_luth_bap.png", type = "cairo-png", width = 21, height = 15, dpi = 300,  b)


