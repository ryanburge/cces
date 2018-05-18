cces16 <- cces16 %>% 
  mutate(gay = recode(sexuality, "2:5=1; else =0")) %>% 
  mutate(lgbevan = gay + evangelical) %>% 
  mutate(lgbevan = recode(lgbevan, "2=1; else=0"))



g1 <- cces16 %>% select(V101, evangelical, gay, lgbevan) %>% 
  gather(new, x1, evangelical:lgbevan) %>% 
  filter(x1 ==1) %>% select(V101, new) 

cces16 <- left_join(cces16, g1)



cces16 <- cces16 %>% 
  mutate(guncontrol = recode(CC16_301a, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(abortion = recode(CC16_301b, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(taxes = recode(CC16_301c, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(immigration = recode(CC16_301d, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(deficit = recode(CC16_301e, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(defense = recode(CC16_301f, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(ssecurity = recode(CC16_301g, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(environment = recode(CC16_301h, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(jobs = recode(CC16_301i, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(crime = recode(CC16_301j, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(natsec = recode(CC16_301k, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(racerel = recode(CC16_301l, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(healthcare = recode(CC16_301m, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(gaym = recode(CC16_301n, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(corrupt = recode(CC16_301o, "1=5; 2=4; 3=3; 4=2; 5=1; else=99"))

a1 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(guncontrol != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(guncontrol), 
            sd = sd(guncontrol), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Gun Control"))


a2 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(abortion != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(abortion), 
            sd = sd(abortion), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Abortion"))

a3 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(taxes != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(taxes), 
            sd = sd(taxes), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Taxes"))


a4 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(immigration != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(immigration), 
            sd = sd(immigration), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Immigration"))


a5 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(deficit != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(deficit), 
            sd = sd(deficit), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Budget Deficit")) 

a6 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(defense != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(defense), 
            sd = sd(defense), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Defense Spending"))


a7 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(ssecurity != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(ssecurity), 
            sd = sd(ssecurity), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Social Security"))



a8 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(environment != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(environment), 
            sd = sd(environment), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Environment"))

a9 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(jobs != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(jobs), 
            sd = sd(jobs), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Jobs"))

a10 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(crime != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(crime), 
            sd = sd(crime), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Crime"))

a11 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(natsec != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(natsec), 
            sd = sd(natsec), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("National Security"))


a12 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(racerel != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(racerel), 
            sd = sd(racerel), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Race Relations"))


a13 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(healthcare != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(healthcare), 
            sd = sd(healthcare), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Health Care"))

a13 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(gaym != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(gaym), 
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Gay Marriage"))

a13 <- cces16 %>% 
  filter(new != "NA") %>% 
  filter(corrupt != 99) %>% 
  group_by(new) %>% 
  summarise(mean = mean(corrupt), 
            sd = sd(corrupt), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Government Corruption"))


total <-  bind_rows(a1, a2, a3,  a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) %>% 
          mutate(group = recode(new, "'evangelical' = 'Evangelical'; 'lgbevan' = 'Evangelical + LGB'; 'gay' = 'LGB'")) %>% 
          select(-new) %>% 
          mutate(issue = as.factor(issue))


total %>% 
  ggplot(., aes(y=mean, x= fct_reorder(issue, mean), color = group)) +
  geom_point(position=position_dodge(width=0.75), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.75), size = 1) +
  coord_flip() +
  mean_rb() +
  labs(title = "Issue Importance", x = "Issue Area", y = "Level of Importance") +
  scale_y_continuous(limits = c(2.5,5.25), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_color_npg()

ggsave(file="D://cces/gay_evangelical/issue_importance.png", type = "cairo-png", width = 20, height =12, dpi = 100)







