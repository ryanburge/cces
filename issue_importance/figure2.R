

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

cces16 <- cces16 %>% 
  mutate(age = 2016-birthyr) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age2 = recode(age, "18:35 = '18-35'; 36:44 = '36-44'; 45:54 = '45-54'; 55:64 = '55-64'; 65:74 = '65-74'; 75:100 = '75 and Older'"))


aaa1 <- cces16 %>% 
  filter(race ==1) %>% 
  filter(evangelical ==1) %>% 
  filter(guncontrol != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(guncontrol), 
            sd = sd(guncontrol), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Gun Control"))


aaa2 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(abortion != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(abortion), 
            sd = sd(abortion), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Abortion"))

aaa3 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(taxes != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(taxes), 
            sd = sd(taxes), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Taxes"))


aaa4 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(immigration != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(immigration), 
            sd = sd(immigration), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Immigration"))


aaa5 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(deficit != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(deficit), 
            sd = sd(deficit), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Budget Deficit")) 

aaa6 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(defense != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(defense), 
            sd = sd(defense), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Def. Spending"))


aaa7 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(ssecurity != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(ssecurity), 
            sd = sd(ssecurity), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Social Security"))



aaa8 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(environment != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(environment), 
            sd = sd(environment), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Environment"))

aaa9 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(jobs != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(jobs), 
            sd = sd(jobs), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Jobs"))

aaa10 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(crime != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(crime), 
            sd = sd(crime), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Crime"))

aaa11 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(natsec != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(natsec), 
            sd = sd(natsec), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Natl. Security"))


aaa12 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(racerel != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(racerel), 
            sd = sd(racerel), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Race Relations"))


aaa13 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(healthcare != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(healthcare), 
            sd = sd(healthcare), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Health Care"))

aaa14 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(gaym != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(gaym), 
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Gay Marriage"))

aaa15 <- cces16 %>% 
  filter(race ==1) %>%
  filter(evangelical ==1) %>% 
  filter(corrupt != 99) %>% 
  group_by(age2) %>% 
  summarise(mean = mean(corrupt), 
            sd = sd(corrupt), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(issue  = c("Corruption"))


all <- bind_df("aaa")

rrr1 <- all %>% 
  filter(age2 == "18-35") %>% 
  arrange(-mean) %>% 
  mutate(rank = seq(1:15))

rrr2 <- all %>% 
  filter(age2 == "36-44") %>% 
  arrange(-mean) %>% 
  mutate(rank = seq(1:15))

rrr3 <- all %>% 
  filter(age2 == "45-54") %>% 
  arrange(-mean) %>% 
  mutate(rank = seq(1:15))


rrr4 <- all %>% 
  filter(age2 == "55-64") %>% 
  arrange(-mean) %>% 
  mutate(rank = seq(1:15))


rrr5 <- all %>% 
  filter(age2 == "65-74") %>% 
  arrange(-mean) %>% 
  mutate(rank = seq(1:15))


rrr6 <- all %>% 
  filter(age2 == "75 and Older") %>% 
  arrange(-mean) %>% 
  mutate(rank = seq(1:15))

rank <- bind_df("rrr")


gg <- newggslopegraph(rank, age2, rank, issue, Title =  "Issue Importance by Age Among White Evangelicals", SubTitle = "1 = Most Important, 15 = Least Important", Caption = "Data: CCES 2016", TitleTextSize = 44, SubTitleTextSize = 34, XTextSize = 34, YTextSize = 10, DataTextSize = 12, CaptionTextSize = 24)

ggsave("images/newslope.png", typ = "cairo-png", gg, width = 10)



