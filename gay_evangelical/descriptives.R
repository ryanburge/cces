
## Making Table 1 #####

ed <- cces16 %>% 
  filter(evangelical ==1 | evanlgb ==1 | lgb ==1) %>% 
  group_by(lgb, evanlgb, evangelical) %>%  
  summarise(mean = mean(educ), 
            sd = sd(educ), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 


race <- cces16 %>% 
  filter(evangelical ==1 | evanlgb ==1 | lgb ==1) %>% 
  group_by(lgb, evanlgb, evangelical) %>% 
  count(race, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(race = to_factor(race)) %>% 
  filter(pct > .035)

cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(evangelical ==1 | evanlgb ==1 | lgb ==1) %>% 
  group_by(lgb, evanlgb, evangelical) %>% 
  summarise(mean = mean(age), 
            sd = sd(age), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 

cces16 %>% 
  filter(faminc < 17) %>% 
  filter(evangelical ==1 | evanlgb ==1 | lgb ==1) %>% 
  group_by(lgb, evanlgb, evangelical) %>% 
  summarise(mean = mean(faminc))


cces16 %>% 
  filter(evangelical ==1 | evanlgb ==1 | lgb ==1) %>%
  group_by(lgb, evanlgb, evangelical) %>% 
  count(gender, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))

cces16 %>% 
  filter(evangelical ==1 | evanlgb ==1 | lgb ==1) %>%
  group_by(lgb, evanlgb, evangelical) %>% 
  count(marstat, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(race = to_factor(marstat)) %>% 
  filter(pct > .035)



cces16 <- cces16 %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else =99"))

cces16 %>% 
  filter(attend !=99) %>% 
  filter(evangelical ==1 | evanlgb ==1 | lgb ==1) %>%
   group_by(lgb, evanlgb, evangelical) %>% 
  summarise(mean = mean(attend), 
                        sd = sd(attend), 
                        n = n()) %>% 
              mutate(se = sd/sqrt(n),
                     lower = mean - qt(1 - (0.05 /2),  n -1) * se,
                     upper = mean + qt(1 - (0.05 /2),  n -1) * se) 


cces16 %>% filter(evangelical ==1) %>% 
  filter(attend == 5| attend ==6) %>% 
  filter(CC16_410a < 10) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n))

cces16 %>% filter(evangelical ==1) %>% 
  filter(attend < 5) %>% 
  filter(CC16_410a < 10) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n))

cces16 %>% filter(evanlgb ==1) %>% 
  filter(attend == 5| attend ==6) %>% 
  filter(CC16_410a < 10) %>% 
  count(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))

cces16 %>% filter(evanlgb ==1) %>% 
  filter(attend < 5) %>% 
  filter(CC16_410a < 10) %>% 
  count(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))

cces16 %>% filter(evanlgb ==1) %>% 
  filter(attend == 5| attend ==6) %>% 
  filter(CC16_410a < 10) %>% 
  count(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))


cces16 %>% filter(evanlgb ==1) %>% 
  filter(relimp1 == 3| relimp1 ==4) %>% 
  filter(CC16_410a < 10) %>% 
  count(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))

cces16 %>% filter(evanlgb ==1) %>% 
  filter(relimp1 == 1| relimp1 ==2) %>% 
  filter(CC16_410a < 10) %>% 
  count(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))

cces16 %>% filter(evanlgb ==1) %>% 
  # filter(relimp1 == 3| relimp1 ==4) %>% 
  # filter(CC16_410a < 10) %>% 
  count(relimp1, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))


