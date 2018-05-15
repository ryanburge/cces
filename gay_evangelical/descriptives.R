cces16 <- cces16 %>% 
  mutate(lgb = recode(sexuality, "2:5=1; else=0")) %>% 
  mutate(evanlgb = lgb + evangelical) %>% 
  mutate(evanlgb = recode(evanlgb, "2=1; else =0"))

g1 <- cces16 %>% select(V101, evangelical, lgb, evanlgb) %>% 
  gather(new, x1, evangelical:evanlgb) %>% 
  filter(x1 ==1) %>% select(V101, new)


cces16 <- cces16 %>% left_join(g1)

ed <- cces16 %>% 
  filter(new != "NA") %>% 
  group_by(new) %>% 
  summarise(mean = mean(educ), 
            sd = sd(educ), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 


race <- cces16 %>% 
  filter(new != "NA") %>% 
  group_by(new) %>% 
  count(race, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(race = to_factor(race)) %>% 
  filter(pct > .035)

cces16 %>% 
  filter(new != "NA") %>% 
  mutate(age = 2017 - birthyr) %>% 
  group_by(new) %>% 
  summarise(mean = mean(age), 
            sd = sd(age), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 

cces16 %>% 
  filter(new != "NA") %>% 
  filter(faminc < 17) %>% 
  group_by(new) %>% 
  summarise(mean = mean(faminc))


cces16 %>% 
  filter(new != "NA") %>% 
  group_by(new) %>% 
  count(gender, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))

cces16 %>% 
  filter(new != "NA") %>% 
  group_by(new) %>% 
  count(marstat, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(race = to_factor(marstat)) %>% 
  filter(pct > .035)



cces16 <- cces16 %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else =99"))

cces16 %>% 
  filter(attend !=99) %>% 
   group_by(new) %>% 
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


