cces16 <- cces16 %>% 
  mutate(lgb = recode(sexuality, "2:5=1; else=0"), tt = recode(trans, "1=1; else=0"), lgbt = lgb + tt, lgbt = recode(lgbt, "1:2=1; else=0")) %>% 
  mutate(evanlgbt = lgbt + sexuality, evanlgbt = recode(evanlgbt, "2=1; else =0")) 

g1 <- cces16 %>% select(V101, evangelical, lgbt, evanlgbt) %>% 
  gather(new, x1, evangelical:evanlgbt) %>% 
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
  filter(pct > .05)

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


