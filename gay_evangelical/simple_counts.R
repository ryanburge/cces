cces16 <- cces16 %>% 
  mutate(lgb = recode(sexuality, "2:5=1; else=0")) %>% 
  mutate(evanlgb = lgb + evangelical) %>% 
  mutate(evanlgb = recode(evanlgb, "2=1; else =0"))

cces16 %>% 
  count(lgb, wt = commonweight_vv_lgbt)  %>% 
  mutate(pct = prop.table(n))

cces16 %>%  
  count(evangelical, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n))

cces16 %>% 
  count(evanlgb, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))

cces16 %>% 
  filter(lgb ==1) %>% 
  count(evangelical, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))