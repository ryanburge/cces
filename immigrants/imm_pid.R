imm_pid <- cces16 %>% 
  filter(reltrad == "evangelical") %>% 
  mutate(imm = recode(immstat, "1:2 =  'Immigrant'; 3 = '1st Gen.'; 4 = '2nd Gen.'; 5 = '3rd Gen.'")) %>%
  filter(imm !=8) %>% 
  group_by(imm) %>% 
  mean_ci(pid7)
