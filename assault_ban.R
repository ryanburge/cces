cces16 %>%
  filter(pew_bornagain==1 & religpew ==1) %>% 
  mutate(pid = recode(pid7, "1:3=1; 4=2; 5:7=3; else=99")) %>% 
  filter(pid < 10) %>% 
  filter(CC16_330a != 8) %>% 
  group_by(pid) %>% 
  count(CC16_330a, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_330a == 2) %>% 
  ungroup(pid) %>% 
  mutate(pid = recode(pid, "1= 'Democrats'; 2= 'Indepedents';  3 = 'Republicans'")) %>% 
  mutate(group = c("Evangelicals")) 
  
cces16 %>%
  filter(pew_bornagain==1 & religpew ==1) %>% 
  mutate(pid = recode(pid7, "1:3=1; 4=2; 5:7=3; else=99")) %>% 
  filter(pid < 10) %>% 
  filter(CC16_330b != 8) %>% 
  group_by(pid) %>% 
  count(CC16_330b, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_330b == 2) %>% 
  ungroup(pid) %>% 
  mutate(pid = recode(pid, "1= 'Democrats'; 2= 'Indepedents';  3 = 'Republicans'")) %>% 
  mutate(group = c("Evangelicals")) 

a1 <- cces16 %>%
  filter(pew_bornagain==1 & religpew ==1) %>% 
  mutate(pid = recode(pid7, "1:3=1; 4=2; 5:7=3; else=99")) %>% 
  filter(pid < 10) %>% 
  filter(CC16_330d != 8) %>% 
  group_by(pid) %>% 
  count(CC16_330d, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_330d == 1) %>% 
  ungroup(pid) %>% 
  mutate(pid = recode(pid, "1= 'Democrats'; 2= 'Independents';  3 = 'Republicans'")) %>% 
  mutate(pid = as.character(pid)) %>% 
  mutate(group = c("Evangelicals")) 


a2 <- cces16 %>%
  # filter(pew_bornagain==1 & religpew ==1) %>% 
  mutate(pid = recode(pid7, "1:3=1; 4=2; 5:7=3; else=99")) %>% 
  filter(pid < 10) %>% 
  filter(CC16_330d != 8) %>% 
  group_by(pid) %>% 
  count(CC16_330d, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_330d == 1) %>% 
  ungroup(pid) %>% 
  mutate(pid = recode(pid, "1= 'Democrats'; 2= 'Independents';  3 = 'Republicans'")) %>% 
  mutate(pid = as.character(pid)) %>% 
  mutate(group = c("Entire Sample")) 

a3 <- cces16 %>%
   filter(pew_bornagain==2 & religpew ==1) %>% 
  mutate(pid = recode(pid7, "1:3=1; 4=2; 5:7=3; else=99")) %>% 
  filter(pid < 10) %>% 
  filter(CC16_330d != 8) %>% 
  group_by(pid) %>% 
  count(CC16_330d, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_330d == 1) %>% 
  ungroup(pid) %>% 
  mutate(pid = recode(pid, "1= 'Democrats'; 2= 'Independents';  3 = 'Republicans'")) %>% 
  mutate(pid = as.character(pid)) %>% 
  mutate(group = c("Mainline")) 

a4 <- cces16 %>%
  filter(religpew ==2) %>% 
  mutate(pid = recode(pid7, "1:3=1; 4=2; 5:7=3; else=99")) %>% 
  filter(pid < 10) %>% 
  filter(CC16_330d != 8) %>% 
  group_by(pid) %>% 
  count(CC16_330d, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_330d == 1) %>% 
  ungroup(pid) %>% 
  mutate(pid = recode(pid, "1= 'Democrats'; 2= 'Independents';  3 = 'Republicans'")) %>% 
  mutate(pid = as.character(pid)) %>% 
  mutate(group = c("Catholics")) 

total <- bind_rows(a1, a2, a3, a4) %>% select(-CC16_330d, -n)

total %>% 
  ggplot(., aes(x=pid, y=pct)) + geom_col(aes(fill = pid), color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Party Identification", y= "Percent of Sample", title = "In Favor of Banning Assault Weapons", caption = "Data: CCES 2016" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))   +
  theme(legend.position = "none") + theme(legend.title=element_blank()) + 
  scale_fill_manual(values=c("dodgerblue3", "darkgrey", "firebrick1")) +
  theme(plot.title = element_text(face="bold")) + facet_grid(.~group) 

ggsave(file="assault_ban.png", type = "cairo-png", width = 20, height =12)
