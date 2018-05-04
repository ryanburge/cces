att1 <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  group_by(religpew) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew = recode(religpew, "2 = 'Catholic';
                           3 = 'Mormon';
                           4 = 'Orthodox';
                           5 = 'Jewish';
                           6 = 'Muslim';
                           7 = 'Buddhist';
                           8 = 'Hindu'; 
                           else =99")) %>% 
  filter(religpew != 99)


bap_att <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  group_by(religpew_baptist) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew_baptist = recode(religpew_baptist, "1 = 'Southern Baptist'; 2 = 'ABCUSA'; 4 = 'Ind. Baptist'; 
                                   else =99")) %>% 
  filter(religpew_baptist != 99) %>% 
  rename(religpew = religpew_baptist)

meth_att <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  group_by(religpew_methodist) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew_methodist = recode(religpew_methodist, "1 = 'United Methodist';
                                     else =99")) %>% 
  filter(religpew_methodist != 99) %>% 
  rename(religpew = religpew_methodist)

nd_att <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else =99")) %>% 
  group_by(nd) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew_nondenom = recode(nd, "1:90 = 'Nondenominational';
                                    else =99")) %>% 
  filter(religpew_nondenom != 99) %>% 
  rename(religpew = religpew_nondenom)

luth_att <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  group_by(religpew_lutheran) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew_lutheran = recode(religpew_lutheran, "1 = 'ELCA'; 2 = 'Lutheran - Missouri Synod';
                                    else =99")) %>% 
  filter(religpew_lutheran != 99) %>% 
  rename(religpew = religpew_lutheran)

pres_att <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  group_by(religpew_presby) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew_presby = recode(religpew_presby, "1 = 'PCUSA'; 2 = 'PCA';
                                  else =99")) %>% 
  filter(religpew_presby != 99) %>% 
  rename(religpew = religpew_presby)


pent_att <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  mutate(pent= recode(religpew_pentecost, "1:90=1; else =99")) %>% 
  group_by(pent) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew_pentecost= recode(pent, "1:90 = 'Pentecostal';
                                    else =99")) %>% 
  filter(religpew_pentecost != 99) %>% 
  rename(religpew = religpew_pentecost)


epis_att <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  mutate(pent= recode(religpew_episcop, "1:90=1; else =99")) %>% 
  group_by(pent) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew_episcop= recode(pent, "1:90 = 'Episcopal';
                                  else =99")) %>% 
  filter(religpew_episcop != 99) %>% 
  rename(religpew = religpew_episcop)

ucc_att <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  group_by(religpew_congreg) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew_congreg = recode(religpew_congreg, "1 = 'United Church of Christ';
                                      else =99")) %>% 
  filter(religpew_congreg != 99) %>% 
  rename(religpew = religpew_congreg)





all_att <- bind_rows(att1, bap_att, meth_att, nd_att, luth_att, pres_att, pent_att, epis_att, ucc_att) %>% 
  select(religpew, mean) %>% 
  arrange(-mean) %>% 
  mutate(mean = round(mean, 3))

all_att %>% 
  ggplot(., aes(x= reorder(religpew, mean), y = mean, fill = mean)) + geom_col(color = "black") +
  coord_flip() +
  flip_bar_rb() +
  scale_fill_gradient(low = "#fffbd5", high = "#b20a2c") +
  theme(legend.position="none") + scale_y_continuous(labels = scales::percent) +
  labs(x = "Religious Family", y = "Percent Who Pray Daily or More", title = "Which Religious Group Prays the Most?", caption = "Data: CCES (2016)") +
  geom_text(aes(y = mean - .055, label = paste0(mean*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  theme(plot.title = element_text(hjust = 1, vjust=2.12, size =42)) 

ggsave(file="pray_by_trad_not.png", type = "cairo-png", width = 15, height = 12)

att_2 <- cces16 %>% 
  filter(pew_prayer < 8) %>% 
  mutate(pray = 8 - pew_prayer) %>% 
  mutate(hipray = recode(pray, "6:7=1; else=0")) %>% 
  group_by(religpew) %>% 
  summarise(mean = mean(hipray)) %>% 
  mutate(religpew = recode(religpew, "9 = 'Atheist';
                           10 = 'Agnostic';
                           11 = 'Nothing in Particular';
                           12 = 'Something Else';
                           else =99")) %>% 
  filter(religpew != 99)


att_2 %>% 
  mutate(mean = round(mean, 3)) %>% 
  ggplot(., aes(x= reorder(religpew, mean), y = mean, fill = mean)) + geom_col(color = "black") +
  coord_flip() +
  flip_bar_rb() +
  scale_fill_gradient(low = "#fffbd5", high = "#b20a2c") +
  theme(legend.position="none") + scale_y_continuous(labels = scales::percent) +
  labs(x = "Religious Family", y = "Percent Who Pray Daily or More", title = "Which Religious Group Prays the Most?", caption = "Data: CCES (2016)") +
  geom_text(aes(y = mean + .035, label = paste0(mean*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  theme(plot.title = element_text(hjust = 1, vjust=2.12, size =42)) 

ggsave(file="pray_by_trad_nones.png", type = "cairo-png", width = 15, height = 12)
