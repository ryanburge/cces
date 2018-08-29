cces16 %>% 
  filter(evangelical ==1) %>% 
  mutate(imm = recode(immstat, "1:2 =  'Immigrant'; 3 = '1st Gen.'; 4 = '2nd Gen.'; 5 = '3rd Gen.'; else = NA")) %>%
  mutate(att = recode(pew_churatd, "6=0; 5=1; 4=2; 3=3; 2=4; 1=5; else = NA")) %>% 
  mutate(pray = recode(pew_prayer, "7=0; 6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  filter(imm != "NA") %>% 
  mutate(imm = factor(imm, levels = c("Immigrant", "1st Gen.", "2nd Gen.","3rd Gen."))) %>% 
  ggplot(., aes(x= att, y = pray)) +
  geom_jitter(aes(color = factor(imm), alpha = .2)) +
  facet_wrap(. ~ imm, ncol = 2) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size=64, family="font")) +
  scale_color_jama() +
  theme(plot.subtitle = element_text(size = 32)) +
  # theme(axis.text.y = element_text(size = 32)) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5), labels = c("Never", "", "", "", "", "Weekly+")) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6), labels = c("Never", "", "", "", "", "", "Daily+")) +
  labs(x = "Frequency of Church Attendance", y = "Frequency of Prayer", title = "Religious Activity Among Evangelicals", caption = "Data: CCES 2016", subtitle  = "Immigrants Have the Highest Frequency of Prayer and Church Attendance") 

ggsave("immigrants/scatter.png", type = "cairo-png")


cces16 %>% 
  filter(evangelical ==1) %>% 
  mutate(att = recode(pew_churatd, "5=0; 4=0; 3=0; 2=1; 1=1; else = NA")) %>% 
  filter(immstat ==1 | immstat ==2) %>% 
  mutate(trump = recode(CC16_410a, "1=1; 2=0; else= NA")) %>% 
  group_by(att) %>% 
  mean_ci(trump, wt = commonweight_vv_post)
  