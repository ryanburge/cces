att_know <-cces16 %>% 
  mutate(att = recode(pew_churatd, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  group_by(reltrad, att) %>% 
  mean_ci(know) %>% 
  filter(reltrad == "evangelical" | reltrad == "catholic" | reltrad == "none" | reltrad == "mainline" | reltrad == "bprot") %>% 
  ungroup(reltrad)

att_know <- att_know %>% 
  mutate(reltrad = recode(reltrad, "'bprot' = 'Black Protestant';
                          'catholic' = 'Catholic';
                          'evangelical' = 'Evangelical';
                          'mainline' = 'Mainline';
                          5 = 'Jewish';
                          'none' = 'No Faith';
                          6 = 'Other Faith';
                          8 = 'Entire Sample'")) 


att_know2 <-cces16 %>% 
  mutate(att = recode(pew_churatd, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  group_by(att) %>% 
  mean_ci(know) %>% 
  mutate(reltrad = "   Entire Sample   ")
  
att_know <- bind_rows(att_know, att_know2) %>% 
  na.omit() 

font_add_google("Oswald", "font")
showtext_auto()



att_know %>% 
  ggplot(., aes(x= att, y = mean, fill = reltrad)) +
  geom_col(color = "black") +
  facet_wrap(. ~ reltrad) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  scale_x_continuous(limits = c(.5,6.5), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Few Times a Year", "1x-2x/Month", "Once a Week", "Weekly+")) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size=64, family="font")) +
  theme(plot.subtitle = element_text(size = 36)) +
  theme(plot.title = element_text(face= "bold", size = 54)) +
  theme(axis.text.x = element_text(size =34, angle = 45, hjust  =1, vjust = 1)) +
  labs(title = "Does Increased Church Attendance Increase Political Knowledge?", y = "Political Knowledge", x = "" ) +
  scale_fill_d3()
  
ggsave("pol_know/figure5.png", type = "cairo-png")


facet_grid(. ~ reltrad) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  theme_minimal() +
  scale_y_continuous(limits =c(0,4.25)) +
  theme(legend.position = "bottom") + 
  theme(legend.title=element_blank()) + 
  theme(text=element_text(size=64, family="font")) +
  theme(plot.subtitle = element_text(size = 36)) +
  theme(plot.title = element_text(face= "bold", size = 66)) +
  labs(title = "Does Political Knowledge Vary by Party ID?", y = "Political Knowledge", x = "" ) 



ggsave("pol_know/figure4.png", type = "cairo-png")

