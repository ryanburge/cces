ed <- cces16 %>% 
  filter(educ <7) %>% 
  group_by(religpew) %>% summarise(mean = mean(educ),
                                   sd = sd(educ), 
                                   n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 


ed <- ed %>% 
  mutate(religpew = recode(religpew, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))



ed %>% 
  filter(religpew != "12") %>% 
  filter(religpew != "98") %>%  
  filter(religpew != "Orthodox") %>% 
  ggplot(., aes(x= reorder(religpew, - mean), y = mean, fill = religpew)) + geom_col(color = "black") +  
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") +
  bar_rb() + theme(legend.position="none") + scale_fill_npg() +
  labs(y= "Average Education", x = "Religious Tradition", title = "Relationship Between Religion and Education", caption = "Data: CCES 2016", subtitle = "") + 
  scale_y_continuous(limits = c(0,5.4), breaks = c(1,2,3,4,5,6), labels = c("No HS", "HS Grad", "Some College", "2 Yr College", "4 Yr College", "Post-Grad")) +
  theme(plot.title = element_text(family = "Product Sans", size = 46, vjust =2, face = "bold"))


ggsave(file="D://cces/mean_educ_reltrad.png", type = "cairo-png", width = 20, height =12, dpi = 300)


edd <- cces16 %>%  
  filter(educ <7) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(reltrad, educ) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  ungroup(reltrad) %>% 
  mutate(educ = recode(educ, "1 = 'No HS'; 2 ='HS Grad'; 3= 'Some College'; 4= '2 Yr College'; 5 ='4 Yr College'; 6 = 'Post-Grad'")) %>% 
  mutate(reltrad = recode(reltrad, "'bprot' = 'Black Protestant';
                                    'catholic' = 'Catholic';
                                    'evangelical' = 'Evangelical';
                                    'mainline' = 'Mainline';
                                    'jewish' = 'Jewish';
                                    'none' = 'No Faith';
                                    'other' = 'Other Faith'")) 

edd$educ <- factor(edd$educ, levels = c("No HS", "HS Grad", "Some College", "2 Yr College", "4 Yr College", "Post-Grad"))


edd %>% 
  na.omit() %>% 
  ggplot(., aes(x= educ, y = mean, fill = reltrad)) + geom_col(color = "black") + facet_grid(. ~ reltrad)  + 
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") +
  bar_rb() + theme(legend.position="none") + scale_fill_npg() +
  labs(y= "% of Two Party Vote for Trump", x = "Highest Level of Education Obtained", title = "Relationship Between Education and Vote in 2016", caption = "Data: CCES 2016", subtitle = "") +
  scale_y_continuous(limits = c(0, .85), labels = scales::percent) 
 


ggsave(file="D://cces/educ_vote_choice.png", type = "cairo-png", width = 20, height =12, dpi = 300)




  
