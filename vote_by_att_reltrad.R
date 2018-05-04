djt <- cces16 %>% 
  na.omit() %>%  
  filter(CC16_410a ==1) %>% 
  count(reltrad, wt = commonweight_vv_post) %>%
  mutate(pct = prop.table(n)) %>% 
  mutate(candidate = c("Donald Trump"))


hrc <- cces16 %>% 
  na.omit() %>%  
  filter(CC16_410a ==2) %>% 
  count(reltrad, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(candidate = c("Hillary Clinton"))

total <- bind_rows(djt, hrc)

total <- total %>% 
  mutate(reltrad = recode(reltrad, "'bprot' = 'Black Protestant';
                                    'catholic' = 'Catholic';
                                    'evangelical' = 'Evangelical';
                                    'mainline' = 'Mainline';
                                    'jewish' = 'Jewish';
                                    'none' = 'No Faith';
                                    'other' = 'Other Faith'")) 

tibble::enframe(canva_palettes) %>% filter(name == "Subdued & Professional")

total <- total %>% mutate(pct = round(pct,3))

total %>% 
  ggplot(., aes(1, pct)) + geom_col(aes(fill = fct_rev(reltrad)), color = "black") + coord_flip() + 
  flip_bar_rb() + facet_grid(candidate ~. ) +
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
  # scale_fill_brewer(palette = "Set1") +
  labs(x = "", y = "Percent of Each Candidate's Vote Share", title = "What Did the Candidates' Coalition Look Like?", subtitle = "The 'Nones' Were Almost as Important to Clinton, as Evangelicals were to Trump", caption = "Data: CCES 2016") +
  scale_y_continuous(labels = scales::percent) + scale_fill_nejm() + guides(fill = guide_legend(reverse = TRUE)) 


ggsave(file="D://cces/voter_makeup.png", type = "cairo-png", width = 20, height =12)


attend <-cces16 %>%  
  filter(pew_churatd < 7) %>% 
  mutate(attend = 7- pew_churatd) %>% 
  filter(CC16_410a ==1 | CC16_410a ==2) %>% 
  rename(vote16 = CC16_410a) %>% 
  mutate(vote16 = recode(vote16, "1=1; 2=0")) %>% 
  group_by(reltrad, attend) %>% 
  summarise(mean = mean(vote16),
            sd = sd(vote16), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  ungroup(reltrad) %>% 
  mutate(attend = recode(attend, "1 = 'Never'; 2 ='Seldom'; 3= 'Yearly'; 4= 'Monthly'; 5 ='Weekly'; 6 = 'Weekly+'")) %>% 
  mutate(reltrad = recode(reltrad, "'bprot' = 'Black Protestant';
                                    'catholic' = 'Catholic';
                                    'evangelical' = 'Evangelical';
                                    'mainline' = 'Mainline';
                                    'jewish' = 'Jewish';
                                    'none' = 'No Faith';
                                    'other' = 'Other Faith'")) 

attend$attend <- factor(attend$attend, levels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))

  
attend %>% 
  na.omit() %>% 
  ggplot(., aes(x= attend, y = mean, fill = reltrad)) + geom_col(color = "black") + facet_grid(. ~ reltrad)  + 
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") +
  bar_rb() + theme(legend.position="none") + scale_fill_npg() +
  labs(y= "% of Two Party Vote for Trump", x = "Church Attendance", title = "Relationship Between Attendance and Vote in 2016", caption = "Data: CCES 2016", subtitle = "") +
  scale_y_continuous(labels = scales::percent)


ggsave(file="D://cces/attend_vote_choice.png", type = "cairo-png", width = 20, height =12, dpi = 300)



