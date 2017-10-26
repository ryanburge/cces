

abunder <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age <= 35) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_332a)) %>% 
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(abort = as.numeric(CC16_332a)) %>% 
  mutate(abort = recode(abort, "1= 'Pro-Choice';
                        2= 'Pro-Life'")) %>% 
  mutate(age = c("Under 35"), sample = c("White Born Again")) %>% 
  filter(abort != "8") %>% 
  select(abort, weight, age, sample)


abover <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age > 35) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_332a)) %>% 
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(abort = as.numeric(CC16_332a)) %>% 
  mutate(abort = recode(abort, "1= 'Pro-Choice';
                        2= 'Pro-Life'")) %>% 
  mutate(age = c("Over 35"), sample = c("White Born Again")) %>% 
  filter(abort != "8") %>% 
  select(abort, weight, age, sample)


abunder1 <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age <= 35) %>% 
  filter(race ==1) %>% 
  filter(complete.cases(CC16_332a)) %>% 
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(abort = as.numeric(CC16_332a)) %>% 
  mutate(abort = recode(abort, "1= 'Pro-Choice';
                        2= 'Pro-Life'")) %>% 
  mutate(age = c("Under 35"), sample = c("Entire White Sample")) %>% 
  filter(abort != "8") %>% 
  select(abort, weight, age, sample)


abover1 <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age > 35) %>% 
  filter(race ==1) %>% 
  filter(complete.cases(CC16_332a)) %>% 
  count(CC16_332a, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(abort = as.numeric(CC16_332a)) %>% 
  mutate(abort = recode(abort, "1= 'Pro-Choice';
                        2= 'Pro-Life'")) %>% 
  mutate(age = c("Over 35"), sample = c("Entire White Sample")) %>% 
  filter(abort != "8") %>% 
  select(abort, weight, age, sample)

abort <- bind_rows(abunder, abover, abunder1, abover1)

abort %>% 
  filter(abort == "Pro-Choice") %>% 
  ggplot(., aes(x=abort, y=weight, fill = fct_rev(age))) + 
  geom_col( color = "black", width = .85, position = position_dodge(width=.95)) +
  facet_wrap(~ sample, ncol = 2) +
  labs(x= "View of Abortion", y="Percent of Sample", title = "White, Born Again Protestants - Millennial vs. All Other Ages") + 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + 
  scale_fill_brewer(palette = "Dark2") +
  # scale_fill_manual(values=c( "darkgrey","purple", "goldenrod1", "dodgerblue3", "firebrick1")) +
  scale_y_continuous(labels = scales::percent) +  
  theme(plot.title = element_text(face="bold")) + theme(legend.title=element_blank())

ggsave(file="abort_millennials.png", type = "cairo-png", width = 12, height = 12)



