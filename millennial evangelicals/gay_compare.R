

gayunder <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age <= 35) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  count(CC16_335, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(gay = as.numeric(CC16_335)) %>% 
  mutate(gay = recode(gay, "1= 'Favor';
                      2= 'Oppose'")) %>% 
  ungroup(CC16_410a) %>% 
  mutate(age = c("Under 35"), sample = c("White Born Again")) %>% 
  filter(gay != "8") %>%
  select(gay, weight, age, sample)

gayover <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age > 35) %>% 
  filter(race ==1 & pew_bornagain ==1 & religpew ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  count(CC16_335, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(gay = as.numeric(CC16_335)) %>% 
  mutate(gay = recode(gay, "1= 'Favor';
                      2= 'Oppose'")) %>% 
  ungroup(CC16_410a) %>% 
  mutate(age = c("Over 35"), sample = c("White Born Again")) %>% 
  filter(gay != "8") %>%
  select(gay, weight, age, sample)




gayunder1 <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age <= 35) %>% 
  filter(race ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  count(CC16_335, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(gay = as.numeric(CC16_335)) %>% 
  mutate(gay = recode(gay, "1= 'Favor';
                      2= 'Oppose'")) %>% 
  ungroup(CC16_410a) %>% 
  mutate(age = c("Under 35"), sample = c("Entire White Sample")) %>% 
  filter(gay != "8") %>%
  select(gay, weight, age, sample)

gayover1 <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  filter(age > 35) %>% 
  filter(race ==1) %>% 
  filter(complete.cases(CC16_335)) %>% 
  count(CC16_335, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(gay = as.numeric(CC16_335)) %>% 
  mutate(gay = recode(gay, "1= 'Favor';
                      2= 'Oppose'")) %>% 
  ungroup(CC16_410a) %>% 
  mutate(age = c("Over 35"), sample = c("Entire White Sample")) %>% 
  filter(gay != "8") %>%
  select(gay, weight, age, sample)

gay <- bind_rows(gayunder, gayover, gayunder1, gayover1)


gay %>% 
  filter(gay == "Favor") %>% 
  ggplot(., aes(x=gay, y=weight, fill = fct_rev(age))) + 
  geom_col( color = "black", width = .85, position = position_dodge(width=.95)) +
  facet_wrap(~ sample, ncol = 2) +
  labs(x= "View of Gay Marriage", y="Percent of Sample", title = "White, Born Again Protestants - Millennial vs. All Other Ages") + 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + 
  scale_fill_brewer(palette = "Dark2") +
  # scale_fill_manual(values=c( "darkgrey","purple", "goldenrod1", "dodgerblue3", "firebrick1")) +
  scale_y_continuous(labels = scales::percent) +  
  theme(plot.title = element_text(face="bold")) + theme(legend.title=element_blank())

ggsave(file="gay_millennials.png", type = "cairo-png", width = 12, height = 12)

