
imm_vote <- cces16 %>% 
  mutate(imm = recode(immstat, "1:2 =  'Immigrant'; 3 = '1st Gen.'; 4 = '2nd Gen.'; 5 = '3rd Gen.'")) %>%
  filter(imm !=8) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(reltrad, imm) %>% 
  mutate(trump = recode(CC16_410a, "1=1; 2=0; else =NA")) %>% 
  mean_ci(trump, wt = commonweight_vv_post) %>% 
  ungroup(reltrad, imm)

imm_vote <- imm_vote %>% 
  mutate(reltrad = recode(reltrad, "3 = 'Black Protestant';
                          'catholic' = 'Catholic';
                          'evangelical' = 'Evangelical';
                          'mainline' = 'Mainline';
                          5 = 'Jewish';
                          'none' = 'No Faith';
                          'other' = 'Other Faith';
                          8 = 'Entire Sample'")) 


imm_vote$imm <- factor(imm_vote$imm, levels = c("Immigrant", "1st Gen.", "2nd Gen.", "3rd Gen."))


font_add_google("Oswald", "font")
showtext_auto()

imm_vote %>% 
  filter(reltrad == "Evangelical" | reltrad == "Catholic" | reltrad == "No Faith" | reltrad == "Other Faith") %>% 
  ggplot(., aes(x=imm, y=mean, fill = imm)) + geom_col(color = "black")+ 
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "azure4") +
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme_minimal() +
  theme(legend.position="none") +
  labs(x = "Immigration Status", y = "Percent of Two Party Vote for Trump", title = "Immigration Status and Vote Choice in 2016", caption = "Data: CCES 2016") +
  scale_fill_jama() +
  labs(fill="")  + xlab("") +   
  theme(text=element_text(size=64, family="font")) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(. ~ reltrad, ncol = 2)

ggsave("immigrants/immstat_vote_4trads.png", width =8, type = "cairo-png")