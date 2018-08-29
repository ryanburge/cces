imm_ct <- cces16 %>% 
  mutate(imm = recode(immstat, "1:2 =  'Immigrant'; 3 = '1st Gen.'; 4 = '2nd Gen.'; 5 = '3rd Gen.'")) %>%
  filter(imm !=8) %>% 
  group_by(reltrad) %>% 
  ct(imm, commonweight_vv) %>% ungroup(reltrad)
  
font_add_google("Oswald", "font")
showtext_auto()


imm_ct <- imm_ct %>%
  mutate(reltrad = recode(reltrad, "'evangelical'= 'Evangelical';
                          'mainline'= 'Mainline';
                          'catholic'= 'Catholic';
                          'jewish'= 'Jewish';
                          'bprot' = 'Black Protestant';
                          'other' = 'Other Faith';
                          'none' = 'No Faith'"))

imm_ct <- imm_ct %>% 
  mutate(imm = as.factor(imm)) 


imm_ct$imm <- factor(imm_ct$imm, levels = c("Immigrant", "1st Gen.", "2nd Gen.", "3rd Gen."))


imm_ct %>%
  filter(reltrad != "NA") %>% 
  ggplot(., aes(x = imm, y = pct, fill = reltrad)) + geom_col(color = "black") +
  facet_wrap(.~ reltrad, ncol =4) + 
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(text=element_text(size=64, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_aaas() + 
  geom_text(aes(y = pct + .065, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 12, family = "font") +
  labs(x = "Immigration Status", y ="", title = "Immigration Status of Religious Traditions", caption = "Data: CCES 2016")

ggsave("immigrants/immstat_reltrad.png", type = "cairo-png", width = 12)


