

a1 <- cces16 %>% 
  filter(race ==3) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  count(CC16_332f, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Evangelicals")) %>% 
  filter(CC16_332f ==1)
 
a2 <-cces16 %>% 
  filter(race ==3) %>% 
  count(CC16_332f, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Entire Sample")) %>% 
  filter(CC16_332f ==1)

a3 <-cces16 %>% 
  filter(race ==1) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  count(CC16_332f, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Evangelicals")) %>% 
  filter(CC16_332f ==1)


a4 <-cces16 %>% 
  filter(race ==1) %>% 
  count(CC16_332f, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Entire Sample")) %>% 
  filter(CC16_332f ==1)

ab <- bind_rows(a1, a2, a3, a4) %>% mutate(issue= c("Pro-Life")) %>% select(issue, group, pct)



a1 <- cces16 %>% 
  filter(race ==3) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  count(CC16_335, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Evangelicals")) %>% 
  filter(CC16_335 ==2)

a2 <-cces16 %>% 
  filter(race ==3) %>% 
  count(CC16_335, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Entire Sample")) %>% 
  filter(CC16_335 ==2)

a3 <-cces16 %>% 
  filter(race ==1) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  count(CC16_335, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Evangelicals")) %>% 
  filter(CC16_335 ==2)


a4 <-cces16 %>% 
  filter(race ==1) %>% 
  count(CC16_335, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Entire Sample")) %>% 
  filter(CC16_335 ==2)

ssm <- bind_rows(a1, a2, a3, a4) %>% mutate(issue= c("Oppose SSM")) %>% select(issue, group, pct)

com <- bind_rows(ab, ssm)

ab$facet <- c("Hispanic", "Hispanic", "White", "White")

ab <- ab %>% mutate(pct = round(pct,3))

a1 <- ab %>% 
  ggplot(., aes(x=group, y = pct, fill = group)) + geom_col(color = "black") + bar_rb() +
  theme(axis.text.x = element_text(family = "Product Sans", size =24)) +
  scale_y_continuous(labels = scales::percent) +  
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "", y = "Percent Who Are Opposed", title = "Abortion - Any Reason") +
  theme(plot.title = element_text(size=42)) +
  scale_fill_brewer(palette = "Dark2") + theme(legend.position="none") + 
  facet_grid(.~ facet)

ggsave(file="D://cces/hispanic/abort_hisp.png", type = "cairo-png", width = 15, height = 12)

ssm$facet <- c("Hispanic", "Hispanic", "White", "White")

ssm <- ssm %>% mutate(pct = round(pct,3))

a2 <- ssm %>% 
  ggplot(., aes(x=group, y = pct, fill = group)) + geom_col(color = "black") + bar_rb() +
  theme(axis.text.x = element_text(family = "Product Sans", size =24)) +
  scale_y_continuous(labels = scales::percent) +  
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "", y = "Percent Who Are Opposed", title = "Same Sex Marriage") +
  theme(plot.title = element_text(size=42)) +
  scale_fill_brewer(palette = "Dark2") + theme(legend.position="none") + 
  facet_grid(.~ facet)

ggsave(file="D://cces/hispanic/ssm_hisp.png", type = "cairo-png", width = 15, height = 12)


a <- a1 + a2 

ggsave(file="D://cces/hispanic/patchwork_gay_abort.png", type = "cairo-png", width = 21, height = 15, a)



