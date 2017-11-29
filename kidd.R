cces16$r2 <-to_factor(cces16$race)

a1<- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  count(r2, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n))



ggplot(a1, aes(x=reorder(r2, pct), y=pct)) + geom_col(fill ="darkcyan", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Racial Group", y= "Percent of Sample", title = "Racial Breakdown of Born Again Protestants", caption = "Data: CCES 2016" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=40, family="KerkisSans"))   +
  theme(legend.position = "none") + theme(legend.title=element_blank()) + 
  scale_fill_manual(values=c("dodgerblue3", "darkgrey", "firebrick1")) +
  theme(plot.title = element_text(face="bold")) + coord_flip()

ggsave(file="baprot_racial.png", type = "cairo-png", width = 20, height =12)


cces16 <- cces16 %>% 
  mutate(pid3 = recode(pid7, "1:3= 'Democrat'; 4 = 'Independent'; 5:7 = 'Republican'"))

a1<- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  count(r2, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n))



ggplot(a1, aes(x=reorder(r2, pct), y=pct)) + geom_col(fill ="darkcyan", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Racial Group", y= "Percent of Sample", title = "Racial Breakdown of Born Again Protestants", caption = "Data: CCES 2016" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=40, family="KerkisSans"))   +
  theme(legend.position = "none") + theme(legend.title=element_blank()) + 
  scale_fill_manual(values=c("dodgerblue3", "darkgrey", "firebrick1")) +
  theme(plot.title = element_text(face="bold")) + coord_flip()

ggsave(file="baprot_racial.png", type = "cairo-png", width = 20, height =12)



cces16 <- cces16 %>% 
  mutate(pid7 = as.numeric(pid7)) %>% 
  mutate(pid3 = recode(pid7, "1:3= 'Democrat'; 4 = 'Independent'; 5:7 = 'Republican'"))

a2<- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  count(pid3, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(pid3 != "8") %>% 
  filter(pid3 != "98") %>% 
  filter(pid3 != "99")

a2 %>% 
  ggplot(., aes(x=pid3, y=pct, fill = pid3)) + geom_col(color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Self Described Party Identity", y= "Percent of Sample", title = "Partisan Breakdown of Born Again Protestants", caption = "Data: CCES 2016" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=40, family="KerkisSans"))   +
  theme(legend.position = "none") + theme(legend.title=element_blank()) + 
  scale_fill_manual(values=c("dodgerblue3", "darkgrey", "firebrick1")) +
  theme(plot.title = element_text(face="bold")) 

ggsave(file="baprot_pid3.png", type = "cairo-png", width = 20, height =12)



