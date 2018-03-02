sbcnd <- read_csv("sbcnd.csv") %>% select(-xx)

sbcnd <- sbcnd %>% 
  mutate(sbcpct = sbc/prot) %>% 
  mutate(ndpct = nd/prot) %>% 
  mutate(umcpct = umc/prot) %>% 
  mutate(prespct = pres/prot) %>% 
  mutate(elcapct = elca/prot) %>% 
  select(year, sbcpct, ndpct, umcpct, prespct, elcapct) %>% 
  melt(id  = c("year")) %>% 
  mutate(variable = recode(variable, "'sbcpct' = 'Southern Baptist'; 'ndpct' = 'Nondenominational'; 'umcpct' = 'United Methodist'; 'prespct' = 'PCUSA'; 'elcapct' = 'ELCA'"))

sbcnd %>% 
  ggplot(., aes(x=year, y=value, label = variable, color= variable, group = variable)) + 
  geom_line(size =2) + long_rb() +
  scale_y_continuous(labels = scales::percent) +
  geom_point(colour = "black", size =2, shape =21, stroke =1.5, show.legend = F) +
  scale_color_brewer(palette = "Set1") + theme(legend.text=element_text(size=36)) + theme(plot.title = element_text(size=48)) +
  labs(x= "Year", y = "Percent of the All Protestants", title = "Nondenominational Christians Are Growing", caption = "Data: CCES (2008-2016)") +
  annotate("text", x = 2015, y = .22, label = "Nondenominational", size = 10, family = "Product Sans") +
  annotate("text", x = 2015, y = .1375, label = "United Methodist", size = 10, family = "Product Sans") +
  annotate("text", x = 2015, y = .115, label = "Southern Baptist", size = 10, family = "Product Sans") +
  annotate("text", x = 2015, y = .055, label = "ELCA", size = 10, family = "Product Sans") +
  annotate("text", x = 2015, y = .03, label = "PCUSA", size = 10, family = "Product Sans")

ggsave(file="D://cces/nd_long.png", type = "cairo-png", width = 21, height = 15)


