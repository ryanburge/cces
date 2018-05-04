
cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(CC16_415r <101) %>% 
  filter(CC16_415r != "NA") %>% 
  mutate(CC16_415r = as.numeric(CC16_415r), race = to_factor(race)) %>% 
  ggplot(., aes(x = CC16_415r, y = race)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("dodgerblue3", "gray", "firebrick3")) +
  mean_rb() +
  scale_x_continuous(limits = c(0,115), breaks = c(0,50,100), labels = c("All Taxes", "Even Mix", "All Cuts")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "How to Handle Budget Deficit", y ="", title = "Taxes and Spending", caption = "Data: CCES 2016", subtitle = "Among Born Again Protestants")


ggsave(file="D://cces/hispanic/ridge_taxes.png", type = "cairo-png", width = 20, height = 15)


