
cces16$income <- as.numeric(cut_number(cces16$faminc, 3))


low <- cces16 %>% 
  filter(income ==1) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(pid7 <8) %>%
  mutate(pid7 = as.numeric(pid7), race = to_factor(race)) %>% 
  ggplot(., aes(x = pid7, y = race)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("dodgerblue3", "gray", "firebrick3")) +
  mean_rb() +
  scale_x_continuous(limits = c(-.5,9), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "Self Described Party Identification", y ="", title = "Distribution of Party ID", caption = "Data: CCES 2016", subtitle = "Among Born Again Protestants w/Income Below $50k") +
  theme(plot.title = element_text(family = "Product Sans", size = 34, vjust =2, face = "bold"))


mid <- cces16 %>% 
  filter(income ==2) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(pid7 <8) %>%
  mutate(pid7 = as.numeric(pid7), race = to_factor(race)) %>% 
  ggplot(., aes(x = pid7, y = race)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("dodgerblue3", "gray", "firebrick3")) +
  mean_rb() +
  scale_x_continuous(limits = c(-.5,9), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "Self Described Party Identification", y ="", title = "Distribution of Party ID", caption = "Data: CCES 2016", subtitle = "Among Born Again Protestants w/Income $50k-$100k") +
  theme(plot.title = element_text(family = "Product Sans", size = 34, vjust =2, face = "bold"))


high <- cces16 %>% 
  filter(income ==3) %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  filter(pid7 <8) %>%
  mutate(pid7 = as.numeric(pid7), race = to_factor(race)) %>% 
  ggplot(., aes(x = pid7, y = race)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("dodgerblue3", "gray", "firebrick3")) +
  mean_rb() +
  scale_x_continuous(limits = c(-.5,9), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "Self Described Party Identification", y ="", title = "Distribution of Party ID", caption = "Data: CCES 2016", subtitle = "Among Born Again Protestants w/Income Above $100k") +
  theme(plot.title = element_text(family = "Product Sans", size = 34, vjust =2, face = "bold"))


p <- low + mid + high + plot_layout(ncol = 1)


ggsave(file="D://cces/income_ridges.png", type = "cairo-png", width = 21, height = 34, p)

