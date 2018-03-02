nd <- cces16 %>% 
  filter(religpew_protestant ==3) %>% 
  filter(pew_churatd !=8) %>% 
  mutate(attend = 8 - pew_churatd) %>% 
  count(attend) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Nondenominational"))


ridge <- cces16 %>% 
  filter(pew_churatd <7) %>% 
  filter(religpew_protestant ==3) %>% 
  mutate(attend = 7 - pew_churatd) %>% 
  mutate(group = c("Nondenominational")) %>% 
  select(group, attend)

ridge2 <- cces16 %>% 
  filter(pew_churatd <7) %>% 
  filter(religpew_baptist ==1) %>% 
  mutate(attend = 7 - pew_churatd) %>% 
  mutate(group = c("Southern Baptist")) %>% 
  select(group, attend)
 
ridge3 <- cces16 %>% 
  filter(pew_churatd <7) %>% 
  filter(religpew_methodist ==1) %>% 
  mutate(attend = 7 - pew_churatd) %>% 
  mutate(group = c("United Methodist")) %>% 
  select(group, attend)

rplot <- bind_rows(ridge, ridge2, ridge3)


my.cols <- brewer.pal(7, "YlGnBu")


rplot %>% 
  mutate(attend = as.numeric(attend), group  = as.factor(group)) %>% 
  ggplot(., aes(x=attend, y= group)) +
  geom_density_ridges_gradient(aes(fill = ..x.., alpha = .2), scale =3, size = .03, color = "black", alpha = .4) +
  scale_fill_gradientn(colours = c("#ffffb2", "#FD8D3C", "#225EA8")) +
  mean_rb() +
  scale_x_continuous(limits = c(-.5,9), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "Self Described Party Identification", y ="", title = "Distribution of Church Attendance", caption = "Data: CCES 2016") +theme(plot.title = element_text(size=30))

ggsave(file="D://cces/ndsbc/ridges.png", type = "cairo-png", width = 21, height = 15)

test <- rplot %>% group_by(group, attend) %>% count() %>% ungroup(group)

rplot %>% 
  mutate(attend = as.numeric(attend), group  = as.factor(group)) %>% 
  ggplot(., aes(x=attend, y=group, height = ..density.., fill = group, alpha =.4)) + geom_density_ridges(stat = "density") +
  mean_rb() +
  scale_x_continuous(limits = c(-.5,9), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "Self Described Church Attendance", y ="", title = "Distribution of Church Attendance", caption = "Data: CCES 2016") +theme(plot.title = element_text(size=64))

ggsave(file="D://cces/ndsbc/ridges2.png", type = "cairo-png", width = 21, height = 15)


