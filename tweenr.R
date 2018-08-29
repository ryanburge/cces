gss %>% 
  filter(year == 1974 | year == 1984 | year == 1994 | year == 2004 | year == 2014 | year == 2016) %>% 
  filter(mainline ==1) %>% 
  filter(partyid < 7) %>% 
  mutate(partyid = as.numeric(partyid), year = as.factor(year)) %>% 
  ggplot(., aes(x= partyid, y =year)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("dodgerblue1", "gray", "firebrick1")) +
  mean_rb() +
  scale_x_continuous(limits = c(-.5,7), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "Self Described Party Identification", y ="", title = "Distribution of Party ID", caption = "Data: GSS 1972-2016", subtitle = "Among Mainliner Protestants")


ggsave(file="D://cces/gss_ridges_mainline.png", type = "cairo-png", width = 18, height =12)


library(tweenr)
library(ggridges)
library(gganimate)


tw <- gss %>% 
  filter(evangelical ==1 & race ==1) %>% 
  arrange(year) %>% 
  mutate(y = 1) %>% 
  select(year, partyid, y, id) %>% 
  rename(x = partyid, y=y, time = year, id = id) %>%
  mutate(ease="linear")

tw_anim <- tween_elements(tw, "time", "id", "ease", nframes = 300)


tw_anim <- tw_anim %>%
  mutate(year = round(time),
         id = .group) %>% 
  mutate(id = as.numeric(id))

tw_anim <- inner_join(tw_anim, evan)

font_add_google("Oswald", "font")

showtext_auto()

tween_chart <- tw_anim %>% 
  filter(x <7) %>% 
  mutate(.frame =as.factor(.frame)) %>% 
  ggplot(., aes(x = x, y = y, frame = .frame)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("dodgerblue1", "gray", "firebrick1")) +
  theme_minimal() +  theme(text=element_text(size=22, family="font")) +
  scale_x_continuous(limits = c(-.5,7), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "Self Described Party Identification", y ="", title = "Distribution of Party ID", caption = "Data: GSS 1972-2016", subtitle = "Among White Evangelicals") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

  
gganimate(tween_chart, title_frame = FALSE, interval = 0.05, "f_tween_FF104.gif")

