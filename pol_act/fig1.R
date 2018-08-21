cces16 <- cces16 %>% 
  mutate(meet = recode(CC16_417a_1, "1=1; else =0")) %>% 
  mutate(sign = recode(CC16_417a_2, "1=1; else =0")) %>% 
  mutate(vol = recode(CC16_417a_3, "1=1; else =0")) %>% 
  mutate(money = recode(CC16_417a_4, "1=1; else =0")) %>% 
  mutate(blood = recode(CC16_417a_5, "1=1; else =0")) %>% 
  mutate(activity = meet + sign + vol + money + blood)


ct_fun <- function(var, num, relig){
  var <- enquo(var)
  num <- enquo(num)
  relig <- enquo(relig)
  
b1 <- cces16 %>% 
  filter(!! var  == !! num) %>% 
  mean_ci(meet, wt = commonweight_vv_post) %>% 
  mutate(religpew = !! relig ) %>% 
  mutate(act = "Pol. Meeting") %>% 
  mutate(religpew = !! relig )  
  
b2 <- cces16 %>% 
  filter(!! var  == !! num) %>% 
  mean_ci(sign, wt = commonweight_vv_post) %>% 
  mutate(religpew = !! relig ) %>% 
  mutate(act = "Campaign Sign") %>% 
  mutate(religpew = !! relig ) 
  
b3 <- cces16 %>% 
  filter(!! var  == !! num) %>% 
  mean_ci(vol, wt = commonweight_vv_post) %>% 
  mutate(religpew = !! relig ) %>% 
  mutate(act = "Campaign Vol.") %>% 
  mutate(religpew = !! relig ) 

b4 <- cces16 %>% 
  filter(!! var  == !! num) %>% 
  mean_ci(money, wt = commonweight_vv_post) %>% 
  mutate(religpew = !! relig ) %>% 
  mutate(act = "Donate Money") %>% 
  mutate(religpew = !! relig ) 

b5 <- cces16 %>% 
  filter(!! var  == !! num) %>% 
  mean_ci(blood, wt = commonweight_vv_post) %>% 
  mutate(religpew = !! relig ) %>% 
  mutate(act = "Donate Blood") %>% 
  mutate(religpew = !! relig ) 

bind_rows(b1, b2, b3, b4, b5)

}

a2 <- ct_fun(religpew_baptist, 1, "Southern Baptist")
a3 <- ct_fun(religpew_methodist, 1, "United Methodist")
a4 <- ct_fun(religpew_protestant, 3, "Non-denominational")
a5 <- ct_fun(religpew_protestant, 7, "Episcopalian")
a6 <- ct_fun(religpew_lutheran, 1, "ELCA")
a7 <- ct_fun(religpew_lutheran, 2, "Luth. - MO Synod")
a8 <- ct_fun(religpew_presby, 1, "PCUSA")
a9 <- ct_fun(religpew, 2, "Roman Catholic")
a10 <- ct_fun(religpew, 3, "Mormon")
a11 <- ct_fun(religpew, 5, "Jewish")
a12 <- ct_fun(religpew, 6, "Muslim")
a13 <- ct_fun(religpew, 7, "Buddhist")
a14 <- ct_fun(religpew, 8, "Hindu")
a15 <- ct_fun(religpew, 9, "Atheist")
a16 <- ct_fun(religpew, 10, "Agnostic")  
a17 <- ct_fun(religpew, 11, "Nothing in Particular")

mm <- bind_rows(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)


font_add_google("Oswald", "font")
showtext_auto()

mm %>% 
  ggplot(., aes(x= religpew, y= mean, fill = factor(religpew))) +
  geom_col(color = "black") +
  facet_wrap(.~ act, nrow = 5) + 
  theme_minimal() +
  theme(legend.position = "none") +   
  scale_y_continuous(labels = scales::percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  scale_fill_d3(palette = "category20") + 
  theme(text=element_text(size=64, family="font")) +
  theme(axis.text.x = element_text(family = "font", size =34, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "font", size =34)) +
  labs(y = "Percent Engaging in Each Activity", x = "", title = "Which Activities Do People Engage In?", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(face= "bold", size = 56))

  
ggsave("pol_act/figure1_flip.png", type = "cairo-png", width = 10,  height = 10)


