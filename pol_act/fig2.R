

cces16 <- cces16 %>% 
  mutate(att = recode(pew_churatd, "6=0; 5=1; 4=2; 3=3; 2=4; 1=5; else = NA"))


bd_fun <- function(var, num, relig){
  var <- enquo(var)
  num <- enquo(num)
  relig <- enquo(relig)
  
  cces16 %>% 
    na.omit() %>% 
    group_by(att) %>% 
    filter(!! var  == !! num) %>% 
    mean_ci(activity, wt = commonweight_vv_post) %>% 
    mutate(religpew = !! relig )  
  
}


a2 <- bd_fun(religpew_baptist, 1, "Southern Baptist")
a3 <- bd_fun(religpew_methodist, 1, "United Methodist")
a4 <- bd_fun(religpew_protestant, 3, "Non-denominational")
a5 <- bd_fun(religpew_protestant, 7, "Episcopalian")
a6 <- bd_fun(religpew_lutheran, 1, "ELCA")
a7 <- bd_fun(religpew_lutheran, 2, "Luth. - MO Synod")
a8 <- bd_fun(religpew_presby, 1, "PCUSA")
a9 <- bd_fun(religpew, 2, "Roman Catholic")
a10 <- bd_fun(religpew, 3, "Mormon")
a11 <- bd_fun(religpew, 5, "Jewish")
a12 <- bd_fun(religpew, 6, "Muslim")
a13 <- bd_fun(religpew, 7, "Buddhist")
a14 <- bd_fun(religpew, 8, "Hindu")
a15 <- bd_fun(religpew, 9, "Atheist")
a16 <- bd_fun(religpew, 10, "Agnostic")  
a17 <- bd_fun(religpew, 11, "Nothing in Particular")

mm <- bind_rows(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)


font_add_google("Oswald", "font")
showtext_auto()

mm %>% 
  ggplot(., aes(x= att, y= mean, fill = religpew)) +
  geom_col(color = "black") +
  facet_wrap(.~ religpew) + 
  theme_minimal() +
  theme(legend.position = "none") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_y_continuous(limits = c(0,3)) +
  scale_fill_d3(palette = "category20") + 
  theme(text=element_text(size=64, family="font")) +
  theme(axis.text.x = element_text(family = "font", size =48, angle = 45, hjust = 1)) +
  labs(x = "Church Attendance", y = "# of Social/Political Activities", title = "Disengagement from Church is Related to Disengagement from Social Activities", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(face= "bold", size = 56))

ggsave("pol_act/fig2.png", type = "cairo-png", width = 10, height = 10)
  

