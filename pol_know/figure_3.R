

know_fun <- function(var, num, relig){
  var <- enquo(var)
  num <- enquo(num)
  relig <- enquo(relig)
  
  cces16 %>% 
    na.omit() %>% 
    filter(pid3 <=3) %>% 
    group_by(pid3) %>% 
    filter(!! var  == !! num) %>% 
    mean_ci(know) %>% 
    mutate(religpew = !! relig ) %>% 
    ungroup(pid3) %>% 
    mutate(pid3 = as.numeric(pid3)) %>% 
    mutate(pid3 = recode(pid3, "1 = '   Democrat   '; 2 = '   Republican   '; 3 = '   Independent   '"))
  
  
}


a2 <- know_fun(religpew_baptist, 1, "Southern Baptist")
a3 <- know_fun(religpew_methodist, 1, "United Methodist")
a4 <- know_fun(religpew_protestant, 3, "Non-denominational")
a5 <- know_fun(religpew_protestant, 7, "Episcopalian")
a6 <- know_fun(religpew_lutheran, 1, "ELCA")
a7 <- know_fun(religpew_lutheran, 2, "Luth. - MO Synod")
a8 <- know_fun(religpew_presby, 1, "PCUSA")
a9 <- know_fun(religpew, 2, "Roman Catholic")
a10 <- know_fun(religpew, 3, "Mormon")
a11 <- know_fun(religpew, 5, "Jewish")
a12 <- know_fun(religpew, 6, "Muslim")
a13 <- know_fun(religpew, 7, "Buddhist")
a14 <- know_fun(religpew, 8, "Hindu")
a15 <- know_fun(religpew, 9, "Atheist")
a16 <- know_fun(religpew, 10, "Agnostic")
a17 <- know_fun(religpew, 11, "Nothing in Particular")

mm <- bind_rows(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

mm %>% 
  ggplot(., aes(x= pid3, y = mean, fill = pid3)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("dodgerblue3", "azure3", "firebrick3")) +
  facet_wrap(~ religpew, ncol = 4) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  theme_minimal() +
  scale_y_continuous(limits =c(0,4.25)) +
  theme(legend.position = "bottom") + 
  theme(legend.title=element_blank()) + 
  theme(text=element_text(size=64, family="font")) +
  theme(plot.subtitle = element_text(size = 36)) +
  theme(plot.title = element_text(face= "bold", size = 66)) +
  labs(title = "Does Political Knowledge Vary by Party ID?", y = "Political Knowledge", x = "" ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("pol_know/figure3.png", type = "cairo-png", height = 10)