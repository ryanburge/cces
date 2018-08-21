

map_fun <- function(var, num, relig){
  var <- enquo(var)
  num <- enquo(num)
  relig <- enquo(relig)
  
  cces16 %>% 
    filter(!! var  == !! num) %>% 
    ggplot(., aes(x = att, y = activity)) +
    geom_jitter(alpha = .2) + geom_smooth(method = lm) +
    scale_x_continuous(limits = c(0,5), breaks = c(0,1,2,3,4,5), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) + 
    theme(text=element_text(size=24, family="font")) +
    labs(x = "Church Attendance", y = "Social Activity") +
    theme(plot.title = element_text(size = 48, face = "bold"))
    
  
}



a2 <- map_fun(religpew_baptist, 1) + labs(subtitle = "Southern Baptist -  Estimate = .086*", title = "Correlation Between Attendance and Activity") 
a3 <- map_fun(religpew_methodist, 1) + labs(subtitle = "United Methodist -  Estimate = .084*")
a4 <- map_fun(religpew_protestant, 3) + labs(subtitle = "Non-denominational -  Estimate = .005")
a5 <- map_fun(religpew_protestant, 7) + labs(subtitle = "Episcopalian -  Estimate = .091*")
a6 <- map_fun(religpew_lutheran, 1) + labs(subtitle = "ELCA -  Estimate = .0133*")
a7 <- map_fun(religpew_lutheran, 2) + labs(subtitle = "Luth. - MO Synod -  Estimate = .061")
a8 <- map_fun(religpew_presby, 1) + labs(subtitle = "PCUSA -  Estimate = .114*")
a9 <- map_fun(religpew, 2) + labs(subtitle = "Roman Catholic-  Estimate = .134*")
a10 <- map_fun(religpew, 3) + labs(subtitle = "Mormon -  Estimate = .147*") 
a11 <- map_fun(religpew, 5) + labs(subtitle = "Jewish -  Estimate = .121*")
a12 <- map_fun(religpew, 6) + labs(subtitle = "Muslim -  Estimate = .096*")
a13 <- map_fun(religpew, 7) + labs(subtitle = "Buddhist -  Estimate = .981")
a14 <- map_fun(religpew, 8) + labs(subtitle = "Hindu -  Estimate = .049")
a15 <- map_fun(religpew, 9) + labs(subtitle = "Atheist -  Estimate = -.012")
a16 <- map_fun(religpew, 10)   + labs(subtitle = "Agnostic -  Estimate = .043*")
a17 <- map_fun(religpew, 11) + labs(subtitle = "Nothing in Particular -  Estimate = .009")

all <-  a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17

ggsave("pol_act/figure3.png", type = "cairo-png", all)

cor_fun <- function(var, num, relig){
  var <- enquo(var)
  num <- enquo(num)
  relig <- enquo(relig)
  
  cces16 %>% 
    filter(att != "NA") %>% 
    filter(activity != "NA") %>% 
    filter(!! var  == !! num) %>% 
    summarise(cor = cor(att, activity)) %>% 
    mutate(group =!! relig)
  
}


a2 <- cor_fun(religpew_baptist, 1, "Southern Baptist")
a3 <- cor_fun(religpew_methodist, 1, "United Methodist")
a4 <- cor_fun(religpew_protestant, 3, "Non-denominational")
a5 <- cor_fun(religpew_protestant, 7, "Episcopalian")
a6 <- cor_fun(religpew_lutheran, 1, "ELCA")
a7 <- cor_fun(religpew_lutheran, 2, "Luth. - MO Synod")
a8 <- cor_fun(religpew_presby, 1, "PCUSA")
a9 <- cor_fun(religpew, 2, "Roman Catholic")
a10 <- cor_fun(religpew, 3, "Mormon")
a11 <- cor_fun(religpew, 5, "Jewish")
a12 <- cor_fun(religpew, 6, "Muslim")
a13 <- cor_fun(religpew, 7, "Buddhist")
a14 <- cor_fun(religpew, 8, "Hindu")
a15 <- cor_fun(religpew, 9, "Atheist")
a16 <- cor_fun(religpew, 10, "Agnostic")  
a17 <- cor_fun(religpew, 11, "Nothing in Particular")


mm <- bind_rows(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

pval_fun <- function(var, num, relig){
  var <- enquo(var)
  num <- enquo(num)
  relig <- enquo(relig)

df <- cces16 %>% 
  filter(att != "NA") %>% 
  filter(activity != "NA") %>% 
  filter(!! var  == !! num) 

cor.test(df$att, df$activity) %>% 
  tidy() %>% 
  mutate(group = !! relig)

}



a2 <- pval_fun(religpew_baptist, 1, "Southern Baptist")
a3 <- pval_fun(religpew_methodist, 1, "United Methodist")
a4 <- pval_fun(religpew_protestant, 3, "Non-denominational")
a5 <- pval_fun(religpew_protestant, 7, "Episcopalian")
a6 <- pval_fun(religpew_lutheran, 1, "ELCA")
a7 <- pval_fun(religpew_lutheran, 2, "Luth. - MO Synod")
a8 <- pval_fun(religpew_presby, 1, "PCUSA")
a9 <- pval_fun(religpew, 2, "Roman Catholic")
a10 <- pval_fun(religpew, 3, "Mormon")
a11 <- pval_fun(religpew, 5, "Jewish")
a12 <- pval_fun(religpew, 6, "Muslim")
a13 <- pval_fun(religpew, 7, "Buddhist")
a14 <- pval_fun(religpew, 8, "Hindu")
a15 <- pval_fun(religpew, 9, "Atheist")
a16 <- pval_fun(religpew, 10, "Agnostic")  
a17 <- pval_fun(religpew, 11, "Nothing in Particular")
mm <- bind_rows(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) %>% select(estimate, statistic, p.value, group) %>% mutate(p.value = round(p.value,3))

