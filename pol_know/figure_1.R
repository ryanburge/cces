
first <- cces16 %>% 
  na.omit() %>% 
  filter(state != "Nebraska") %>% 
  mutate(religpew = to_factor(religpew)) %>% 
  group_by(religpew) %>% 
  mean_ci(know) %>% 
  arrange(-mean) %>% 
  filter(religpew == "Atheist" | religpew == "Agnostic" |  religpew  == "Jewish" | religpew == "Mormon" | religpew == "Roman Catholic" | religpew == "Nothing in particular" | religpew == "Buddhist" | religpew == "Muslim")



know_fun <- function(var, num, relig){
  var <- enquo(var)
  num <- enquo(num)
  relig <- enquo(relig)
  
  cces16 %>% 
    na.omit() %>% 
    filter(!! var  == !! num) %>% 
    mean_ci(know) %>% 
    mutate(religpew = !! relig )
  
  
}


a2 <- know_fun(religpew_baptist, 1, "Southern Baptist")
a3 <- know_fun(religpew_methodist, 1, "United Methodist")
a4 <- know_fun(religpew_protestant, 3, "Non-denominational")
a5 <- know_fun(religpew_protestant, 7, "Episcopalian")
a6 <- know_fun(religpew_lutheran, 1, "ELCA")
a7 <- know_fun(religpew_lutheran, 2, "Luth. - MO Synod")
a8 <- know_fun(religpew_presby, 1, "PCUSA")


cces16 %>% 
  na.omit() %>% 
  mutate(pid3 = to_factor(pid3)) %>% 
  # group_by(pid3) %>% 
  mean_ci(know) %>% 
  mutate(religpew = "Entire Sample")


a9 <- cces16 %>% 
  na.omit() %>% 
  group_by(reltrad) %>% 
  mean_ci(know) %>% 
  filter(reltrad == "bprot") %>% 
  mutate(religpew = recode(reltrad, "'bprot' = 'Black Protestants'")) %>% 
  select(-reltrad)

graph <- bind_rows(first, a2, a3, a4, a5, a6, a7, a8, a9) %>% 
  arrange(-mean)

font_add_google("Oswald", "font")
showtext_auto()

graph %>% 
  ggplot(., aes(x= mean,  y = reorder(religpew, mean))) +
  geom_point(shape=21, size =4, aes(fill = factor(religpew)), show.legend = FALSE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(religpew)), height=0, size = 1, show.legend = FALSE) +
  theme_minimal()+
  labs(x= "Political Knowledge Scale", y = "Religious Tradition", title = "Which Religious Groups Have the Most Political Knowledge?", caption = "Data: CCES 2016", subtitle = "Knowledge Assessed Through Five Questions About Federal/State Government")+
  geom_vline(xintercept = 3.3, linetype="dashed") +
  scale_fill_d3(palette = "category20") +
  scale_color_d3(palette = "category20") + 
  theme(text=element_text(size=64, family="font")) +
  annotate("text", x= 3.1, y = 15, label = "Sample Average:", size = 18, family = "font") +
  annotate("text", x= 3.1, y = 14, label = "3.3", size = 18, family = "font") +
  theme(plot.subtitle = element_text(size = 36)) +
  theme(plot.title = element_text(face= "bold", size = 66))


ggsave("pol_know/figure1.png", type = "cairo-png", width = 10)