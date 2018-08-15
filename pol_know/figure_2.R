

group_fun <- function(var, num, relig){
  var <- enquo(var)
  num <- enquo(num)
  relig <- enquo(relig)
  
  cces16 %>% 
    na.omit() %>% 
    filter(!! var  == !! num) %>% 
    mutate(gg = !! relig )
  
  
}

a2 <- group_fun(religpew_baptist, 1, "Southern Baptist")
a3 <- group_fun(religpew_methodist, 1, "United Methodist")
a4 <- group_fun(religpew_protestant, 3, "Non-denominational")
a5 <- group_fun(religpew_protestant, 7, "Episcopalian")
a6 <- group_fun(religpew_lutheran, 1, "ELCA")
a7 <- group_fun(religpew_lutheran, 2, "Luth. - MO Synod")
a8 <- group_fun(religpew_presby, 1, "PCUSA")
a9 <- group_fun(religpew, 2, "Roman Catholic")
a10 <- group_fun(religpew, 3, "Mormon")
a11 <- group_fun(religpew, 5, "Jewish")
a12 <- group_fun(religpew, 6, "Muslim")
a13 <- group_fun(religpew, 9, "Atheist")
a14 <- group_fun(religpew, 10, "Agnostic")
a15 <- group_fun(religpew, 11, "Nothing in particular")
a16 <- cces16 %>% 
  filter(bprot ==1) %>% 
  mutate(gg = "Black Protestants")

cc <- bind_rows(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

library(patchwork)
library(purrr)
library(glue)


a <- map(
  c("Southern Baptist", "United Methodist", "Non-denominational", "Episcopalian", "ELCA", "Luth. - MO Synod", "PCUSA", "Black Protestant", "Catholic", "Mormon", "Jewish", "Muslim", "Atheist", "Agnostic", "Nothing in Particular"), 
  ~cc %>%
    filter(gg == .x) %>%
    ggplot() +
    aes(educ, know) +
    geom_jitter(alpha = .2) +
    geom_smooth(method = lm) +
    labs(
      title = glue("{.x}")
    )
) %>%
  reduce(`+`)

ggsave("ed_pk.png", type = "cairo-png", a)


cces16 <- cces16 %>% 
  mutate(income = recode(faminc, "97:98 = NA"))

cc %>% 
  group_by(gg) %>% 
  mean_ci(educ) %>% 
  arrange(-mean)

cces16 <- cces16 %>% 
  mutate(black = recode(race, "2=1; else =0"))

reg1 <- lm(know ~ income + educ + factor(black), data = cces16)
r1 <- tidy(reg1)


cc <- cc %>% 
  mutate(income = recode(faminc, "97:98 = NA"))

tt<- cc %>% 
  group_by(gg) %>% 
  summarise(inc = mean(income, na.rm = TRUE), ed = mean(educ))

rr <- cc %>% 
  group_by(gg) %>% 
  ct(race) %>% 
  filter(race ==2)

full <- left_join(rr, tt)

full <- full %>% 
  mutate(estimate =  (pct*-.329) + (inc*.067) + (ed*.202) + 2.049 ) %>% 
  select(-race, -n) %>% 
  rename(pct_blk = pct) %>% 
  arrange(-estimate)

merge <- full %>% 
  select(gg, estimate) %>% 
  rename(religpew= gg)

reg_g <- left_join(graph, merge) %>% 
  select(religpew, mean, estimate) %>% 
  filter(religpew != "Buddhist") 

# reg_g <- reg_g %>% 
#   filter(religpew != "Buddhist") %>% 
#   select(-diff) %>% 
#   melt()


reg_g <- reg_g %>% 
  mutate(diff = estimate - mean) %>% 
  mutate(diff = round(diff, 2))

reg_g %>% 
  ggplot(., aes(x= estimate, xend = mean, y = reorder(religpew, mean))) +
  geom_dumbbell(colour_x = "firebrick2", colour_xend = "darkorchid",  size = .75, size_x =  1.75, size_xend = 1.75 ) +
  theme_minimal() +
  labs(x= "Political Knowledge Scale", y = "Religious Tradition", title = "Estimating Political Knowledge", caption = "Data: CCES 2016", subtitle = "Estimate based on Education, Income, and an African-American Dichotomous Variable")+
  scale_fill_d3(palette = "category20") +
  scale_color_d3(palette = "category20") + 
  theme(text=element_text(size=64, family="font")) +
  theme(plot.subtitle = element_text(size = 36)) +
  theme(plot.title = element_text(face= "bold", size = 66)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) + 
  geom_text(data = filter(reg_g, religpew == "Episcopalian"), aes(x= estimate, y = religpew, label = "Predicted Knowledge"), vjust = -.75, family = "font", size = 8) +
  geom_text(data = filter(reg_g, religpew == "Episcopalian"), aes(x= mean, y = religpew, label = "Actual Knowledge"), vjust = -.75, family = "font", size = 8) +
  geom_text(data = reg_g, aes(x=estimate, y=religpew, label= round(estimate, 1)), size=8, vjust=2.0, family="font") +
  geom_text(data = reg_g, aes(x=mean, y=religpew, label= round(mean, 1)), size=8, vjust=2.0, family="font") + 
  geom_rect(data=reg_g, aes(xmin=4.0, xmax=4.1, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=reg_g, aes(label=diff, y=religpew, x=4.05), fontface="bold", size=10, family="font") +
  geom_text(data=filter(reg_g, religpew=="Episcopalian"), aes(x=4.05, y=religpew, label="DIFF"), size=10, fontface="bold", family="font", vjust = -.65)
  
ggsave("pol_know/figure2.png", type = "cairo-png", width = 10)



