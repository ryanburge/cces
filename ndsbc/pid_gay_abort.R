nd <- cces16 %>% 
  filter(religpew_protestant ==3) %>% 
  filter(pid7 < 8) %>% 
  summarise(mean = mean(pid7), 
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(group = c("Nondenominational"))

sbc <- cces16 %>% 
  filter(religpew_baptist ==1) %>% 
  filter(pid7 < 8) %>% 
  summarise(mean = mean(pid7), 
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(group = c("Southern Baptist"))


umc <- cces16 %>% 
  filter(religpew_methodist ==1) %>% 
  filter(pid7 < 8) %>% 
  summarise(mean = mean(pid7), 
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(group = c("United Methodist"))

pid <- bind_rows(nd, sbc, umc)

pid %>% 
  ggplot(., aes(x = mean, y = group))  +
  geom_point(aes(colour = factor(group)), size =4, shape =1, stroke =2, show.legend = F) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(group)), height=0, size = 2, show.legend = FALSE) + 
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  labs(x = "Self Described Party Identification", y ="", title = "Average Party Identification", caption = "Data: CCES 2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(2,5.5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not Strong Republican", "Strong Republican"))  +
  mean_rb()
  
ggsave(file="D://cces/ndsbc/pid_mean.png", type = "cairo-png", width = 21, height = 7)


cces16$abort1 <- Recode(cces16$CC16_332a, "2=1; else=0") ## Always Allow
cces16$abort2 <- Recode(cces16$CC16_332b, "1=1; else=0") ## Only Rape, Incest, Life of Mother
cces16$abort3 <- Recode(cces16$CC16_332c, "1=1; else=0") ## Ban after 20 weeks
cces16$abort4 <- Recode(cces16$CC16_332d, "1=1; else=0") ## Employers decline abortion coverage
cces16$abort5 <- Recode(cces16$CC16_332e, "1=1; else=0") ## Prohibit federal funds for abortion
cces16$abort6 <- Recode(cces16$CC16_332f, "1=1; else=0") ## Make abortion illegal in all circumstances

cces16 <- cces16 %>% 
  mutate(abort = abort1 + abort2 + abort3 + abort4 + abort5 + abort6) 

nd <- cces16 %>% 
  filter(religpew_protestant ==3) %>% 
  select(abort1, abort2, abort3, abort4, abort5, abort6) %>% 
  summarise_all(funs(mean)) %>% 
  mutate(group = c("Nondenominational")) %>% 
  mutate(ab = (abort1 + abort2 + abort3 + abort4 + abort5 + abort6)/6) 

sbc <- cces16 %>% 
  filter(religpew_baptist ==1) %>% 
  select(abort1, abort2, abort3, abort4, abort5, abort6) %>% 
  summarise_all(funs(mean)) %>% 
  mutate(group = c("Southern Baptist")) %>% 
  mutate(ab = (abort1 + abort2 + abort3 + abort4 + abort5 + abort6)/6)
  

umc <- cces16 %>% 
  filter(religpew_methodist ==1) %>% 
  select(abort1, abort2, abort3, abort4, abort5, abort6) %>% 
  summarise_all(funs(mean)) %>% 
  mutate(group = c("United Methodist")) %>% 
  mutate(ab = (abort1 + abort2 + abort3 + abort4 + abort5 + abort6)/6)


abort <- bind_rows(nd, sbc, umc) %>% mutate(abort1 = round(abort1, 3))



a1 <- abort %>% 
  ggplot(., aes(x=group, y = abort1, fill = group)) + geom_col(color = "black") + bar_rb() +
  theme(axis.text.x = element_text(family = "Product Sans", size =24)) +
  scale_y_continuous(labels = scales::percent) +  
  geom_text(aes(y = abort1 + .025, label = paste0(abort1*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "", y = "Percent Who Are Opposed", title = "Abortion", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(size=32)) +
  scale_fill_brewer(palette = "Dark2") + theme(legend.position="none")

ggsave(file="D://cces/ndsbc/abortion.png", type = "cairo-png", width = 21, height = 15)




## Gay Marriage

cces16$gay <- Recode(cces16$CC16_335, "1=1; else=0")

nd <- cces16 %>% 
  filter(religpew_protestant ==3) %>% 
  filter(pid7 < 8) %>% 
  summarise(mean = mean(gay), 
            sd = sd(gay), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(group = c("Nondenominational"))

sbc <- cces16 %>% 
  filter(religpew_baptist ==1) %>% 
  filter(pid7 < 8) %>% 
  summarise(mean = mean(gay), 
            sd = sd(gay), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(group = c("Southern Baptist"))


umc <- cces16 %>% 
  filter(religpew_methodist ==1) %>% 
  filter(pid7 < 8) %>% 
  summarise(mean = mean(gay), 
            sd = sd(gay), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(group = c("United Methodist"))


gay <- bind_rows(nd, sbc, umc) 
  
gay <- gay %>%   
  mutate(mean = round(mean, 3))

gay <- gay %>% mutate(mean2 = 1-mean)



a1 <- abort %>% 
  ggplot(., aes(x=group, y = abort1, fill = group)) + geom_col(color = "black") + bar_rb() +
  theme(axis.text.x = element_text(family = "Product Sans", size =24)) +
  scale_y_continuous(labels = scales::percent) +  
  geom_text(aes(y = abort1 + .025, label = paste0(abort1*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "", y = "Percent Who Are Opposed", title = "Abortion as a Matter of Choice") +
  theme(plot.title = element_text(size=42)) +
  scale_fill_brewer(palette = "Dark2") + theme(legend.position="none")


a2 <- gay %>% 
  ggplot(., aes(x=group, y = mean2, fill = group)) + geom_col(color = "black") + bar_rb() +
  theme(axis.text.x = element_text(family = "Product Sans", size =24)) +
  scale_y_continuous(labels = scales::percent) +  
  geom_text(aes(y = mean2 + .025, label = paste0(mean2*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "", y = "", title = "Marriage for Gays and Lesbians", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(size=42)) +
  scale_fill_brewer(palette = "Dark2") + theme(legend.position="none")

# ggsave(file="D://cces/ndsbc/gay.png", type = "cairo-png", width = 21, height = 15)


a <- a1 + a2 

ggsave(file="D://cces/ndsbc/patchwork_gay_abort1.png", type = "cairo-png", width = 21, height = 15, a)




