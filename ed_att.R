library(tidyverse)
library(car)
library(forcats)
library(extrafont)

cces16 <- read_dta("D:/cces/data/cces.dta")
cces12 <- read_dta("D:/cces/data/cces12.dta")

cces16 %>% 
  mutate(attend = 7 - pew_churatd) %>% 
  group_by(educ) %>% 
  count(attend) %>% 
  na.omit() %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(attend >0) %>% 
  mutate(ed = factor(educ), attend = factor(attend)) %>% 
  mutate(attend = recode(attend, "0= 'Do not Know';
                                        1= 'Never';
                                        2= 'Seldom';
                                        3= 'Yearly';
                                        4= 'Monthly';
                                        5= 'Weekly';
                                        6= 'More than Weekly'")) %>% 
  mutate(ed = recode(ed, "1= 'Less than HS';
                              2='HS Grad'; 
                              3= 'Some College'; 
                              4= 'Associates'; 
                              5= 'Bachelors';
                              6= 'Graduate'")) %>% 
  ungroup(educ) %>% 
  mutate(attend = fct_inorder(attend), ed = fct_inorder(ed)) %>% 
ggplot(., aes(x=ed, y = weight, fill = attend)) + geom_col(position = "dodge") +  
  theme(axis.title.y = element_blank()) + 
  ylab("Percent of Votes Cast") + xlab("Level of Education") +
  theme(legend.position="bottom") +
  ggtitle("Educational Level and Church Attendance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))  + 
  scale_y_continuous(labels = scales::percent) + theme(legend.title=element_blank()) + labs(caption = "Data from CCES 2016")

ggsave(file="educ_attend_cces.png", type = "cairo-png", width = 20, height =12)

cces16$attend <- 7 - cces16$pew_churatd
cces16$age <- 2016- cces16$birthyr
cces16$age <- cces16$age/99
cces16$ed <- cces16$educ/6
cces16$att2 <- cces16$attend/6

reg1 <- lm(att2 ~ ed + age, data = cces16)
dwplot(reg1)  + geom_vline(xintercept = 0, colour = "grey50", linetype = 2)

cces12$attend <- 7 - cces12$pew_churatd
cces12$age <- 2016- cces12$birthyr
cces12$age <- cces12$age/99
cces12$ed <- cces12$educ/6
cces12$att2 <- cces12$attend/6

reg1 <- lm(att2 ~ ed + age, data = cces12)
dwplot(reg1)  + geom_vline(xintercept = 0, colour = "grey50", linetype = 2)

gss16 <- read_dta("D:/cces/data/gss16.dta")
gss14 <- read_dta("D:/cces/data/gss14.dta")
gss12 <- read_dta("D:/cces/data/gss12.dta")
gss10 <- read_dta("D:/cces/data/gss10.dta")

gss16 %>% mutate(educ2 = recode(educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")) %>% 
                   mutate(att = recode(attend, "8=6; 6:7=5; 4:5=4; 3=3; 2=2; 1=1; 0=0")) %>% 
  group_by(educ2) %>% 
  count(att) %>% 
  na.omit() %>% 
  mutate(weight = prop.table(n))  %>% 
  filter(att >0) %>% 
  filter(educ2 >0) %>% 
  mutate(ed = factor(educ2), attend = factor(att)) %>% 
  mutate(attend = recode(attend, "0= 'Do not Know';
                         1= 'Never';
                         2= 'Seldom';
                         3= 'Yearly';
                         4= 'Monthly';
                         5= 'Weekly';
                         6= 'More than Weekly'")) %>% 
  mutate(ed = recode(ed, "1= 'Less than HS';
                     2='HS Grad'; 
                     3= 'Some College'; 
                     4= 'Associates'; 
                     5= 'Bachelors';
                     6= 'Graduate'")) %>% 
  ungroup(educ2) %>% 
  mutate(attend = fct_inorder(attend), ed = fct_inorder(ed)) %>% 
  ggplot(., aes(x=ed, y = weight, fill = attend)) + geom_col(position = "dodge") +  
  theme(axis.title.y = element_blank()) + 
  ylab("Percent of Votes Cast") + xlab("Level of Education") +
  theme(legend.position="bottom") +
  ggtitle("Educational Level and Church Attendance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))  + 
  scale_y_continuous(labels = scales::percent) + theme(legend.title=element_blank()) + labs(caption = "Data from GSS 2016")

ggsave(file="educ_attend_gss.png", type = "cairo-png", width = 20, height =12)


reg16 <- gss16 %>% mutate(educ2 = recode(educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")) %>% 
  mutate(att = recode(attend, "8=6; 6:7=5; 4:5=4; 3=3; 2=2; 1=1; 0=0")) %>% 
  mutate(ed = educ2/6) %>% 
  mutate(att2 = att/6) %>% 
  mutate(age2 = age/89)

reg16 <- lm(att2 ~ ed + age2, data = reg16)
reg16 <- tidy(reg16) %>% mutate(model = c("GSS 2016"))

reg14 <- gss14 %>% mutate(educ2 = recode(educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")) %>% 
  mutate(att = recode(attend, "8=6; 6:7=5; 4:5=4; 3=3; 2=2; 1=1; 0=0")) %>% 
  mutate(ed = educ2/6) %>% 
  mutate(att2 = att/6) %>% 
  mutate(age2 = age/89)

reg14 <- lm(att2 ~ ed + age2, data = reg14)
reg14 <- tidy(reg14) %>% mutate(model = c("GSS 2014"))

reg12 <- gss12 %>% mutate(educ2 = recode(educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")) %>% 
  mutate(att = recode(attend, "8=6; 6:7=5; 4:5=4; 3=3; 2=2; 1=1; 0=0")) %>% 
  mutate(ed = educ2/6) %>% 
  mutate(att2 = att/6) %>% 
  mutate(age2 = age/89)

reg12 <- lm(att2 ~ ed + age2, data = reg12)
reg12 <- tidy(reg12) %>% mutate(model = c("GSS 2012"))

reg10 <- gss10 %>% mutate(educ2 = recode(educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")) %>% 
  mutate(att = recode(attend, "8=6; 6:7=5; 4:5=4; 3=3; 2=2; 1=1; 0=0")) %>% 
  mutate(ed = educ2/6) %>% 
  mutate(att2 = att/6) %>% 
  mutate(age2 = age/89)

reg10 <- lm(att2 ~ ed + age2, data = reg10)
reg10 <- tidy(reg10) %>% mutate(model = c("GSS 2010"))

gss_reg <- bind_rows(reg10, reg12, reg14, reg16)


dwplot(gss_reg)  + geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-7, -8.0), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
    theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=22, family="KerkisSans")) +
  relabel_y_axis(c("Education", "Age")) + labs(x="Coefficient Estimate", y="", 
                                                                        title="Predicting Religious Attendance ",
                                                                        caption="Data from the GSS")  
 

ggsave(file="gss_attnd_dwplot.png", type = "cairo-png", width = 12, height = 12)


cces16$attend <- 7 - cces16$pew_churatd
cces16$age <- 2016- cces16$birthyr
cces16$age <- cces16$age/99
cces16$ed <- cces16$educ/6
cces16$att2 <- cces16$attend/6

reg16 <- lm(att2 ~ ed + age, data = cces16)
reg16 <- tidy(reg16) %>% mutate(model = c("CCES 2016"))

cces12$attend <- 7 - cces12$pew_churatd
cces12$age <- 2016- cces12$birthyr
cces12$age <- cces12$age/99
cces12$ed <- cces12$educ/6
cces12$att2 <- cces12$attend/6

reg12 <- lm(att2 ~ ed + age, data = cces12)
reg12 <- tidy(reg12) %>% mutate(model = c("CCES 2012"))


cces08$age <- 2008- cces08$V207
cces08$age <- cces08$age/100

cces08$attend <- 7- cces08$V217
cces08$attend <- cces08$attend/6

cces08$educ <- cces08$V213
cces08$ed <- cces08$educ/6

reg08 <- lm(attend ~ ed + age, data = cces08)

reg08 <- tidy(reg08) %>% mutate(model = c("CCES 2008"))

cces_reg <- bind_rows(reg08, reg12, reg16)

dwplot(cces_reg)  + geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-6, -10.0), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=22, family="KerkisSans")) +
  relabel_y_axis(c("Education", "Age")) + labs(x="Coefficient Estimate", y="", 
                                               title="Predicting Religious Attendance ",
                                               caption="Data from the CCES") 

ggsave(file="cces_attnd_dwplot.png", type = "cairo-png", width = 12, height = 12)


