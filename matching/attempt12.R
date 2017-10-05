library(tidyverse)
library(car)
library(cem)
library(haven)

cces12 <- read_dta("D://cces/data/cces12.dta") 

#cces12 <- data.frame(na.omit(cces12))


cc <- cces12 %>% filter(religpew ==1 & pew_bornagain ==1)

cc <- cc %>%  mutate(treated = recode(race, "1=1; else=0"))

match <- cc %>% mutate(age = 2017 - birthyr, educ= educ, income= faminc, attend= pew_churatd, male = gender, imp = pew_religimp, pray = pew_prayer, pid = pid7, vote12 = CC410a) %>% 
  select(treated, age, educ, income, attend, male, imp, pray, pid, vote12)

match <- match %>% mutate(age = recode(age, "18:35 =.25; 36:44=.5; 45:64=.75; 65:100=1")) %>% 
  mutate(educ = recode(educ, "1:2=.33; 3:4=.66; 5:6=1; else=0")) %>% 
  mutate(income = recode(income, "1:5=.33; 6:10=.66; 11:32=1; else=0")) %>% 
  mutate(attend = recode(attend, "1:2=1; 3:4=.66; 5:7=.33; else=0")) %>% 
  mutate(male = recode(male, "1=1; else=0")) %>% 
  mutate(imp = recode(imp, "1:2=1; 3:4=.5; else=0")) %>% 
  mutate(pray = recode(pray, "1:2=.33; 3:4=.66; 5:7=1; else=0")) %>% 
  mutate(pid = recode(pid, "1:3=.33; 4=.66; 5:7=1; 8:99 =0")) %>% 
  mutate(romney = recode(vote12, "2=1; else=0")) %>% 
  select(-vote12)

match <- match %>% na.omit()

match <- as.data.frame(match)

mat <- cem(treatment = "treated", data = match, drop = "romney", keep.all = TRUE)

est <- att(mat, romney ~ treated + age + educ + income + attend + male + imp + pray + pid, data = match)

reg12 <- glm(romney ~ treated + age + educ + income + attend + male + imp + pray + pid, data = match)
reg12 <- tidy(reg12) %>% mutate(year = c(2012)) 

treg <- as.data.frame(t(est$att.model))
treg$term <- rownames(treg)
rownames(treg) <- NULL


# write.csv(treg, "D:/cces/matching/treg12.csv")


treg <- read_csv("D:/cces/matching/treg12.csv")

treg12 <- treg %>% mutate(model = c(2012)) %>% select(-X6)



treg <- treg %>% clean_names()
write.csv(treg, "treg12.csv")
treg <- read_csv("D://cces/matching/treg12.csv")


dwplot(treg, dodge_size = .05, show_intercept = FALSE) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold")) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=16, family="KerkisSans")) + labs(x="Coefficient Estimate", y="", 
                                                                title="Predicting Trump's Vote Share ",
                                                                caption="Data from CCES 2016") 



reg1 <- lm(romney ~ treated + age + educ + income + attend + male + imp + pray + pid, data = match)

reg1_df <- broom::tidy(reg1) %>% 
  relabel_predictors(c("(Intercept)" = "Intercept", treated = "White", age = "Age", educ = "Education", income ="Income", attend ="Church Attendance", male = "Male", imp = "Importance of Religion", pray ="Freq. of Prayer", pid = "Republican PID"))

reg1_df <- reg1_df[-1,]

dwplot(reg1_df, dodge_size = .05, show_intercept = FALSE) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold")) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=16, family="KerkisSans")) + labs(x="Coefficient Estimate", y="", 
                                                                title="Predicting Trump's Vote Share ",
                                                                caption="Data from CCES 2016") 
