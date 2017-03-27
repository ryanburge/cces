library(dplyr)
library(haven)
library(car)
library(dotwhisker)


cces16 <- read_dta("C:/Users/rpburge/Dropbox/data/cces.dta")
cces12 <- read_dta("C:/Users/rpburge/Dropbox/data/cces12.dta")
cces08 <- read_dta("C:/Users/rpburge/Dropbox/data/cces2008.dta")

gss10 <- read_dta("C:/Users/rpburge/Dropbox/data/gss10.dta")
gss12 <- read_dta("C:/Users/rpburge/Dropbox/data/gss12.dta")
gss14 <- read_dta("C:/Users/rpburge/Dropbox/data/gss14.dta")


##Getting an Evangelical Variable
cces16$evanbaptist <- Recode(cces16$religpew_baptist, "1=1; 5:90=1; else=0")
cces16$evanmeth <- Recode(cces16$religpew_methodist, "2=1; else=0")
cces16$evannd <- Recode(cces16$religpew_nondenom, "1:90=1; else=0")
cces16$evanluth <- Recode(cces16$religpew_lutheran, "2:3=1; else=0")
cces16$evanpres <- Recode(cces16$religpew_presby, "6=1; else=0")
cces16$pente <- Recode(cces16$religpew_pentecost, "1:90=1; else=0")
cces16$evanchrist <- Recode(cces16$religpew_christian, "1=1; 3:4=1; else=0")
cces16$evancong <- Recode(cces16$religpew_congreg, "2=1; else=0")
cces16$evanholy <- Recode(cces16$religpew_holiness, "1:90=1; else=0")
cces16$evanadvent <- Recode(cces16$religpew_advent, "1:90=1; else=0")

cces16$evangelical <- cces16$evanbaptist + cces16$evanmeth + cces16$evannd + cces16$evanluth + cces16$evanpres + cces16$pente + cces16$evanchrist + cces16$evancong + cces16$evanholy + cces16$evanadvent
cces16$evangelical <- Recode(cces16$evangelical, "1:4=1; else=0")

cces12$evanbaptist <- Recode(cces12$religpew_baptist, "1=1; 5:90=1; else=0")
cces12$evanmeth <- Recode(cces12$religpew_methodist, "2=1; else=0")
cces12$evannd <- Recode(cces12$religpew_nondenom, "1:90=1; else=0")
cces12$evanluth <- Recode(cces12$religpew_lutheran, "2:3=1; else=0")
cces12$evanpres <- Recode(cces12$religpew_presby, "6=1; else=0")
cces12$pente <- Recode(cces12$religpew_pentecost, "1:90=1; else=0")
cces12$evanchrist <- Recode(cces12$religpew_christian, "1=1; 3:4=1; else=0")
cces12$evancong <- Recode(cces12$religpew_congreg, "2=1; else=0")
cces12$evanholy <- Recode(cces12$religpew_holiness, "1:90=1; else=0")
cces12$evanadvent <- Recode(cces12$religpew_advent, "1:90=1; else=0")


cces12$evangelical <- cces12$evanbaptist + cces12$evanmeth + cces12$evannd + cces12$evanluth + cces12$evanpres + cces12$pente + cces12$evanchrist + cces12$evancong + cces12$evanholy + cces12$evanadvent
cces12$evangelical <- Recode(cces12$evangelical, "1:4=1; else=0")

cces08$evanbaptist <- Recode(cces08$V222, "1=1; 5:90=1; else=0")
cces08$evanmeth <- Recode(cces08$V223, "2=1; else=0")
cces08$evannd <- Recode(cces08$V224, "1:90=1; else=0")
cces08$evanluth <- Recode(cces08$V225, "2:3=1; else=0")
cces08$evanpres <- Recode(cces08$V226, "6=1; else=0")
cces08$pente <- Recode(cces08$V227, "1:90=1; else=0")
cces08$evanchrist <- Recode(cces08$V229, "1=1; 3:4=1; else=0")
cces08$evancong <- Recode(cces08$V230, "2=1; else=0")
cces08$evanholy <- Recode(cces08$V231, "1:90=1; else=0")
cces08$evanreform <- Recode(cces08$V232, "2=1; else=0")
cces08$evanadvent <- Recode(cces08$V233, "1:90=1; else=0")

cces08$evangelical <- cces08$evanbaptist + cces08$evanmeth + cces08$evannd + cces08$evanluth + cces08$evanpres + cces08$pente + cces08$evanchrist + cces08$evancong + cces08$evanholy + cces08$evanadvent + cces08$evanreform
cces08$evangelical <- Recode(cces08$evangelical, "1:4=1; else=0")

## White Variable

cces08$white <- Recode(cces08$V211, "1=1; else=0")
cces12$white <- Recode(cces12$race, "1=1; else=0")
cces16$white <- Recode(cces16$race, "1=1; else=0")

## Born Again

cces08$bagain <- Recode(cces08$V215, "1=1; else=0")
cces12$bagain <- Recode(cces12$pew_bornagain, "1=1; else=0")
cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=0")

## Protestant

cces08$protestant <- Recode(cces08$V219, "1=1; else=0")
cces12$protestant <- Recode(cces12$religpew, "1=1; else=0")
cces16$protestant <- Recode(cces16$religpew, "1=1; else=0")


## Abortion question coding

cces08$abort <- Recode(cces08$CC310, "4=1; else=0")
cces12$abort <- Recode(cces12$CC324, "4=1; else=0")
cces16$abort <- Recode(cces16$CC16_332a, "1=1; else=0")


## Cleaning up Controls

cces08$age <- 2008- cces08$V207
cces08$age <- cces08$age/100

cces12$age <- 2012- cces12$birthyr
cces12$age <- cces12$age/94

cces16$age <- 2016- cces16$birthyr
cces16$age <- cces16$age/99

cces08$educ <- cces08$V213/6
cces12$educ <- cces12$educ/6
cces16$educ <- cces16$educ/6

cces08$male <- Recode(cces08$V208, "1=1; else=0")
cces12$male <- Recode(cces12$gender, "1=1; else=0")
cces16$male <- Recode(cces16$gender, "1=1; else=0")

cces08$pid7 <- cces08$CC307a
cces08$pid7[cces08$pid7==8] <- NA
cces12$pid7[cces12$pid7==8] <- NA
cces16$pid7[cces16$pid7==8] <- NA

cces08$pid <- cces08$pid7/7
cces12$pid <- cces12$pid7/7
cces16$pid <- cces16$pid7/7


baprot16 <- filter(cces16, white == 1 & protestant ==1 & bagain ==1)
evan16 <- filter(cces16, white ==1 & evangelical ==1)

baprot12 <- filter(cces12, white == 1 & protestant ==1 & bagain ==1)
evan12 <- filter(cces12, white ==1 & evangelical ==1)

baprot08 <- filter(cces08, white == 1 & protestant ==1 & bagain ==1)
evan08 <- filter(cces08, white ==1 & evangelical ==1)


reg1 <- glm(abort ~ educ + male + age + pid, data = baprot16)
reg2 <- glm(abort ~ educ + male + age + pid, data = evan16)

reg1 <- tidy(reg1) %>% mutate(model = "BA + Prot")
reg2 <- tidy(reg2) %>% mutate(model = "Evangelical")


reg3 <- glm(abort ~ educ + male + age + pid, data = baprot12)
reg4 <- glm(abort ~ educ + male + age + pid, data = evan12)

reg3 <- tidy(reg3) %>% mutate(model = "BA + Prot")
reg4 <- tidy(reg4) %>% mutate(model = "Evangelical")

reg5 <- glm(abort ~ educ + male + age + pid, data = baprot08)
reg6 <- glm(abort ~ educ + male + age + pid, data = evan08)

reg5 <- tidy(reg5) %>% mutate(model = "BA + Prot")
reg6 <- tidy(reg6) %>% mutate(model = "Evangelical")

model16 <- rbind(reg1, reg2)
model12 <- rbind(reg3, reg4)
model08 <- rbind(reg5, reg6)

dwplot(model16, dodge_size = .05) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, -5.5), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  relabel_y_axis(c("Education", "Male", "Age", "Republican ID")) + labs(x="Coefficient Estimate", y="", 
                                                                        title="Predicting Support for Abortion ",
                                                                        caption="Data from CCES 2016") 


dwplot(model12, dodge_size = .05) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, -5.5), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  relabel_y_axis(c("Education", "Male", "Age", "Republican ID")) + labs(x="Coefficient Estimate", y="", 
                                                                        title="Predicting Support for Abortion ",
                                                                        caption="Data from CCES 2012")



dwplot(model08, dodge_size = .05) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, -5.5), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  relabel_y_axis(c("Education", "Male", "Age", "Republican ID")) + labs(x="Coefficient Estimate", y="", 
                                                                        title="Predicting Support for Abortion ",
                                                                        caption="Data from CCES 2008")






