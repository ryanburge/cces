

library(dplyr)
library(car)
library(ggplot2)
library(extrafont)
library(weights)
library(forcats)
library(haven)


cces <- cces16 

cces <- read_dta("C:/Users/Ryan Burge/Desktop/cces.dta")

cces$white <- Recode(cces$race, "1=1; else=0")


cces$black <- Recode(cces$race, "2=1; else=0")

## Vote 2016 

cces$vote16 <- cces$CC16_410a

#cces$vote16 <- as.numeric(cces$vote16)

cces$hiattend <- Recode(cces$pew_churatd, "1:3=1; else=0")

cces$repubid <-Recode(cces$pid3, "2=1; else=0")

cces$repubid7 <-Recode(cces$pid7, "5:7=1; else=0")
cces$bagain <- Recode(cces$pew_bornagain, "1=1; else=0")
cces$newpid <- Recode(cces$pid7, "8=4")
cces$newideo <- Recode(cces$ideo5, "6=3")

#cces <- filter(cces, vote16 <=4)

#cces$vote16<-Recode(cces$vote16,"1='Donald Trump';
# 2='Hillary Clinton';
#   3='Gary Johnson';
#  4='Jill Stein'")

## Evangelical

cces$evanbaptist <- Recode(cces$religpew_baptist, "1=1; 5:90=1; else=0")
cces$evanmeth <- Recode(cces$religpew_methodist, "2=1; else=0")
cces$evannd <- Recode(cces$religpew_nondenom, "1:90=1; else=0")
cces$evanluth <- Recode(cces$religpew_lutheran, "2:3=1; else=0")
cces$evanpres <- Recode(cces$religpew_presby, "6=1; else=0")
cces$pente <- Recode(cces$religpew_pentecost, "1:90=1; else=0")
cces$evanchrist <- Recode(cces$religpew_christian, "1=1; 3:4=1; else=0")
cces$evancong <- Recode(cces$religpew_congreg, "2=1; else=0")
cces$evanholy <- Recode(cces$religpew_holiness, "1:90=1; else=0")
cces$evanadvent <- Recode(cces$religpew_advent, "1:90=1; else=0")

evangelical <- filter(cces, evanbaptist == 1 | evanmeth == 1 | evannd == 1 | evanluth == 1 | evanpres == 1 | pente == 1 | evanchrist == 1 | evancong == 1 | evanholy == 1 | evanadvent ==1)
evangelical <- filter(evangelical, white ==1)

attendevan <- filter(evangelical, hiattend ==1)

cces$evangelical <- cces$evanbaptist + cces$evanmeth + cces$evannd + cces$evanluth + cces$evanpres + cces$pente + cces$evanchrist + cces$evancong + cces$evanholy + cces$evanadvent
cces$evangelical <- Recode(cces$evangelical, "1:4=1; else=0")

## Mainline

cces$mlbaptist <- Recode(cces$religpew_baptist, "2=1; 4=1; else=0")
cces$mlmeth <- Recode(cces$religpew_methodist, "1=1; 90=1; else=0")
cces$mlluth <- Recode(cces$religpew_lutheran, "1=1; 4=1; else=0")
cces$mlpres <- Recode(cces$religpew_presby, "1:5=1; 90=1; else=0")
cces$mlchrist <- Recode(cces$religpew_christian, "2=1; else=0")
cces$mlcong <- Recode(cces$religpew_congreg, "1=1; 3=1; else=0")
cces$mlreform <- Recode(cces$religpew_reformed, "1:90=1; else=0")
cces$episp <- Recode(cces$religpew_episcop, "1:90=1; else=0")

cces$mainline <- cces$mlbaptist + cces$mlmeth + cces$mlluth + cces$mlpres + cces$mlchrist + cces$mlcong + cces$mlreform + cces$episp
cces$mainline <- Recode(cces$mainline, "1:4=1; else=0")
#cces$mainline <- Recode(cces$mainline, "1:4=2; else=0")


## Black Protestant

bprot <- filter(cces, black ==1 & religpew ==1)
cces$protestant <- Recode(cces$religpew, "1=1; else=0")
cces$bprot <- cces$black + cces$protesant
cces$bprot <- Recode(cces$bprot, "2=1; else=0")

## Catholic 
cces$catholic <- Recode(cces$religpew_catholic, "1:90=1; else=0")
##cces$catholic <- Recode(cces$catholic, "1=4; else=0")

## Mormon
cces$mormon <- Recode(cces$religpew_mormon, "1:90=1; else=0")
##cces$mormon <- Recode(cces$mormon, "1=5; else=0)

## Jewish
cces$jewish <- Recode(cces$religpew, "5=1; else=0")
##cces$jewish <- Recode(cces$jewish, "1=6; else=0")

## Muslim 
cces$muslim <- Recode(cces$religpew, "6=1; else=0")
##cces$muslim <- Recode(cces$muslim, "1=7; else=0")

## Buddhist
cces$buddhist <- Recode(cces$religpew, "7=1; else=0")
##cces$buddhist <- Recode(cces$buddhist, "1=8; else=0")

## Hindus
cces$hindu <- Recode(cces$religpew, "8=1; else=0")
##cces$hindu <- Recode(cces$hindu, "1=9; else=0")

## Atheist
cces$atheist <- Recode(cces$religpew, "9=1; else=0")
##cces$atheist <- Recode(cces$atheist, "1=10; else=0")
## Agnostic 
cces$agnostic <- Recode(cces$religpew, "10=1; else=0")
## cces$agnostic <- Recode(cces$agnostic, "1=11; else=0")

mainline <- filter(cces, mainline ==1)
bprot <- filter(cces, black ==1 & religpew ==1)
catholic <-filter(cces, catholic ==1)
mormon <- filter(cces, mormon ==1)
jewish <- filter(cces, jewish ==1)
muslim <- filter(cces, muslim ==1)
buddhist <- filter(cces, buddhist ==1)
hindu <- filter(cces, hindu ==1)
atheist <- filter(cces, atheist ==1)
agnostic <- filter(cces, agnostic ==1)

mean(evangelical$newpid, na.rm = TRUE)
##4.81
mean(mainline$newpid, na.rm = TRUE)
##3.87
mean(bprot$newpid, na.rm = TRUE)
## 1.88
mean(catholic$newpid, na.rm = TRUE)
## 3.64
mean(mormon$newpid, na.rm = TRUE)
##4.79
mean(jewish$newpid, na.rm = TRUE)
##2.91
mean(muslim$newpid, na.rm = TRUE)
##2.34
mean(buddhist$newpid, na.rm=TRUE)
##3.03
mean(hindu$newpid, na.rm=TRUE)
##2.78
mean(atheist$newpid, na.rm=TRUE)
##2.56
mean(agnostic$newpid, na.rm = TRUE)
##2.88


pid <- data.frame("class" =c("White Evangelical", "Mainline", "Black Protestant", "Catholic", "Mormon", "Jewish", "Muslim", "Hindu", "Buddhist", "Atheist", "Agnostic"), 
                       pid7 = c(4.81,3.87,1.88, 3.64,4.79,2.91,2.34,3.03,2.78, 2.56, 2.88))
pid$variable <- c("Party Identification")


#pid <- arrange(pid, desc(pid7))

pid <- mutate(pid, class=factor(class, levels=rev(class)))


pidplot <- ggplot(pid, aes(x = value, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(class))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("Generally speaking, do you think of yourself as a ...?") + ylab("") + xlim(1,7.5)  +
  scale_x_continuous(limits = c(1,7), breaks = c(1,2,3,4,5, 6, 7), labels = c("Strong Dem.", "Not Strong Dem.", "Lean Dem.", "Neither", "Lean Rep.", "Not Strong Rep.", "Strong Rep.")) + 
  theme(text=element_text(size=18, family="KerkisSans")) +  
  scale_fill_manual(values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  guides(fill=FALSE)


mean(evangelical$newideo, na.rm = TRUE)
##3.6
mean(mainline$newideo, na.rm = TRUE)
##3.14
mean(bprot$newideo, na.rm = TRUE)
## 2.82
mean(catholic$newideo, na.rm = TRUE)
## 3.1
mean(mormon$newideo, na.rm = TRUE)
##3.64
mean(jewish$newideo, na.rm = TRUE)
##2.70
mean(muslim$newideo, na.rm = TRUE)
##2.87
mean(buddhist$newideo, na.rm=TRUE)
##2.53
mean(hindu$newideo, na.rm=TRUE)
##2.54
mean(atheist$newideo, na.rm=TRUE)
##2.11
mean(agnostic$newideo, na.rm = TRUE)
##2.42

ideo <- data.frame("class" =c("White Evangelical", "Mainline", "Black Protestant", "Catholic", "Mormon", "Jewish", "Muslim", "Hindu", "Buddhist", "Atheist", "Agnostic"), 
                  ideo5 = c(3.6,3.14,2.82, 3.1,3.64,2.7,2.87,2.53,2.54, 2.11, 2.42))
ideo$variable <- c("Political Ideology ")


#ideo <- arrange(ideo, desc(ideo5))

ideo <- mutate(ideo, class=factor(class, levels=rev(class)))

idplot <- ggplot(ideo, aes(x = value, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(class))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("Generally speaking, do you think of yourself as a ...?") + ylab("")  +
  scale_x_continuous(limits =c(1.5,4.5), breaks = c(1,2,3,4,5), labels = c("Very liberal", "Liberal", "Moderate", "Conservative", "Very Conservative")) + 
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6"))


pid <- pid %>% rename(value = pid7)
ideo <- ideo %>% rename(value = ideo5)

ideo %>% bind_rows(pid)

plot <- bind_rows(ideo, pid)

ggplot(plot, aes(x = value, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(class))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("Generally speaking, do you think of yourself as a ...?") + ylab("")  +
  scale_x_continuous(limits =c(1.5,4.5), breaks = c(1,2,3,4,5), labels = c("Very liberal", "Liberal", "Moderate", "Conservative", "Very Conservative")) + 
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) + facet_grid(variable ~ .)

grid.newpage()
grid.draw(rbind(ggplotGrob(pidplot), ggplotGrob(idplot),  size = "last"))

## Abortion Public Opinion

cces$abort1 <- Recode(cces$CC16_332a, "2=1; else=0") ## Always Allow
cces$abort2 <- Recode(cces$CC16_332b, "1=1; else=0") ## Only Rape, Incest, Life of Mother
cces$abort3 <- Recode(cces$CC16_332c, "1=1; else=0") ## Ban after 20 weeks
cces$abort4 <- Recode(cces$CC16_332d, "1=1; else=0") ## Employers decline abortion coverage
cces$abort5 <- Recode(cces$CC16_332e, "1=1; else=0") ## Prohibit federal funds for abortion
cces$abort6 <- Recode(cces$CC16_332f, "1=1; else=0") ## Make abortion illegal in all circumstances


rel <- cces %>% select(V101, evangelical, mainline, bprot, catholic, mormon, jewish, muslim, buddhist, hindu, atheist, agnostic)
reltrad <- rel %>% gather(reltrad, x1, evangelical:agnostic) %>% filter(x1==1) %>% select(reltrad)
abort <- cces %>% select(V101, abort1:abort6)

aplot <- abort %>% right_join(reltrad) %>% 
  select(-V101) %>% group_by(reltrad) %>% 
  summarise(a1 = mean(abort1), a2 = mean(abort2), a3 = mean(abort3), a4 = mean(abort4), a5 = mean(abort5), a6 = mean(abort6)) %>% 
  melt(id =c("reltrad"))


aplot <- aplot %>% filter(reltrad == "evangelical" | reltrad == "mainline" | reltrad == "catholic" | reltrad == "bprot" | reltrad == "atheist" | reltrad ==  "agnostic")


aplot %>% filter(variable == "a1") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("% That Oppose Abortion as a Matter of Choice") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))


aplot %>% filter(variable == "a2") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("% That Support Abortion Only for Rape, Incest, Life of Mother") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

aplot %>% filter(variable == "a3") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("% That Support Prohibiting Abortion After 20 Weeks") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

aplot %>% filter(variable == "a4") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("% That Support Employers Declining Abortion Coverage in Insurance Plans") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

aplot %>% filter(variable == "a5") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("% That Support Prohibiting Federal Funds for Abortion") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

aplot %>% filter(variable == "a6") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("% That Support Making Abortion Completely Illegal") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))







