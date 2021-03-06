

library(dplyr)
library(car)
library(ggplot2)
library(extrafont)
library(weights)
library(forcats)
library(haven)
library(grid)
library(tidyr)
library(reshape2)



cces <- read_dta("D:/cces/data/cces.dta")

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
cces$bprot <- cces$black + cces$protestant
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
cces$nothing <- Recode(cces$religpew, "11=1; else=0")



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


pid <- data.frame("class" =c("Evangelical", "Mainline", "Black Protestant", "Catholic", "Atheist", "Agnostic"), 
                       pid7 = c(4.81,3.87,1.88, 3.64, 2.56, 2.88))
pid$variable <- c("Party Identification")


#pid <- arrange(pid, desc(pid7))

pid <- mutate(pid, class=factor(class, levels=rev(class)))


pidplot <- ggplot(pid, aes(x = pid7, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(class))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("Generally speaking, do you think of yourself as a ...?") + ylab("") + xlim(1,7.5)  +
  scale_x_continuous(limits = c(1,7), breaks = c(1,2,3,4,5, 6, 7), labels = c("Strong Dem.", "Not Strong Dem.", "Lean Dem.", "Neither", "Lean Rep.", "Not Strong Rep.", "Strong Rep.")) + 
  theme(text=element_text(size=24, family="KerkisSans")) +  
  scale_fill_manual(values = c("#000000","#FFFF00","#FF34FF" ,  "#1CE6FF", "#008941", "#FF4A46")) +
  guides(fill=FALSE) + 
  geom_text(aes(label=class),hjust=.5, vjust=-1)


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

ideo <- data.frame("class" =c("Evangelical", "Mainline", "Black Protestant", "Catholic", "Atheist", "Agnostic"), 
                  ideo5 = c(3.6,3.14,2.82, 3.1, 2.11, 2.42))
ideo$variable <- c("Political Ideology ")


#ideo <- arrange(ideo, desc(ideo5))

ideo <- mutate(ideo, class=factor(class, levels=rev(class)))



idplot <- ggplot(ideo, aes(x = ideo5, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(class))) +  theme(legend.title=element_blank()) + theme(legend.position="none") +
  xlab("Generally speaking, do you think of yourself as a ...?") + ylab("")  +
  scale_x_continuous(limits =c(1.5,4.5), breaks = c(1,2,3,4,5), labels = c("Very liberal", "Liberal", "Moderate", "Conservative", "Very Conservative")) + 
  theme(text=element_text(size=24, family="KerkisSans")) +
  scale_fill_manual(values = c("#000000","#FFFF00","#FF34FF" ,  "#1CE6FF", "#008941", "#FF4A46")) + 
  geom_text(aes(label=class),hjust=.5, vjust=-1)


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


rel <- cces %>% select(V101, evangelical, mainline, bprot, catholic, mormon, jewish, muslim, buddhist, hindu, atheist, agnostic, nothing)
reltrad <- rel %>% gather(reltrad, x1, evangelical:nothing) %>% filter(x1==1) %>% select(V101,reltrad)
abort <- cces %>% select(V101, abort1:abort6)

aplot <- abort %>% right_join(reltrad) %>% 
  select(-V101) %>% group_by(reltrad) %>% 
  summarise(a1 = mean(abort1), a2 = mean(abort2), a3 = mean(abort3), a4 = mean(abort4), a5 = mean(abort5), a6 = mean(abort6)) %>% 
  melt(id =c("reltrad"))


aplot <- aplot %>% filter(reltrad == "evangelical" | reltrad == "mainline" | reltrad == "catholic" | reltrad == "bprot" | reltrad == "atheist" | reltrad ==  "agnostic")

##levels(aplot$variable) <- c("Oppose as Matter of Choice", "Only for Rape, Incest", "Prohibit Late Term", "Decline Abortion Insurance", "Prohibit Federal Funds", "Completely Illegal")

a1 <- aplot %>% filter(variable == "a1") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.position="none") +
  xlab("% That Oppose Abortion as a Matter of Choice") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

a2 <- aplot %>% filter(variable == "a2") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +    theme(legend.position="none") +
  xlab("% That Support Abortion Only for Rape, Incest, Life of Mother") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

a3 <- aplot %>% filter(variable == "a3") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +    theme(legend.position="none") +
  xlab("% That Support Prohibiting Abortion After 20 Weeks") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

a4 <- aplot %>% filter(variable == "a4") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.position="none") +
  xlab("% That Support Employers Declining Abortion Coverage in Insurance Plans") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

a5 <- aplot %>% filter(variable == "a5") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.position="none") +
  xlab("% That Support Prohibiting Federal Funds for Abortion") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))

a6 <- aplot %>% filter(variable == "a6") %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("% That Support Making Abortion Completely Illegal") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c(""))


grid.newpage()


grid.draw(rbind(ggplotGrob(a1), ggplotGrob(a2), ggplotGrob(a3), ggplotGrob(a4), ggplotGrob(a5), ggplotGrob(a6),  size = "last"))


aplot %>% ggplot(., aes(x = value*100, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("% That Support Making Abortion Completely Illegal") + ylab("")  +
  theme(text=element_text(size=18, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,80), breaks = c(10,20,30,40,50,60,70,80,90,100)) +scale_y_discrete(labels = c("")) + facet_grid(variable ~ .)


tplot <- aplot %>% group_by(reltrad) %>% 
  summarise(mean = mean(value)) %>%
  mutate(label = c("Overall"))

tplot$reltrad <- factor(tplot$reltrad, labels = c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline")) 


tplot %>% ggplot(., aes(x = mean , y = label))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) + theme(legend.position="none") +xlab("Abortion Opposition Scale") + ylab("")  +
  theme(text=element_text(size=24, family="KerkisSans")) +
  scale_fill_manual(labels =c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"), values = c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6")) +
  scale_x_continuous(limits = c(0,.6), breaks = c(0,.1,.2,.3,.40,.50,.60)) +scale_y_discrete(labels = c("")) +
  ggtitle("Overall Opposition to Abortion") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=reltrad),hjust=.5, vjust=-1)

ggsave(file="abortion_atheist_agnostic.png", type = "cairo-png", width = 15, height = 5)

## Gay Marriage

cces$gay <- Recode(cces$CC16_335, "1=1; else=0")

gayplot <- cces %>% right_join(reltrad) %>% 
  group_by(reltrad) %>% 
  summarise(mean = mean(gay)) %>%  
  filter(reltrad == "evangelical" | reltrad == "mainline" | reltrad == "catholic" | reltrad == "bprot" | reltrad == "atheist" | reltrad ==  "agnostic")

colors <- c("#000000","#FFFF00","#FF34FF", "#008941",  "#1CE6FF", "#FF4A46" )
gayplot$reltrad <- as_factor(gayplot$reltrad)

gayplot$reltrad <- factor(gayplot$reltrad, labels = c("Agnostic", "Atheist", "Black Protestant", "Catholic", "Evangelical", "Mainline"))

gayplot %>% ggplot(., aes(x=reorder(reltrad,-mean), y=mean*100)) + geom_col(fill = colors, color = "black") +
  theme(text=element_text(size=24, family="KerkisSans")) + xlab("Religious Tradition") + ylab("Percent in Favor") +
  ggtitle("Do you favor or oppose allowing gays and lesbians to marry legally?") +labs(caption = "Data from CCES 2016") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file="gaymarriage_atheist_agnostic.png", type = "cairo-png", width = 15, height = 10)


## VOTE 16

cces$vote16 <- cces$CC16_410a
cces$vote16 <- as.numeric(cces$vote16)

cces$vote16 <- Recode(cces$vote16, "1='Donald Trump';
2='Hillary Clinton';
3='Gary Johnson';
4='Jill Stein';
else = NA")

voteplot <- cces %>% right_join(reltrad) %>% 
  group_by(reltrad) %>% 
  filter(complete.cases(vote16)) %>% 
  filter(reltrad == "atheist" | reltrad == "agnostic") %>% 
  count(vote16) %>% mutate(weight = prop.table(n))

voteplot$vote16 <- word(voteplot$vote16, -1)
voteplot$reltrad <- factor(voteplot$reltrad, labels = c("Agnostic", "Atheist"))

colors <- c("dodgerblue3", "firebrick1" , "gold1", "chartreuse1", "dodgerblue3", "firebrick1" , "gold1", "chartreuse1")


ggplot(voteplot, aes(x=reorder(vote16, -weight), y=weight*100)) + geom_col(fill = colors, color = "black", position= "dodge")  + 
  ggtitle("Agnostics and Atheists at the Ballot Box") +
  scale_fill_manual(values = c("chartreuse1", "dodgerblue3","firebrick1","gold1")) +
  xlab("Candidate") + ylab("Percentage of the Vote") + 
  theme(legend.title=element_blank()) + 
  theme(text=element_text(size=24, family="KerkisSans"))  +
  theme(legend.position = "bottom")  +
  theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(~reltrad)

a <- cces %>% select(V101, age) %>% left_join(reltrad) %>% na.omit()
a %>% group_by(reltrad) %>% summarise(age = mean(age))
plotage <- a %>% group_by(reltrad) %>% summarise(age = mean(age)) 



plotage$reltrad <- factor(plotage$reltrad, labels = c("Agnostic", "Atheist", "Black Protestant", "Buddhist", "Catholic", "Evangelical", "Hindu", "Jewish", "Mainline", "Mormon", "Muslim", "Nothing in Particular" ))


colors <- c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6", "gray")
colors <- c("gray", "gray", "red", "gray", "red", "red", "gray", "gray", "gray", "gray", "gray", "gray")


ggplot(plotage, aes(x=reorder(reltrad, age), y=age)) + geom_col(fill = colors, color = "black") + 
  coord_flip()  + 
  theme(text=element_text(size=24, family="KerkisSans"))  +
  theme(legend.position = "bottom")  +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("") + ylab("Average Age") +labs(caption = "Data from CCES 2016") + 
  ggtitle("The Oldest Religious Groups") + scale_y_continuous(breaks=c(20,30,40,50,60,70), limits = c(0,60))

ggsave(file="age_reltrad.png", type = "cairo-png", width = 10, height = 10)