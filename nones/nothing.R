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
library(janitor)


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


## This is just making the overall demos chart. 

rel <- cces %>% select(V101, commonweight_post, evangelical, mainline, bprot, catholic, mormon, jewish, muslim, buddhist, hindu, atheist, agnostic, nothing)
reltrad <- rel %>% gather(reltrad, x1, evangelical:nothing) %>% filter(x1==1) %>% select(V101,reltrad, commonweight_post)

p1 <- reltrad %>%  count(reltrad, wt = commonweight_post) %>% mutate(weight = prop.table(n)) 

p1$reltrad <- factor(p1$reltrad , levels=unique(p1$reltrad))


ggplot(p1, aes(1, y=weight, fill = reltrad)) + geom_col(colour = "black") + 
  theme(axis.title.y = element_blank()) +  
  geom_text(aes(label=reltrad), size =3, position = position_stack(vjust =.5)) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + theme(legend.position="none") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("#000000","#FFFF00","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6", "pink"))


## This is just making the big demo chart

t1 <- tibble("group" =c("No Religion", "Protestants", "Catholics", "Everyone Else"), count = c(16162, 24835 , 11433, 2912 ))
t1$group <- as_factor(t1$group)

t1 <- t1 %>% mutate(pct = count/sum(count)) 

t1$group <- fct_relevel(t1$group, "Protestants" , "No Religion" , "Catholics", "Everyone Else")

t1$label <- c("Distribution")

ggplot(t1, aes(x=label, y=pct*100, fill = forcats::fct_rev(group))) + geom_col(color = "black") + coord_flip() +
  theme(legend.position = "bottom") + guides(fill = guide_legend(reverse=TRUE)) +
  scale_x_discrete(labels = c("")) + xlab("") + 
  ylab("Percent of Sample") + theme(legend.title=element_blank())  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Major Religious Traditions in the United States") +
  labs(caption = "Data from CCES 2016")

ggsave(file="none_dist.png", type = "cairo-png", width = 10, height = 2)
  
## This is just making the big demo chart

t2 <- tibble("group" =c("Nothing in Particular", "Atheist", "Agnostic"), count = c(10162, 3009 , 2991))
t2$group <- as_factor(t2$group)

t2 <- t2 %>% mutate(pct = count/sum(count)) 

t2$group <- fct_relevel(t2$group, "Protestants" , "No Religion" , "Catholics", "Everyone Else")

t2$label <- c("Distribution")

ggplot(t2, aes(x=label, y=pct*100, fill = group)) + geom_col(color = "black") + coord_flip() +
  theme(legend.position = "bottom") + guides(fill = guide_legend(reverse=TRUE)) +
  scale_x_discrete(labels = c("")) + xlab("") + 
  ylab("Percent of Sample") + theme(legend.title=element_blank())  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Who are the 'No Religion'?") +
  labs(caption = "Data from CCES 2016")

ggsave(file="none_dist_2.png", type = "cairo-png", width = 10, height = 2)


## Gay Marriage

cces$gay <- Recode(cces$CC16_335, "1=1; else=0")

gayplot <- cces %>% right_join(reltrad) %>% 
  group_by(reltrad) %>% 
  summarise(mean = mean(gay)) %>%  
  filter(reltrad == "evangelical" | reltrad == "atheist" | reltrad == "agnostic" | reltrad == "nothing" | reltrad == "catholic")

## Church Attendance

attplot <- cces %>% right_join(reltrad) %>% 
  group_by(reltrad) %>% 
  summarise(mean = mean(pew_churatd)) %>%  
  filter(reltrad == "evangelical" | reltrad == "atheist" | reltrad == "agnostic" | reltrad == "nothing" | reltrad == "catholic")


cces %>% right_join(reltrad) %>% 
  filter(reltrad == "atheist") %>% tabyl(pew_churatd)
##88.5% Never Attend
## SD is .576
## Mean is 5.83
## CI is .018

cces %>% right_join(reltrad) %>% 
  filter(reltrad == "agnostic") %>% tabyl(pew_churatd)
## 69.7% Never Attend
## SD is .749
## Mean is 5.59
## CI is .023

cces %>% right_join(reltrad) %>% 
  filter(reltrad == "nothing") %>% tabyl(pew_churatd)
## 48.9% Never Attend
## SD is 1.22
## Mean is 5.15
## CI is .022






