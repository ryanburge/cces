library(ggplot2)
library(dplyr)
library(weights)
library(extrafont)
library(car)
library(gridExtra)
library(haven)

cces16 <- read_dta("C:/Users/Ryan Burge/Desktop/cces.dta")

cces16$white <- Recode(cces16$race, "1=1; else=0")
cces16$black <- Recode(cces16$race, "2=1; else=0")

cces16$age <- 2017 - cces16$birthyr

## Vote 2016 

cces16$vote16 <- cces16$CC16_410a

cces16$vote16 <- as.numeric(cces16$vote16)

cces16$hiattend <- Recode(cces16$pew_churatd, "1:3=1; else=0")

cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=0")
bagain <- filter(cces16, bagain ==1)

bagain16 <- filter(cces16, bagain ==1)
baprot <- filter(cces16, bagain ==1 & religpew == 1)


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

evan16 <- filter(cces16, evanbaptist == 1 | evanmeth == 1 | evannd == 1 | evanluth == 1 | evanpres == 1 | pente == 1 | evanchrist == 1 | evancong == 1 | evanholy == 1 | evanadvent ==1)
evan16 <- filter(evan16, white ==1)
#evan16 <- filter(evan16, black !=1)
#evan16 <- filter(evan16, white ==1 & hiattend ==1)


measure  <- data.frame("class" =c("Monthly Attend Evangelical Church", "White Evangelical", "Evangelical Not AA", "Born Again Protestant","Affiliate w/Evangelical Church", "Born Again"), pct = c(80.4,75.9,73.9,65.5,61.4,61.2))


ggplot(measure, aes(reorder(x=class, -pct), y = pct, fill = class)) + geom_col()  +   
  scale_fill_manual(values=c("#ef6548", "#d7301f", "#b30000", "#7f0000")) + theme(axis.text.x = element_text(angle = 90)) + 
  theme(legend.position="none") + ylim(0,85)

ggplot(measure, aes(x=reorder(class, pct), y=pct)) + geom_point(size =4, color = "firebrick1") + 
  geom_segment(aes(x=class, xend=class, y=0, yend=pct), color = "black",lwd = 1.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()  +
  theme(plot.title = element_text(hjust = 0.5)) + theme_minimal(base_family="Arial Narrow") + labs(x="Potential Measurement", y="Percent Voting for Donald Trump", 
                                                                                                   title="What is an Evangelical?",
                                                                                                   subtitle="Different Approaches to the Same Concept",
                                                                                                   caption="Data from CCES 2016")
