library(ggplot2)
library(dplyr)
library(weights)
library(extrafont)
library(car)
library(gridExtra)
library(haven)

## Loading Data
cces16 <- read_dta("C:/Users/Ryan Burge/Desktop/cces.dta")

## Race and Age Measure
cces16$white <- Recode(cces16$race, "1=1; else=0")
cces16$black <- Recode(cces16$race, "2=1; else=0")
cces16$age <- 2017 - cces16$birthyr

## Vote 2016 

cces16$vote16 <- cces16$CC16_410a

cces16$vote16 <- as.numeric(cces16$vote16)

## Monthly Attendance

cces16$hiattend <- Recode(cces16$pew_churatd, "1:3=1; else=0")

## Born Again

cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=0")
bagain <- filter(cces16, bagain ==1)

bagain16 <- filter(cces16, bagain ==1)
baprot <- filter(cces16, bagain ==1 & religpew == 1)

## PID Measures 

cces$repubid <-Recode(cces$pid3, "2=1; else=0")
cces16$repubid7 <-Recode(cces16$pid7, "5:7=1; else=0")

## Creating Reltrad Evangelical
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


## Creating Dummy Measures for Evangelical Denominations
cces16$sbc <- Recode(cces16$religpew_baptist, "1=1; else=0")
sbc <- filter(cces16, sbc ==1 & white ==1)
meth <- filter(cces16, evanmeth ==1 & white ==1)
nondenom <- filter(cces16, evannd ==1 & white ==1)
luth <- filter(cces16, evanluth ==1 & white ==1)
pres <- filter(cces16, evanpres ==1 & white ==1)
pente <- filter(cces16, pente ==1 & white ==1)
christ <- filter(cces16, evanchrist ==1 & white ==1)
cong  <- filter(cces16, evancong ==1 & white ==1)
holy <- filter(cces16, evanholy ==1 & white ==1)
advent <- filter(cces16, evanadvent ==1 & white ==1)

## Getting Pcts For Trump By Denoms
wpct(evan16$vote16, evan16$commonweight_post)
wpct(sbc$vote16, sbc$commonweight_post)
wpct(meth$vote16, meth$commonweight_post)
wpct(nondenom$vote16, nondenom$commonweight_post)
wpct(luth$vote16, luth$commonweight_post)
wpct(pres$vote16, pres$commonweight_post)
wpct(pente$vote16, pente$commonweight_post)
wpct(christ$vote16, christ$commonweight_post)
wpct(cong$vote16, cong$commonweight_post)
wpct(holy$vote16, holy$commonweight_post)
wpct(advent$vote16, advent$commonweight_post)



## Which Denoms Supported Trump the Most
trad <- c("Cons. Congregat.", "Pentecostal", "Southern Baptist","Holiness", "Church of Christ", "Evan. Presby.", "Non-Denom.", "Luth. - MO or WI Synod", "Free Methodist", "Adventist")
g1 <- cbind(trad)
g1 <- as.data.frame(g1)

g1$pct <- c(86.6, 85.5, 83.4, 81.1, 76.5, 74.4, 72.9, 66.8, 58.1, 49.5)
g1$trad <- factor(g1$trad, levels=unique(g1$trad))

#write.csv(g1, "denom_trump.csv")

ggplot(g1, aes(reorder(trad, pct), pct)) + geom_col() + theme(axis.text.x = element_text(angle = 90)) + coord_flip()

## Bar Chart Style
ggplot(g1, aes(reorder(trad, pct), pct, fill = trad)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + coord_flip() +  guides(fill=FALSE) +
  scale_fill_manual(values=rev(c("#fff7ec","#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#b30000", "#7f0000"))) + theme_minimal(base_family="Arial Narrow") + labs(x="Denomination", y="Percent Voting for Donald Trump",  
                                                                                                                                                                                                   title="What Denominations Were Strongest for Trump?",
                                                                                                                                                                                                   subtitle="Only White Evangelicals Reported Below",
                                                                                                                                                                                                   caption="Data from CCES 2016")


## Lollipop Chart Style 
ggplot(g1, aes(reorder(trad, pct), pct, fill = trad)) + geom_point(size =4, color = "firebrick1") + geom_segment(aes(x=trad, xend=trad, y=0, yend=pct), color = "black",lwd = 1.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()  +   guides(fill=FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) + theme_minimal(base_family="Arial Narrow") + labs(x="Denomination", y="Percent Voting for Donald Trump", 
                                                                                                   title="What Denominations Were Strongest for Trump?",
                                                                                                   subtitle="Only White Evangelicals Reported Below",
                                                                                                   caption="Data from CCES 2016")
   
  
  





                                                                                                                                                                                                   



## Creating Evangeical Subset

evan16 <- filter(cces16, evanbaptist == 1 | evanmeth == 1 | evannd == 1 | evanluth == 1 | evanpres == 1 | pente == 1 | evanchrist == 1 | evancong == 1 | evanholy == 1 | evanadvent ==1)
evan16 <- filter(evan16, white ==1)
#evan16 <- filter(evan16, black !=1)
#evan16 <- filter(evan16, white ==1 & hiattend ==1)

## Trump Voting Evangelicals and Non Trump Evangelicals
trump <- filter(evan16, vote16 == 1)
ntrump <- filter(evan16, vote16 != 1)

## Comparing by Gender

wpct(trump$gender, trump$commonweight_post)
## 49.3 Male 50.6 Female
wpct(ntrump$gender, ntrump$commonweight_post)
## 43.7% Male 56.2% Female

gtrump <- data.frame("class" = c("Male", "Female"), pct =c(49.3, 50.6))
gntrump <- data.frame("class" = c("Male", "Female"), pct =c(43.7, 56.2))

gtrump$label <- c("Trump")
gntrump$label <- c("Non-Trump")

gender <- rbind(gtrump, gntrump)

Palette <- c("gray61", "red3")


ggplot(gender, aes(x=class, y=pct)) + geom_col(aes(fill=label), position = "dodge") + scale_fill_manual(values = Palette) +  theme_minimal(base_family="Arial Narrow") + 
  labs(x="Gender", y="Percentage of Respondents", title="Evangelical Voters", subtitle="", caption="Data from CCES 2016") + 
  theme(legend.position="bottom") + labs(fill="")

## Comparing by Religious Attendance

wpct(trump$pew_churatd, trump$commonweight_post)
## 19.6% Week+  28.6% Weekly 9.2% Monthly 13.3% Yearly 21.2% Seldom 7% Never  
wpct(ntrump$pew_churatd, ntrump$commonweight_post)
## 11.4% Week+  22.4% Weekly 9.8% Monthly 15.5% Yearly 26.7% Seldom 13.2% Never

tattend  <- data.frame("class" =c("Weekly+", "Weekly", "Monthly", "Yearly","Seldom", "Never"), pct = c(19.6,28.6,9.2,13.3,21.2,7))
ntattend <- data.frame("class" =c("Weekly+", "Weekly", "Monthly", "Yearly","Seldom", "Never"), pct = c(11.4,22.4,9.8,15.5,26.7,13.2))
tattend$class <- factor(tattend$class, levels=unique(tattend$class))
ntattend$class <- factor(ntattend$class, levels=unique(ntattend$class))

tattend$label <- c("Trump")
ntattend$label <- c("Non-Trump")

attend <- rbind(tattend, ntattend)



ggplot(attend, aes(x=class, y=pct)) + geom_col(aes(fill=label), position = "dodge") + scale_fill_manual(values = Palette) +  theme_minimal(base_family="Arial Narrow") + 
  labs(x="Church Attendance", y="Percentage of Respondents", title="Evangelical Voters", subtitle="", caption="Data from CCES 2016") + 
  theme(legend.position="bottom") + labs(fill="")


## Comparing by Education

wpct(trump$educ, trump$commonweight_post)
## 8% Less than HS 34% HS Grad 24.8% Some College 11.5% 2 yr college 15.2% 4 yr college 6.4% Grad Degree
wpct(ntrump$educ, ntrump$commonweight_post)
## 5.1% Less than HS 25.7% HS Grad 23.9% Some College 10.6% 2 yr college 22.1% 4 yr college 12.5% Grad Degree


teduc  <- data.frame("class" =c("Less than HS", "HS Grad", "Some College", "2 yr College","4 yr College", "Grad Degree"), pct = c(8,34,24.8,11.5,15.2,6.4))
nteduc <- data.frame("class" =c("Less than HS", "HS Grad", "Some College", "2 yr College","4 yr College", "Grad Degree"), pct = c(5.1,25.7,23.9,10.6,22.1,12.5))
teduc$class <- factor(teduc$class, levels=unique(teduc$class))
nteduc$class <- factor(nteduc$class, levels=unique(nteduc$class))
teduc$label <- c("Trump")
nteduc$label <- c("Non-Trump")

educ <- rbind(teduc, nteduc)

ggplot(educ, aes(x=class, y=pct)) + geom_col(aes(fill=label), position = "dodge") + scale_fill_manual(values = Palette) +  theme_minimal(base_family="Arial Narrow") + 
  labs(x="Church Attendance", y="Percentage of Respondents", title="Evangelical Voters", subtitle="", caption="Data from CCES 2016") + 
  theme(legend.position="bottom") + labs(fill="")



## Making the Age Chart

age1 <- ggplot(trump, (aes(x=age))) + geom_bar(fill = "red3", colour = "black") + geom_vline(xintercept = 57.3, linetype =2) +  theme_minimal(base_family="Arial Narrow") + labs(x="Age", y="Number of Respondents", 
                                                                                                 title="Evangelical Trump Voters",
                                                                                                 subtitle="Mean = 57.3",
                                                                                                 caption="Data from CCES 2016")

age2 <- ggplot(ntrump, (aes(x=age))) + geom_bar(fill = "gray61", colour = "black") + geom_vline(xintercept = 52.5, linetype =2) +  theme_minimal(base_family="Arial Narrow") + labs(x="Age", y="Number of Respondents", 
                                                                                                                                           title="Evangelical Non-Trump Voters",
                                                                                                                                           subtitle="Mean = 52.5",
                                                                                                                                           caption="Data from CCES 2016")
grid.arrange(age1, age2, ncol=1)

                                                                                                                                                                        

