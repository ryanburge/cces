library(dplyr)
library(car)
library(ggplot2)
library(extrafont)

cces$black <- Recode(cces$race, "2=1; else=0")

## Vote 2016 

cces$vote16 <- Recode(cces$CC16_364c, "1=1; 2=2; 3=3; 4=4; else=0")

cces$vote16<-Recode(cces$vote16,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein'",
                    as.factor=TRUE)

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
evangelical <- filter(evangelical, black !=1)


## Mainline

cces$mlbaptist <- Recode(cces$religpew_baptist, "2=1; 4=1; else=0")
cces$mlmeth <- Recode(cces$religpew_methodist, "1=1; 90=1; else=0")
cces$mlluth <- Recode(cces$religpew_lutheran, "1=1; 4=1; else=0")
cces$mlpres <- Recode(cces$religpew_presby, "1:5=1; 90=1; else=0")
cces$mlchrist <- Recode(cces$religpew_christian, "2=1; else=0")
cces$mlcong <- Recode(cces$religpew_congreg, "1=1; 3=1; else=0")
cces$mlreform <- Recode(cces$religpew_reformed, "1:90=1; else=0")
cces$episp <- Recode(cces$religpew_episcop, "1:90=1; else=0")


## Catholic 
cces$catholic <- Recode(cces$religpew_catholic, "1:90=1; else=0")

## Mormon
cces$mormon <- Recode(cces$religpew_mormon, "1:90=1; else=0")

## Jewish
cces$jewish <- Recode(cces$religpew, "5=1; else=0")

## Muslim 
cces$muslim <- Recode(cces$religpew, "6=1; else=0")

## No Religion 
cces$atheist <- Recode(cces$religpew, "9:10=1; else=0")


## Trying to Plot 

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(6562, 2592, 640, 143)
vote <- cbind(candidate, count)
vote <- as.data.frame(vote)
vote$candidate <- factor(vote$candidate, levels=unique(vote$candidate))
vote$count <- c(6562, 2592, 640, 130)
vote$pct <- vote$count/9937

colors <- c("red","blue", "yellow", "green")
ggplot(vote, aes(x= candidate, y = pct*100)) + geom_col(fill = colors, colour = "black") + ggtitle("Vote Choice Among Evangelicals") + xlab("Candidate") + ylab("Percent of Evangelicals") +
theme(text=element_text(size=18, family="KerkisSans")) +
theme(plot.title = element_text(hjust = 0.5))





