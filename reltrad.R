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

## Catholic 
cces$catholic <- Recode(cces$religpew_catholic, "1:90=1; else=0")

## Mormon
cces$mormon <- Recode(cces$religpew_mormon, "1:90=1; else=0")

## Jewish
cces$jewish <- Recode(cces$religpew, "5=1; else=0")

## Muslim 
cces$muslim <- Recode(cces$religpew, "6=1; else=0")

## Buddhist
cces$buddhist <- Recode(cces$religpew, "7=1; else=0")

## Hindus
cces$hindu <- Recode(cces$religpew, "8=1; else=0")

## No Religion 
cces$atheist <- Recode(cces$religpew, "9:10=1; else=0")


## Trying to Plot 

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(6562, 2592, 640, 143)
evan <- cbind(candidate, count)
evan <- as.data.frame(evan)
evan$candidate <- factor(evan$candidate, levels=unique(evan$candidate))
evan$count <- c(66, 26, 6.4, 1.3)
evan$tradition <- c("Evangelical")


colors <- c("firebrick1","dodgerblue3", "goldenrod1", "forestgreen")
ggplot(evan, aes(x= candidate, y = pct*100)) + geom_col(fill = colors, colour = "black") + ggtitle("Vote Choice Among Evangelicals") + xlab("Candidate") + ylab("Percent of Evangelicals") +
theme(text=element_text(size=18, family="KerkisSans")) +
theme(plot.title = element_text(hjust = 0.5))


## With Weights
library(weights)
wpct(evangelical$vote16, evangelical$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(72, 22, 5, 1 )
count <- as.numeric(count)
evan <- cbind(candidate, count)
evan <- as.data.frame(evan)
evan$count <- c(72, 22, 5, 1)
evan$candidate <- factor(vote$candidate, levels=unique(vote$candidate))
evan$tradition <- c("Evangelical")

colors <- c("firebrick1","dodgerblue3", "goldenrod1", "forestgreen")
ggplot(vote, aes(x= candidate, y = count)) + geom_col(fill = colors, colour = "black") + ggtitle("Vote Choice Among Evangelicals (w/Weights)") + xlab("Candidate") + ylab("Percent of Evangelicals") +
  theme(text=element_text(size=18, family="KerkisSans")) +
  theme(plot.title = element_text(hjust = 0.5))

## Mormons 

mormon <- filter(cces, mormon ==1)
wpct(mormon$vote16, mormon$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Evan McMullin", "Jill Stein")
count <- c(50, 23, 13, 12, 2 )
count <- as.numeric(count)
mormon <- cbind(candidate, count)
mormon <- as.data.frame(mormon)
mormon$count <- c(45, 26, 13.6, 12.9, 2.5 )
mormon$candidate <- factor(mormon$candidate, levels=unique(mormon$candidate))
mormon$tradition <- c("Mormon")

colors <- c("firebrick1","dodgerblue3", "goldenrod1", "darkgrey", "forestgreen")
ggplot(mormon, aes(x= candidate, y = count)) + geom_col(fill = colors, colour = "black") + 
  ggtitle("Vote Choice Among Mormons") + 
  xlab("Candidate") + ylab("Percent of Votes Cast") +
  theme(text=element_text(size=18, family="KerkisSans")) +
  theme(plot.title = element_text(hjust = 0.5))

colors <- c("firebrick1","dodgerblue3", "goldenrod1", "darkgrey", "forestgreen")
ggplot(mormon, aes(1, count)) + geom_col(aes(fill=candidate), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("Mormons") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("firebrick1","dodgerblue3", "goldenrod1", "darkgrey", "forestgreen")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")

## Mainline
mainline <- filter(cces, mainline ==1)
wpct(mainline$vote16, mainline$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(50, 44.8, 4, 1 )
count <- as.numeric(count)
ml <- cbind(candidate, count)
ml <- as.data.frame(ml)
ml$count <- c(50, 44.8, 4, 1 )
ml$candidate <- factor(ml$candidate, levels=unique(ml$candidate))
ml$tradition <- c("Mainline")

## Catholic
catholic <- filter(cces, catholic ==1)
wpct(catholic$vote16, catholic$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(26, 70, 2.5, 1 )
count <- as.numeric(count)
cath <- cbind(candidate, count)
cath <- as.data.frame(cath)
cath$count <- c(26, 70, 2.5, 1  )
cath$candidate <- factor(cath$candidate, levels=unique(cath$candidate))
cath$tradition <- c("Catholic")


## Catholic
jewish <- filter(cces, jewish ==1)
wpct(jewish$vote16, jewish$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(45.5, 49.4, 4, 1 )
count <- as.numeric(count)
jewish <- cbind(candidate, count)
jewish <- as.data.frame(jewish)
jewish$count <- c(45.5, 49.4, 4, 1  )
jewish$candidate <- factor(jewish$candidate, levels=unique(jewish$candidate))
jewish$tradition <- c("Jewish")

## Muslim
muslim <- filter(cces, muslim ==1)
wpct(muslim$vote16, muslim$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(11.3, 82.7, 2.3, 3.5 )
count <- as.numeric(count)
muslim <- cbind(candidate, count)
muslim <- as.data.frame(muslim)
muslim$count <- c(11.3, 82.7, 2.3, 3.5 )
muslim$candidate <- factor(muslim$candidate, levels=unique(muslim$candidate))
muslim$tradition <- c("Muslim")

## Atheist
atheist <- filter(cces, atheist ==1)
wpct(atheist$vote16, atheist$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(17.6, 71, 6.7, 4.6 )
count <- as.numeric(count)
atheist <- cbind(candidate, count)
atheist <- as.data.frame(atheist)
atheist$count <- c(17.6, 71, 6.7, 4.6 )
atheist$candidate <- factor(atheist$candidate, levels=unique(atheist$candidate))
atheist$tradition <- c("Atheist")

## Buddhist
buddhist <- filter(cces, buddhist ==1)
wpct(buddhist$vote16, buddhist$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(24.3, 63.3, 3.1, 9.1 )
count <- as.numeric(count)
buddhist <- cbind(candidate, count)
buddhist <- as.data.frame(buddhist)
buddhist$count <- c(24.3, 63.3, 3.1, 9.1 )
buddhist$candidate <- factor(buddhist$candidate, levels=unique(buddhist$candidate))
buddhist$tradition <- c("Buddhist")

## Buddhist
hindu <- filter(cces, hindu ==1)
wpct(hindu$vote16, hindu$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(17.4, 81.7, .4, .3 )
count <- as.numeric(count)
hindu <- cbind(candidate, count)
hindu <- as.data.frame(hindu)
hindu$count <- c(17.4, 81.7, .4, .3 )
hindu$candidate <- factor(hindu$candidate, levels=unique(hindu$candidate))
hindu$tradition <- c("Hindu")


## Trying this With Facets. 
test <- rbind(evan, ml, cath, mormon, jewish, muslim, atheist, buddhist, hindu)

test$candidate <- factor(test$candidate, levels=unique(test$candidate))

ggplot(test, aes(1, count)) + geom_col(aes(fill= fct_rev(candidate)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("darkgrey", "forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)  

