library(dplyr)
library(car)
library(ggplot2)
library(extrafont)
library(weights)
library(forcats)
library(haven)



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



## Black Protestant

bprot <- filter(cces, black ==1 & religpew ==1)

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

## Atheist
cces$atheist <- Recode(cces$religpew, "9=1; else=0")

## Agnostic 
cces$agnostic <- Recode(cces$religpew, "10=1; else=0")


## Evangelical
wpct(evangelical$vote16, evangelical$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(75.8, 18.8, 2.5, .5 )
count <- as.numeric(count)
evan <- cbind(candidate, count)
evan <- as.data.frame(evan)
evan$count <- c(75.8, 18.8, 2.5, .5)
evan$candidate <- factor(evan$candidate, levels=unique(evan$candidate))
evan$tradition <- c("Evangelical")


## Black Protestant

bprot <- filter(cces, black ==1 & religpew ==1)
wpct(bprot$vote16, bprot$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(8.5, 88.1, .8, .8 )
count <- as.numeric(count)
bprot <- cbind(candidate, count)
bprot <- as.data.frame(bprot)
bprot$count <- c(8.5, 88.1, .8, .8 )
bprot$candidate <- factor(bprot$candidate, levels=unique(bprot$candidate))
bprot$tradition <- c("Black Protestant")

## Mormons 

mormon <- filter(cces, mormon ==1)
wpct(mormon$CC16_410a, mormon$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Evan McMullin", "Jill Stein")
count <- c(45, 26, 13.6, 12.9, 2.5 )
count <- as.numeric(count)
mormon <- cbind(candidate, count)
mormon <- as.data.frame(mormon)
mormon$count <- c(45, 26, 13.6, 12.9, 2.5 )
mormon$candidate <- factor(mormon$candidate, levels=unique(mormon$candidate))
mormon$tradition <- c("Mormon")

## Mainline
mainline <- filter(cces, mainline ==1)
wpct(mainline$vote16, mainline$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(51.5, 43.4, 2.6, .5 )
count <- as.numeric(count)
ml <- cbind(candidate, count)
ml <- as.data.frame(ml)
ml$count <- c(51.5, 43.4, 2.6, .5)
ml$candidate <- factor(ml$candidate, levels=unique(ml$candidate))
ml$tradition <- c("Mainline")

## Jewish
jewish <- filter(cces, jewish ==1)
wpct(jewish$vote16, jewish$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(26.9, 68.8, 2.7, .2 )
count <- as.numeric(count)
jewish <- cbind(candidate, count)
jewish <- as.data.frame(jewish)
jewish$count <- c(26.9, 68.8, 2.7, .2 )
jewish$candidate <- factor(jewish$candidate, levels=unique(jewish$candidate))
jewish$tradition <- c("Jewish")


## Catholic
catholic <- filter(cces, catholic ==1)
wpct(catholic$vote16, catholic$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(49.2, 46.0, 2.1, .7 )
count <- as.numeric(count)
catholic <- cbind(candidate, count)
catholic <- as.data.frame(catholic)
catholic$count <- c(49.2, 46.0, 2.1, .7 )
catholic$candidate <- factor(catholic$candidate, levels=unique(catholic$candidate))
catholic$tradition <- c("Catholic")

## Muslim
muslim <- filter(cces, muslim ==1)
wpct(muslim$vote16, muslim$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(14.7, 81.7, 0.4, 2.4 )
count <- as.numeric(count)
muslim <- cbind(candidate, count)
muslim <- as.data.frame(muslim)
muslim$count <- c(14.7, 81.7, 0.4, 2.4  )
muslim$candidate <- factor(muslim$candidate, levels=unique(muslim$candidate))
muslim$tradition <- c("Muslim")

## Atheist
atheist <- filter(cces, atheist ==1)
wpct(atheist$vote16, atheist$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(18.3, 73.3, 3.5, 3.1 )
count <- as.numeric(count)
atheist <- cbind(candidate, count)
atheist <- as.data.frame(atheist)
atheist$count <- c(18.3, 73.3, 3.5, 3.1 )
atheist$candidate <- factor(atheist$candidate, levels=unique(atheist$candidate))
atheist$tradition <- c("Atheist")

## Buddhist
buddhist <- filter(cces, buddhist ==1)
wpct(buddhist$vote16, buddhist$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(18, 67.2, 2.7, 9.3 )
count <- as.numeric(count)
buddhist <- cbind(candidate, count)
buddhist <- as.data.frame(buddhist)
buddhist$count <- c(18, 67.2, 2.7, 9.3  )
buddhist$candidate <- factor(buddhist$candidate, levels=unique(buddhist$candidate))
buddhist$tradition <- c("Buddhist")

## Hindu
hindu <- filter(cces, hindu ==1)
wpct(hindu$vote16, hindu$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(24.1, 74.8, 0, 0 )
count <- as.numeric(count)
hindu <- cbind(candidate, count)
hindu <- as.data.frame(hindu)
hindu$count <- c(24.1, 74.8, 0, 0)
hindu$candidate <- factor(hindu$candidate, levels=unique(hindu$candidate))
hindu$tradition <- c("Hindu")


## All In One Graph
all <- rbind(evan, ml, cath, mormon, jewish, muslim, atheist, buddhist, hindu, bprot)

all$candidate <- factor(all$candidate, levels=unique(all$candidate))
all$tradition <- factor(all$tradition, levels = c("Evangelical", "Mainline", "Black Protestant", "Mormon", "Catholic", "Jewish", "Muslim", "Hindu", "Buddhist", "Atheist"))

ggplot(all, aes(1, count)) + geom_col(aes(fill= fct_rev(candidate)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("darkgrey", "forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)  


## Just Protestants
prot <- rbind(evan, ml, bprot)

prot$candidate <- factor(prot$candidate, levels=unique(prot$candidate))

ggplot(prot, aes(1, count)) + geom_col(aes(fill= fct_rev(candidate)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)   

## Other Christians
oxtn <- rbind(catholic, mormon, jewish)

oxtn$candidate <- factor(oxtn$candidate, levels=unique(oxtn$candidate))

ggplot(oxtn, aes(1, count)) + geom_col(aes(fill= fct_rev(candidate)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("darkgrey", "forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)   

## Non christians
non <- rbind(atheist, buddhist, hindu, muslim)

non$candidate <- factor(non$candidate, levels=unique(non$candidate))

ggplot(non, aes(1, count)) + geom_col(aes(fill= fct_rev(candidate)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)   


