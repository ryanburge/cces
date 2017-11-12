library(dplyr)
library(car)
library(ggplot2)
library(extrafont)
library(weights)
library(forcats)
library(haven)



cces16 <- read_dta("C:/Users/Ryan Burge/Desktop/cces16.dta")

cces16$white <- Recode(cces16$race, "1=1; else=0")


cces16$black <- Recode(cces16$race, "2=1; else=0")

## Vote 2016 

cces16$vote16 <- cces16$CC16_410a

#cces16$vote16 <- as.numeric(cces16$vote16)

cces16$hiattend <- Recode(cces16$pew_churatd, "1:3=1; else=0")

cces16$repubid <-Recode(cces16$pid3, "2=1; else=0")

cces16$repubid7 <-Recode(cces16$pid7, "5:7=1; else=0")
cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=0")
cces16$newpid <- Recode(cces16$pid7, "8=4")

#cces16 <- filter(cces16, vote16 <=4)

cces16$vote16<-Recode(cces16$vote16,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA")

## Baptist

cces16 <- cces16 %>%
  mutate(sbc = recode(cces16$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces16 <- cces16 %>%
  mutate(abc = recode(cces16$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces16 <- cces16 %>%
  mutate(ibc = recode(cces16$religpew_baptist, "5=1; else=0")) 

cces16 <- cces16 %>%
  mutate(bgc = recode(cces16$religpew_baptist, "6=1; else=0")) 

cces16 <- cces16 %>%
  mutate(mbc = recode(cces16$religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

cces16 <- cces16 %>%
  mutate(cb = recode(cces16$religpew_baptist, "8=1; else=0")) 

cces16 <- cces16 %>%
  mutate(fwb = recode(cces16$religpew_baptist, "9=1; else=0")) 

cces16 <- cces16 %>%
  mutate(gabb = recode(cces16$religpew_baptist, "10=1; else=0")) 

cces16 <- cces16 %>%
  mutate(obc = recode(cces16$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

cces16 <- cces16 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces16 <- cces16 %>%
  mutate(fmc = recode(cces16$religpew_methodist, "2=1; else=0")) 

cces16 <- cces16 %>%
  mutate(omc = recode(cces16$religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

cces16 <- cces16 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces16 <- cces16 %>% 
  mutate(hiatt = recode(pew_churatd, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces16 <- cces16 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces16 <- cces16 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces16 <- cces16 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces16 <- cces16 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces16 <- cces16 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces16 <- cces16 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))


## Mainline

cces16$mlbaptist <- Recode(cces16$religpew_baptist, "2=1; 4=1; else=0")
cces16$mlmeth <- Recode(cces16$religpew_methodist, "1=1; 90=1; else=0")
cces16$mlluth <- Recode(cces16$religpew_lutheran, "1=1; 4=1; else=0")
cces16$mlpres <- Recode(cces16$religpew_presby, "1:5=1; 90=1; else=0")
cces16$mlchrist <- Recode(cces16$religpew_christian, "2=1; else=0")
cces16$mlcong <- Recode(cces16$religpew_congreg, "1=1; 3=1; else=0")
cces16$mlreform <- Recode(cces16$religpew_reformed, "1:90=1; else=0")
cces16$episp <- Recode(cces16$religpew_episcop, "1:90=1; else=0")

cces16$mainline <- cces16$mlbaptist + cces16$mlmeth + cces16$mlluth + cces16$mlpres + cces16$mlchrist + cces16$mlcong + cces16$mlreform + cces16$episp
cces16$mainline <- Recode(cces16$mainline, "1:4=1; else=0")



## Black Protestant

bprot <- filter(cces16, black ==1 & religpew ==1)

## Catholic 
cces16$catholic <- Recode(cces16$religpew_catholic, "1:90=1; else=0")

## Mormon
cces16$mormon <- Recode(cces16$religpew_mormon, "1:90=1; else=0")

## Jewish
cces16$jewish <- Recode(cces16$religpew, "5=1; else=0")

## Muslim 
cces16$muslim <- Recode(cces16$religpew, "6=1; else=0")

## Buddhist
cces16$buddhist <- Recode(cces16$religpew, "7=1; else=0")

## Hindus
cces16$hindu <- Recode(cces16$religpew, "8=1; else=0")

## Atheist
cces16$atheist <- Recode(cces16$religpew, "9=1; else=0")

## Agnostic 
cces16$agnostic <- Recode(cces16$religpew, "10=1; else=0")


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

bprot <- filter(cces16, black ==1 & religpew ==1)
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

mormon <- filter(cces16, mormon ==1)
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
mainline <- filter(cces16, mainline ==1)
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
jewish <- filter(cces16, jewish ==1)
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
catholic <- filter(cces16, catholic ==1)
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
muslim <- filter(cces16, muslim ==1)
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
atheist <- filter(cces16, atheist ==1)
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
buddhist <- filter(cces16, buddhist ==1)
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
hindu <- filter(cces16, hindu ==1)
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
all <- rbind(evan, ml, catholic, mormon, jewish, muslim, atheist, buddhist, hindu, bprot)

all$candidate <- factor(all$candidate, levels=unique(all$candidate))
all$tradition <- factor(all$tradition, levels = c("Evangelical", "Mainline", "Catholic", "Mormon", "Jewish", "Hindu", "Buddhist", "Atheist",  "Muslim","Black Protestant"))

ggplot(all, aes(1, count)) + geom_col(aes(fill= fct_rev(candidate)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + 
  scale_fill_manual(values=c("darkgrey", "forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)  

ggsave(file="vote16_all_trads.png", type = "cairo-png", width = 15, height = 10)



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


cces16 %>%  filter(evangelical ==1 & white ==1 & complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), trad = c("White Evangelical")) %>% arrange(desc(weight))
