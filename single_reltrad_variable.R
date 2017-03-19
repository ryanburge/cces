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
#cces$protestant <- Recode(cces$religpew, "1=1; else=0")
#cces$bprot <- cces$black + cces$protesant
#cces$bprot <- Recode(cces$bprot, "2=3; else=0")

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