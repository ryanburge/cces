library(dplyr)
library(car)
library(ggplot2)
library(extrafont)
library(weights)




cces <- read.csv("cces.csv")

cces$repubid <- Recode(cces$pid7, "8=4")

cces$bagain <- Recode(cces$pew_bornagain, "1=1; else=0")

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


bagain <- filter(cces, bagain ==1)
whtba <- filter(bagain, white ==1)


## 28.2% of the total sample indicate born again

baprot <- filter(cces, bagain ==1 & religpew == 1)
baevan <- filter(cces, bagain ==1 & evangelical ==1)

cathba <- filter(cces, bagain ==1 & catholic ==1) 
## Total: 14877 BA: 1780 11.9%

cathbablk <- filter(cces, bagain ==1 & catholic ==1 & black ==1)
cathblk <- filter(cces, catholic ==1 & black ==1)

cathwht <- filter(cces, catholic ==1 & white ==1)
cathbawht <- filter(cces, bagain ==1 & catholic ==1 & white ==1)
## 12.7% of white Catholics say born again
## 28.1% of black Catholics say born again

evanba <- filter(cces, bagain ==1 & evangelical ==1) 
## Total: 12384 BA: 10655 86.% 

mlba <- filter(cces, bagain ==1 & mainline ==1)
## Total: 8376 BA: 2559 30.5%

jewba <- filter(cces, bagain ==1 & jewish ==1)
## Total: 1546 BA: 69 4.5%

mormonba <- filter(cces, bagain ==1 & mormon ==1)
## Total: 950 BA: 214 22.5%

bprotba <- filter(cces, black ==1 & bagain ==1)
## Total: 4269 BA: 3606 84.5% 



