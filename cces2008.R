library(haven)
cces2008 <- read_dta("C:/Users/Ryan Burge/Desktop/cces2008.dta")

cces2008$white <- Recode(cces2008$V211, "1=1; else=0")

cces2008$black <- Recode(cces2008$V211, "2=1; else=0")

cces2008$vote08 <- cces2008$CC410

cces2008$bagain <- cces2008$V215


cces2008$evanbaptist <- Recode(cces2008$V222, "1=1; 5:90=1; else=0")
cces2008$evanmeth <- Recode(cces2008$V223, "2=1; else=0")
cces2008$evannd <- Recode(cces2008$V224, "1:90=1; else=0")
cces2008$evanluth <- Recode(cces2008$V225, "2:3=1; else=0")
cces2008$evanpres <- Recode(cces2008$V226, "6=1; else=0")
cces2008$pente <- Recode(cces2008$V227, "1:90=1; else=0")
cces2008$evanchrist <- Recode(cces2008$V229, "1=1; 3:4=1; else=0")
cces2008$evancong <- Recode(cces2008$V230, "2=1; else=0")
cces2008$evanholy <- Recode(cces2008$V231, "1:90=1; else=0")
cces2008$evanreform <- Recode(cces2008$V232, "2=1; else=0")
cces2008$evanadvent <- Recode(cces2008$V233, "1:90=1; else=0")

evan08 <- filter(cces2008, evanbaptist == 1 | evanmeth == 1 | evannd == 1 | evanluth == 1 | evanpres == 1 | pente == 1 | evanchrist == 1 | evancong == 1 | evanholy == 1 | evanreform ==1 | evanadvent ==1)
evan08 <- filter(evan08, white ==1)

attendevan <- filter(evangelical, hiattend ==1)

cces2008$evangelical <- cces2008$evanbaptist + cces2008$evanmeth + cces2008$evannd + cces2008$evanluth + cces2008$evanpres + cces2008$pente + cces2008$evanchrist + cces2008$evancong + cces2008$evanholy + cces2008$evanadvent + cces2008$evanreform
cces2008$evangelical <- Recode(cces2008$evangelical, "1:4=1; else=0")

cces2008$protestant <- Recode(cces$V219, "1=1;else=0")

wpct(evan08$vote08, evan08$V201)
## 75.7%  22.7%

bagain08 <- filter(cces2008, bagain ==1)
wpct(bagain08$vote08, bagain08$V201)
##62.6% 36.2%

baprot08 <- filter(cces2008, bagain ==1 & V219 == 1)
wpct(baprot08$vote08, baprot08$V201)
## 70.0%  28.6%


cces2008$hiattend <- Recode(cces2008$V217, "1:3=1; else=0")
attendevan08 <- filter(cces2008, hiattend ==1 & evangelical ==1)
wpct(attendevan08$vote08, attendevan08$V201)
## 66%  33%

##PEW 
##74% 24%
