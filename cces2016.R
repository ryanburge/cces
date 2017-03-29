cces16 <- read_dta("C:/Users/Ryan Burge/Desktop/cces.dta")


cces16$white <- Recode(cces16$race, "1=1; else=0")


cces16$black <- Recode(cces16$race, "2=1; else=0")

## Vote 2016 

cces16$vote16 <- cces16$CC16_410a

cces16$vote16 <- as.numeric(cces16$vote16)

cces16$hiattend <- Recode(cces16$pew_churatd, "1:3=1; else=0")

cces$repubid <-Recode(cces$pid3, "2=1; else=0")

cces16$repubid7 <-Recode(cces16$pid7, "5:7=1; else=0")
cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=0")
#bagain16 <- filter(cces16, bagain ==1)

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



wpct(evan16$vote16, evan16$commonweight_post)
##75.8%  18.8%

wpct(bagain16$vote16, bagain16$commonweight_post)
##61.2%  33.7%

baprot16 <- filter(cces16, bagain ==1 & religpew == 1)
wpct(baprot16$vote16, baprot16$commonweight_post)
## 65.5%  29.4%

attendevan16 <- filter(evan16, hiattend ==1 )
wpct(attendevan16$vote16, attendevan16$commonweight_post)
## 80.4%  13.5%

##PEW 
##81%  16%