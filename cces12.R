
cces12 <- read_dta("C:/Users/Ryan Burge/Desktop/cces12.dta")

cces12$white <- Recode(cces12$race, "1=1; else=0")
cces12$black <- Recode(cces12$race, "2=1; else=0")

cces12$vote12 <- cces12$CC410a

cces12$bagain <- Recode(cces12$pew_bornagain, "1=1; else=0")
cces12$hiattend <- Recode(cces12$pew_churatd, "1:3=1; else=0")
bagain12 <- filter(cces12, bagain ==1)

## Evangelical

cces12$evanbaptist <- Recode(cces12$religpew_baptist, "1=1; 5:90=1; else=0")
cces12$evanmeth <- Recode(cces12$religpew_methodist, "2=1; else=0")
cces12$evannd <- Recode(cces12$religpew_nondenom, "1:90=1; else=0")
cces12$evanluth <- Recode(cces12$religpew_lutheran, "2:3=1; else=0")
cces12$evanpres <- Recode(cces12$religpew_presby, "6=1; else=0")
cces12$pente <- Recode(cces12$religpew_pentecost, "1:90=1; else=0")
cces12$evanchrist <- Recode(cces12$religpew_christian, "1=1; 3:4=1; else=0")
cces12$evancong <- Recode(cces12$religpew_congreg, "2=1; else=0")
cces12$evanholy <- Recode(cces12$religpew_holiness, "1:90=1; else=0")
cces12$evanadvent <- Recode(cces12$religpew_advent, "1:90=1; else=0")

evan12 <- filter(cces12, evanbaptist == 1 | evanmeth == 1 | evannd == 1 | evanluth == 1 | evanpres == 1 | pente == 1 | evanchrist == 1 | evancong == 1 | evanholy == 1 | evanadvent ==1)
evan12 <- filter(evan12, white ==1)

wpct(evan12$vote12, evan12$weight_vv_post)
##74.5%  23.9%

wpct(bagain12$vote12, bagain12$weight_vv_post)
##62.5% 35.8%

baprot12 <- filter(cces12, bagain ==1 & religpew == 1)
wpct(baprot12$vote12, baprot12$weight_vv_post)
## 64.5%  33.9%

attendevan12 <- filter(evan12, hiattend ==1 )
wpct(attendevan12$vote12, attendevan12$weight_vv_post)
## 81.7%  16.6%

##PEW 
##78% 21%

