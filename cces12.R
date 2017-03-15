
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

cces12$mlbaptist <- Recode(cces12$religpew_baptist, "2=1; 4=1; else=0")
cces12$mlmeth <- Recode(cces12$religpew_methodist, "1=1; 90=1; else=0")
cces12$mlluth <- Recode(cces12$religpew_lutheran, "1=1; 4=1; else=0")
cces12$mlpres <- Recode(cces12$religpew_presby, "1:5=1; 90=1; else=0")
cces12$mlchrist <- Recode(cces12$religpew_christian, "2=1; else=0")
cces12$mlcong <- Recode(cces12$religpew_congreg, "1=1; 3=1; else=0")
cces12$mlreform <- Recode(cces12$religpew_reformed, "1:90=1; else=0")
cces12$episp <- Recode(cces12$religpew_episcop, "1:90=1; else=0")

cces12$mainline <- cces12$mlbaptist + cces12$mlmeth + cces12$mlluth + cces12$mlpres + cces12$mlchrist + cces12$mlcong + cces12$mlreform + cces12$episp

## Black Protestant

bprot <- filter(cces12, black ==1 & religpew ==1)

## Catholic 
cces12$catholic <- Recode(cces12$religpew_catholic, "1:90=1; else=0")

## Mormon
cces12$mormon <- Recode(cces12$religpew_mormon, "1:90=1; else=0")

## Jewish
cces12$jewish <- Recode(cces12$religpew, "5=1; else=0")

## Muslim 
cces12$muslim <- Recode(cces12$religpew, "6=1; else=0")

## Buddhist
cces12$buddhist <- Recode(cces12$religpew, "7=1; else=0")

## Hindus
cces12$hindu <- Recode(cces12$religpew, "8=1; else=0")

## No Religion 
cces12$atheist <- Recode(cces12$religpew, "9:10=1; else=0")



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


ml <- filter(cces12, mainline ==1)
wpct(ml$CC410a, ml$weight_vv_post)

bprot <- filter(cces12, black ==1 & religpew ==1)
wpct(bprot$CC410a, bprot$weight_vv_post)

mormon <- filter(cces12, mormon ==1)
wpct(mormon$CC410a, mormon$weight_vv_post)

catholic <- filter(cces12, catholic ==1)
wpct(catholic$CC410a, catholic$weight_vv_post)

jewish <- filter(cces12, jewish ==1)
wpct(jewish$CC410a, jewish$weight_vv_post)

muslim <- filter(cces12, muslim ==1)
wpct(muslim$CC410a, muslim$weight_vv_post)

hindu <- filter(cces12, hindu ==1)
wpct(hindu$CC410a, hindu$weight_vv_post)

buddhist <- filter(cces12, buddhist ==1)
wpct(buddhist$CC410a, buddhist$weight_vv_post)

atheist <- filter(cces12, atheist ==1)
wpct(atheist$CC410a, atheist$weight_vv_post)

