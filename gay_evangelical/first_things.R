library(tidyverse)
library(extrafont)
library(haven)
library(car)
library(janitor)
library(labelled)
source("D://cces/ggthemes.R")

cces16 <- read_dta("D://cces/data/cces16.dta")

cces16 <- cces16 %>% 
  mutate(white = recode(race, "1=1; else=0"))

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

## Making Mainline

cces16 <- cces16 %>% 
  mutate(abc = recode(cces16$religpew_baptist, "2=1; 4=1; else=0"))

cces16 <- cces16 %>% 
  mutate(epis = recode(cces16$religpew_episcop, "1:90=1; else=0"))

cces16 <- cces16 %>% 
  mutate(luth = recode(cces16$religpew_lutheran, "1=1; 4=1; else=0"))

cces16 <- cces16 %>% 
  mutate(meth = recode(cces16$religpew_methodist, "1=1; 90=1; else=0"))

cces16 <- cces16 %>% 
  mutate(pres = recode(cces16$religpew_presby, "1=1; 90=1; else=0"))

cces16 <- cces16 %>% 
  mutate(cong = recode(cces16$religpew_congreg, "1=1; 3=1; 90=1; else=0"))

cces16 <- cces16 %>% 
  mutate(doc = recode(cces16$religpew_protestant, "8=1; else=0"))

cces16 <- cces16 %>% 
  mutate(reform = recode(cces16$religpew_protestant, "11=1; else=0"))

cces16 <- cces16 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

cces16 <- cces16 %>% 
  mutate(black = recode(race, "2=1; else=0"))

cces16 <- cces16 %>% 
  mutate(meth = recode(cces16$religpew_methodist, "3:4=1; else=0"))

cces16 <- cces16 %>%
  mutate(sbc = recode(cces16$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces16 <- cces16 %>% 
  mutate(nbap = recode(cces16$religpew_baptist, "3=1; else=0"))

cces16 <- cces16 %>%
  mutate(abc = recode(cces16$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces16 <- cces16 %>%
  mutate(miss = recode(cces16$religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces16 <- cces16 %>%
  mutate(obap = recode(cces16$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces16 <- cces16 %>%
  mutate(ometh = recode(cces16$religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces16 <- cces16 %>% 
  mutate(apos = recode(cces16$religpew_pentecost, "6=1; 7=1; else=0"))

cces16 <- cces16 %>%
  mutate(open = recode(cces16$religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces16 <- cces16 %>%
  mutate(holy = recode(cces16$religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


cces16 <- cces16 %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

cces16 <- cces16 %>% 
  mutate(catholic = recode(religpew, "2=1; else=0"))

cces16 <- cces16 %>% 
  mutate(jewish = recode(religpew, "5=1; else=0"))

cces16 <- cces16 %>% 
  mutate(other = recode(religpew, "3=1; 6:8=1; 12=1; else=0"))

cces16 <- cces16 %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))

cces16 <- cces16 %>% 
  mutate(lgb = recode(sexuality, "2:5=1; else=0")) %>% 
  mutate(evanlgb = lgb + evangelical) %>% 
  mutate(evanlgb = recode(evanlgb, "2=1; else =0"))


lgb <- cces16 %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5) %>% 
  mutate(new = "LGB") 

evanlgb <- cces16 %>% 
  filter(evangelical == 1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5) %>% 
  mutate(new = "LGB + Evangelical") 

evan <- cces16 %>% 
  filter(evangelical == 1) %>% 
  mutate(new = "Evangelical") 

cces16 <- bind_rows(lgb, evanlgb, evan)
