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