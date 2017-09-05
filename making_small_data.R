library(tidyverse)
library(haven)
library(car)


cces16 <- read_dta("D://cces/data/cces16.dta")

small <- cces16 %>% 
  select(V101, inputstate, birthyr, gender, educ, race, marstat, CC16_302, CC16_303, CC16_304, CC16_307, CC16_330a, CC16_330b, CC16_330d, CC16_330e, CC16_331_1, CC16_331_2, CC16_331_3, CC16_331_7, CC16_332a, CC16_332f, CC16_335, employ, pid7, pew_churatd, religpew, CC16_410a, ideo5, union, faminc, sexuality) %>% 
  rename(id = V101, state = inputstate, marital = marstat, natecon = CC16_302, mymoney = CC16_303, econfuture = CC16_304, police = CC16_307, background = CC16_330a, registry = CC16_330b, assaultban = CC16_330d, conceal = CC16_330e, pathway = CC16_331_1, border = CC16_331_2, dreamer = CC16_331_3, deport = CC16_331_7, prochoice = CC16_332a, prolife = CC16_332f, gaym = CC16_335, religion = religpew, vote16 = CC16_410a, income = faminc)


small <- read_csv("D://cces/small_cces.csv")
