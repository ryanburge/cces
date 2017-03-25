library(dplyr)
library(car)
library(ggplot2)
library(extrafont)
library(weights)
library(haven)




cces16 <- read_dta("D://cces/data/cces.dta")

cces2008 <- read_dta("D:/cces/data/cces2008.dta")


cces16$white <- Recode(cces16$race, "1=1; else=0")


cces16$black <- Recode(cces16$race, "2=1; else=0")

cces16$repubid <- Recode(cces16$pid7, "8=4")

cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=0")
cces16$protestant <- Recode(cces16$religpew, "1=1; else=0")


cces16$candidate<-Recode(cces16$CC16_410a,"1='Republican';
                    2='Democrat';
                         3='Libertarian';
                         4='Green';
                         5= 'Other';
                         6= 'Not Vote';
                         7= 'Not Sure';
                         8= 'Evan McMullin'; else = NA")



cces12$candidate<-Recode(cces12$CC410a,"1='Democrat';
                    2='Republican';
                    3= 'Other';
                    4= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Skipped'; else = NA")

cces2008$candidate <- Recode(cces2008$CC327, "1= 'Republican';
                                          2= 'Democrat';
                                          3= 'Libertarian';
                                          4= 'Green';
                                          5= 'Libertarian';
                                          6= 'Green';
                                          7= 'Other';
                                          8= 'Not Vote';
                                          9= 'Not Sure'; else = NA")




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

cces16$evangelical <- cces16$evanbaptist + cces16$evanmeth + cces16$evannd + cces16$evanluth + cces16$evanpres + cces16$pente + cces16$evanchrist + cces16$evancong + cces16$evanholy + cces16$evanadvent
cces16$evangelical <- Recode(cces16$evangelical, "1:4=1; else=0")

#evangelical <- filter(cces16, evanbaptist == 1 | evanmeth == 1 | evannd == 1 | evanluth == 1 | evanpres == 1 | pente == 1 | evanchrist == 1 | evancong == 1 | evanholy == 1 | evanadvent ==1)
#evangelical <- filter(evangelical, white ==1)




evan16 <- cces16 %>%  filter(evangelical ==1 & white ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), type = c("White Evangelical"), year = c("2016")) %>% arrange(desc(weight))

baprot16 <- cces16 %>%  filter(bagain ==1 & white ==1 & protestant ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), type = c("White BA + Prot"), year = c("2016")) %>% arrange(desc(weight))

baprot12 <- cces12 %>%  filter(bagain ==1 & white ==1 &  protestant ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), type = c("White BA + Prot"), year = c("2012")) %>% arrange(desc(weight))

evan12 <- cces12 %>%  filter(evangelical ==1 & white ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), type = c("White Evangelical"), year = c("2012")) %>% arrange(desc(weight))

baprot08 <- cces2008 %>%  filter(bagain ==1 & white ==1 &  V219 ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = V201) %>% 
  mutate(weight = prop.table(n), type = c("White BA + Prot"), year = c("2008")) %>% arrange(desc(weight))

evan08 <- cces2008 %>%  filter(evangelical ==1 & white ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = V201) %>% 
  mutate(weight = prop.table(n), type = c("White Evangelical"), year = c("2008")) %>% arrange(desc(weight))

voteplot <- rbind(evan16, evan12, evan08, baprot16, baprot12, baprot08)

ggplot(voteplot %>% filter(candidate == "Republican"), aes(x=year, y=weight, fill = type)) + geom_col(position = "dodge") +  
  theme(axis.ticks = element_blank()) + ylab("Percent Voting for the Republican") + 
  theme(legend.position="bottom") +
  ggtitle("Different Measures of Evangelicalism?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("chartreuse4","darkorange1", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Election Year") 


pidevan16 <- cces16 %>%  filter(evangelical ==1 & white ==1 & complete.cases(pid7)) %>% 
  count(pid7, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), type = c("White Evangelical"), year = c("2016")) %>% arrange(desc(weight))

pidbaprot16 <- cces16 %>%  filter(bagain ==1 & white ==1 & protestant ==1 & complete.cases(pid7)) %>% 
  count(pid7, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), type = c("White BA + Prot"), year = c("2016")) %>% arrange(desc(weight))


pidplot <- rbind(pidevan16, pidbaprot16)


ggplot(pidplot %>% filter(pid7 <= 7 ), aes(x=pid7, y=weight*100, fill = type)) + geom_col(position = "dodge") +  
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme(legend.position="bottom") +
  ggtitle("Different Measures of Evangelicalism?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("chartreuse4","darkorange1", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Self Identified Political Ideology")  +
  scale_x_continuous(limits = c(.5,7.5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Dem.", "Dem.", "Lean Dem.", "Independent", "Lean Rep.", "Rep.", "Strong. Rep"))







#bagain <- filter(cces, bagain ==1)
#whtba <- filter(bagain, white ==1)


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

cces %>%  filter(evangelical ==1 & white ==1 & complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n)) %>% arrange(desc(weight))



