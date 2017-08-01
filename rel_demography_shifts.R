library(tidyverse)
library(car)
library(forcats)
library(extrafont)

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

cces16$mlbaptist <- Recode(cces16$religpew_baptist, "2=1; 4=1; else=0")
cces16$mlmeth <- Recode(cces16$religpew_methodist, "1=1; 90=1; else=0")
cces16$mlluth <- Recode(cces16$religpew_lutheran, "1=1; 4=1; else=0")
cces16$mlpres <- Recode(cces16$religpew_presby, "1:5=1; 90=1; else=0")
cces16$mlchrist <- Recode(cces16$religpew_christian, "2=1; else=0")
cces16$mlcong <- Recode(cces16$religpew_congreg, "1=1; 3=1; else=0")
cces16$mlreform <- Recode(cces16$religpew_reformed, "1:90=1; else=0")
cces16$episp <- Recode(cces16$religpew_episcop, "1:90=1; else=0")

cces16$mainline <- cces16$mlbaptist + cces16$mlmeth + cces16$mlluth + cces16$mlpres + cces16$mlchrist + cces16$mlcong + cces16$mlreform + cces16$episp
cces16$mainline <- Recode(cces16$mainline, "1:4=1; else=0")

cces16$nden <- Recode(cces16$religpew_protestant, "3=1; else =0")

cces16$vote16 <- cces16$CC16_410a

ndvote <-  cces16 %>% filter(nden == 1 & complete.cases(vote16) & race ==1) %>%
  count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% 
  mutate(label = c("Non-Denominational"))

evvote <-  cces16 %>% filter(evangelical ==1 & complete.cases(vote16) & race ==1) %>%
  count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>%  
  mutate(label = c("Evangelical"))

mlvote <-  cces16 %>% filter(mainline ==1 & complete.cases(vote16) & race ==1) %>%
  count(vote16, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>%  
  mutate(label = c("Mainline"))

vote <- bind_rows(ndvote, evvote,mlvote) %>% select(vote16, weight, label)

vote$vote16<-Recode(vote$vote16,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA")

vote$vote16 <- factor(vote$vote16, levels=unique(vote$vote16))

vote %>% filter(vote16 == "Donald Trump" | vote16 == "Hillary Clinton" | vote16 == "Gary Johnson" | vote16 == "Jill Stein" | vote16 == "Other") %>% ggplot(., aes(1, weight)) + geom_col(aes(fill=fct_rev(vote16)), colour = "black") + coord_flip() + facet_grid(label ~.)  +
  theme(legend.position = "bottom") + guides(fill = guide_legend(reverse=TRUE)) +
  scale_x_discrete(labels = c("")) + xlab("") + 
  ylab("Percent of Sample") + theme(legend.title=element_blank())  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + ggtitle("Denominational Differences in White Evangelical Protestants") +
  labs(caption = "Data from CCES 2016") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c( "gray", "chartreuse1", "gold1", "dodgerblue3", "firebrick1"))


cces16 <- cces16 %>% mutate(catholic = recode(religpew, "2=1; else =0"))
cces16 <- cces16 %>% mutate(mormon = recode(religpew, "3=1; else =0"))
cces16 <- cces16 %>% mutate(jewish = recode(religpew, "5=1; else =0"))
cces16 <- cces16 %>% mutate(muslim = recode(religpew, "6=1; else =0"))
cces16 <- cces16 %>% mutate(buddhist = recode(religpew, "7=1; else =0"))
cces16 <- cces16 %>% mutate(hindu = recode(religpew, "8=1; else =0"))
cces16 <- cces16 %>% mutate(atheist = recode(religpew, "9= 1; else =0"))
cces16 <- cces16 %>% mutate(agnostic = recode(religpew, "10= 1; else =0"))
cces16 <- cces16 %>% mutate(nip = recode(religpew, "11= 1; else =0"))

trad <- cces16 %>% select(V101, evangelical, mainline, catholic, mormon, jewish, muslim, buddhist, hindu, atheist, agnostic, nip) %>%  
  gather(reltrad, x1, evangelical:nip) %>% 
  filter(x1 ==1) %>% select(V101, reltrad) %>% 
  left_join(cces16)

jplot <- trad %>% 
  select(reltrad, birthyr) %>% 
  mutate(age = 2017 -birthyr) %>% 
  mutate(age = as.numeric(age), trad = as_factor(reltrad)) %>% 
  mutate(trad = recode(trad, "'muslim'= 'Muslim';
                         'evangelical'= 'Evangelical';
                         'mainline'= 'Mainline';
                         'nden'= 'Non-Denominational';
                         'catholic'= 'Catholic';
                         'mormon'= 'Mormon';
                         'jewish'= 'Jewish';
                         'buddhist'= 'Buddhist';
                         'hindu'= 'Hindu';
                         'atheist' = 'Atheist';
                         'agnostic' = 'Agnostic';
                         'nip' = 'Nothing in Particular'"))

jplot <- jplot %>% 
  group_by(trad) %>% 
  summarise(mean = mean(age)) %>% 
  arrange(mean) %>% 
  left_join(jplot)

ggplot(jplot,aes(x=age, y=fct_reorder(trad, mean), group=trad,  height=..density.., fill = trad)) +
  geom_joy(scale=4, alpha =0.6) +
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_continuous(expand=c(0, 0), limits = c(13,99)) + 
  labs(x= "Age", y= "Religious Tradition", title= "Age Distribution in the United States", caption = "Data: CCES 2016" ) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.position="none")

ggsave(file="age_joy_all_trads.png", type = "cairo-png", width = 20, height =12)


baplot <- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1) %>% 
  group_by(race) %>%  
  mutate(age = 2017 -birthyr) %>% 
  ungroup(race) %>% 
  select(race, age) %>% 
  mutate(age = as.numeric(age), race = as.factor(race)) %>% 
  mutate(race = recode(race, "1= 'White';
                         2= 'Black';
                         3= 'Latino';
                         4= 'Asian';
                         5= 'Native American';
                         6= 'Mixed Race';
                         7= 'Other';
                         8= 'Middle Eastern';
                         'hindu'= 'Hindu';
                         'atheist' = 'Atheist';
                         'agnostic' = 'Agnostic';
                         'nip' = 'Nothing in Particular'"))

baplot <- baplot %>% 
  group_by(race) %>% 
  summarise(mean = mean(age)) %>% 
  arrange(mean) %>% 
  left_join(baplot)


baplot %>% 
  filter(race != "Middle Eastern") %>% 
  ggplot(.,aes(x=age, y=fct_reorder(race, mean), group=race,  height=..density.., fill = race)) +
  geom_joy(scale=4, alpha =0.6) +
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_continuous(expand=c(0, 0), limits = c(13,99)) + 
  labs(x= "Age", y= "Race", title= "Age Distribution of Born Again Protestants in the United States", caption = "Data: CCES 2016" ) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.position="none")
  

