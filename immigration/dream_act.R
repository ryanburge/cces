library(haven)
library(tidyverse)
library(janitor)
library(scales)
library(car)
library(extrafont)

cces16 <- read_dta("D://cces/data/cces16.dta")


cces16 <- cces16 %>% mutate(white = recode(race, "1=1; else =0"), black = recode(race, "2=1; else=0"), bagain = recode(pew_bornagain, "1=1; else =0"),  noba = recode(pew_bornagain, "2=1; else =0"), prot = recode(religpew, "1=1; else=0"))

cces16 <- cces16 %>% 
  mutate(evangelical = white + bagain + prot) %>% mutate(evangelical = recode(evangelical, "3=1; else=0")) %>% 
  mutate(mainline = white + noba + prot) %>%  mutate(mainline = recode(mainline, "3=1; else=0")) %>%
  mutate(blackprot = black + prot) %>% mutate(blackprot = recode(blackprot, "2=1; else=0")) %>% 
  mutate(catholic = recode(religpew, "2=1; else=0")) %>% 
  mutate(jewish = recode(religpew, "5=1; else=0")) %>% 
  mutate(mormon = recode(religpew, "3=1; else=0")) %>% 
  mutate(atheist = recode(religpew, "9 =1; else=0")) %>% 
  mutate(agnostic = recode(religpew, "10 =1; else=0")) %>% 
  mutate(nip = recode(religpew, "11=1; else=0")) %>% 
  mutate(muslim = recode(religpew, "6=1; else =0")) %>%
  mutate(buddhist = recode(religpew, "7=1; else =0")) %>% 
  mutate(hindu = recode(religpew, "8=1; else =0"))

trad <- cces16 %>% select(V101, evangelical, mainline, blackprot, catholic, mormon, jewish, muslim, buddhist, hindu, atheist, agnostic, nip) %>%  
  gather(reltrad, x1, evangelical:nip) %>% 
  filter(x1 ==1) %>% select(V101, reltrad) %>% 
  left_join(cces16) 


cces16 <-  trad %>% mutate(reltrad = recode(reltrad, "'muslim'= 'Muslim';
                       'evangelical'= 'Wht. Evangelical';
                       'mainline'= 'Wht. Mainline';
                       'catholic'= 'Catholic';
                       'mormon'= 'Mormon';
                       'jewish'= 'Jewish';
                       'buddhist'= 'Buddhist';
                       'hindu'= 'Hindu';
                       'atheist' = 'Atheist';
                       'agnostic' = 'Agnostic';
                       'blackprot' = 'Black Protestant';    
                       'nip' = 'Nothing in Particular'"))

path <- cces16 %>% group_by(reltrad) %>% count(pathway = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% filter(pathway ==1) %>% select(reltrad, pct)
border <- cces16 %>% group_by(reltrad) %>% count(border = CC16_331_2) %>% mutate(pct = prop.table(n)) %>% filter(border ==1) %>% select(reltrad, pct)
dream <- cces16 %>% group_by(reltrad) %>% count(dream = CC16_331_3) %>% mutate(pct = prop.table(n)) %>% filter(dream ==1) %>% select(reltrad, pct)
deport <- cces16 %>% group_by(reltrad) %>% count(deport = CC16_331_7) %>% mutate(pct = prop.table(n)) %>% filter(deport ==1) %>% select(reltrad, pct)


cces16$vote16 <- as.numeric(cces16$CC16_410a)
cces16$vote16<-Recode(cces16$vote16,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA")

vote <- cces16 %>% group_by(reltrad) %>% count(vote16, wt = commonweight_vv_post) %>% mutate(pct = prop.table(n)) %>% filter(vote16 == "Donald Trump")


ggplot(vote, aes(reorder(x=reltrad, pct), y=pct)) + 
  geom_col(fill = "firebrick3", color = "black") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Religious Tradition", y= "Percent Of Each Group", title = "Percent Voting for Donald Trump", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="D://cces/immigration/vote16_reltrad.png", type = "cairo-png", width = 15, height = 12)




ggplot(path, aes(reorder(x=reltrad, pct), y=pct)) + 
  geom_col(fill = "cornflowerblue", color = "black") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Religious Tradition", y= "Percent of Each Group in Favor", title = "Pathway to Legal Status", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))


ggplot(border, aes(reorder(x=reltrad, pct), y=pct)) + 
  geom_col(fill = "cornflowerblue", color = "black") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Religious Tradition", y= "Percent of Each Group in Favor", title = "Increase Border Patrol", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))


ggplot(dream, aes(reorder(x=reltrad, pct), y=pct)) + 
  geom_col(fill = "cornflowerblue", color = "black") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Religious Tradition", y= "Percent of Each Group in Favor", title = "Legal Status to Dreamers", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="D://cces/immigration/dreamer_reltrad.png", type = "cairo-png", width = 15, height = 12)



ggplot(deport, aes(reorder(x=reltrad, pct), y=pct)) + 
  geom_col(fill = "cornflowerblue", color = "black") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Religious Tradition", y= "Percent of Each Group in Favor", title = "ID and Deport Illegal Immigrants", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

cces16 %>% filter(reltrad == "Wht. Evangelical") %>% filter(CC16_410a ==1) %>% count(dream = CC16_331_3) %>% mutate(pct = prop.table(dream))

cces16 %>% filter(reltrad == "Wht. Evangelical") %>% filter(CC16_410a ==2) %>% count(dream = CC16_331_3) %>% mutate(pct = prop.table(dream))

a1 <- cces16 %>% filter(reltrad == "Wht. Evangelical") %>%  count(dream = CC16_331_3) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("All")) %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Evangelicals"))
a2 <- cces16 %>% filter(reltrad == "Wht. Evangelical") %>%  filter(CC16_410a ==1) %>% count(dream = CC16_331_3) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("Trump"))   %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Evangelicals"))
a3 <- cces16 %>% filter(reltrad == "Wht. Evangelical") %>%  filter(CC16_410a ==2) %>% count(dream = CC16_331_3) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("Clinton"))   %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Evangelicals"))

dream1 <- bind_rows(a1, a2, a3) %>% mutate(q = c("Legal Status to Dreamers"))

color <- c("dodgerblue3", "darkorchid3", "firebrick3",  "dodgerblue3", "darkorchid3", "firebrick3", "dodgerblue3", "darkorchid3", "firebrick3", "dodgerblue3", "darkorchid3", "firebrick3")

dream1 %>% 
  ggplot(., aes(x=type, y=pct)) + 
  geom_col(fill = color, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Religious Tradition", y= "Percent of Each Group in Favor", title = "Legal Status to Dreamers", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

           
b1 <- cces16 %>% filter(reltrad == "Wht. Evangelical") %>%  count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("All")) %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Mainline"))
b2 <- cces16 %>% filter(reltrad == "Wht. Evangelical") %>%  filter(CC16_410a ==1) %>% count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("Trump"))   %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Mainline"))
b3 <- cces16 %>% filter(reltrad == "Wht. Evangelical") %>%  filter(CC16_410a ==2) %>% count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("Clinton"))   %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Mainline"))


c1 <- cces16 %>% filter(reltrad == "Mormon") %>%  filter(race ==1) %>% count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("All")) %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Mormons"))
c2 <- cces16 %>% filter(reltrad == "Mormon") %>%  filter(race ==1) %>% filter(CC16_410a ==1) %>% count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("Trump"))   %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Mormons"))
c3 <- cces16 %>% filter(reltrad == "Mormon") %>%  filter(race ==1) %>% filter(CC16_410a ==2) %>% count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("Clinton"))   %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Mormons"))


d1 <- cces16 %>% filter(reltrad == "Catholic") %>% filter(race ==1) %>%  count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("All")) %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Catholics"))
d2 <- cces16 %>% filter(reltrad == "Catholic") %>% filter(race ==1) %>%  filter(CC16_410a ==1) %>% count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("Trump"))   %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Catholics"))
d3 <- cces16 %>% filter(reltrad == "Catholic") %>% filter(race ==1) %>%  filter(CC16_410a ==2) %>% count(dream = CC16_331_1) %>% mutate(pct = prop.table(n)) %>% mutate(type = c("Clinton"))   %>% filter(dream ==1) %>% select(type, pct) %>% mutate(group = c("White Catholics"))


comb <- bind_rows(a1, a2, a3, b1, b2, b3, c1, c2, c3, d1, d2, d3) %>% mutate(q = c("Pathway to Legal Status"))

comb$type <- as_factor(comb$type)
comb$type <- factor(comb$type,  levels = c("Trump", "All", "Clinton"))

color <- c("dodgerblue3", "firebrick3",  "dodgerblue3",  "firebrick3", "dodgerblue3",  "firebrick3", "dodgerblue3",  "firebrick3")

comb %>% 
  filter(type != "All") %>% 
  ggplot(., aes(x=fct_rev(type), y=pct)) + 
  geom_col(fill = color, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Vote Choice in 2016", y= "Percent of Each Group in Favor", title = "Legal Status to Dreamers", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=22, family="KerkisSans")) + facet_grid(.~group)

ggsave(file="D://cces/immigration/dreamer_facet_by_vote.png", type = "cairo-png", width = 15, height = 12)


attdream <- cces16 %>% filter(reltrad == "Wht. Evangelical") %>% 
  group_by(pew_churatd) %>% 
  count(dream = CC16_331_1) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(dream ==1) %>% 
  ungroup(pew_churatd) %>% 
  mutate(attend = as.numeric(pew_churatd)) %>% 
  filter(attend <=6) %>% 
  mutate(attend = recode(attend, "1 = 'Weekly+';
                         2=  'Weekly';
                         3= 'Monthly';
                         4= 'Yearly';
                         5= 'Seldom';
                         6= 'Never'"))
  



attdream$attend <- factor(attdream$attend , levels=unique(attdream$attend ))

Palette <- c("firebrick3", "dodgerblue3")

ggplot(attdream,aes(x=fct_rev(attend), y=pct)) + 
  geom_col(fill = "cornflowerblue",  color = "black") + 
 # scale_fill_manual(values = Palette) +
  labs(x="Church Attendance", y="Percentage of Respondents", title="White Evangelicals and the Dream Act", caption="Data from CCES 2016") + 
  theme(legend.position="bottom") + labs(fill="") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=40, family="KerkisSans"))

ggsave(file="D://cces/immigration/dreamer_facet_by_attend.png", type = "cairo-png", width = 15, height = 12)


race <- cces16 %>% filter(pew_bornagain ==1) %>%
  filter(religpew ==1) %>% 
  group_by(race) %>% 
  count(dream = CC16_331_1) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(dream ==1) %>% 
  ungroup(race) %>% 
  mutate(race = as.numeric(race)) %>% 
  filter(race <=8) %>% 
 mutate(race = recode(race, "1= 'White';
                         2=  'Black';
                         3= 'Hispanic';
                         4= 'Asian';
                         5= 'Native American';
                         6= 'Mixed';
                         7= 'Other';
                         8= 'Middle Eastern'"))
  
race$race <- factor(race$race , levels=unique(race$race ))


ggplot(race,aes(x=race, y=pct)) + 
  geom_col(fill = "cornflowerblue",  color = "black") + 
  # scale_fill_manual(values = Palette) +
  labs(x="Church Attendance", y="Percentage of Respondents", title="Born Again Protestants and the Dream Act", caption="Data from CCES 2016") + 
  theme(legend.position="bottom") + labs(fill="") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="D://cces/immigration/dreamer_facet_by_race.png", type = "cairo-png", width = 15, height = 12)

