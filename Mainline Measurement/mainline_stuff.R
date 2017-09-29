library(haven)
library(tidyverse)
library(janitor)
library(scales)

cces08 <- read_dta("D://cces/data/cces2008.dta")
cces10 <- read_dta("D://cces/data/cces10.dta")
cces12 <- read_dta("D://cces/data/cces12.dta")
cces14 <- read_dta("D://cces/data/cces14.dta")
cces16 <- read_dta("D://cces/data/cces16.dta")


l16 <- cces16 %>% count(luth = religpew_lutheran, wt = commonweight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2016)) %>% filter(luth ==1)
l14 <- cces14 %>% count(luth = religpew_lutheran, wt = weight) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2014)) %>% filter(luth ==1)
l12 <- cces12 %>% count(luth = religpew_lutheran, wt = weight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA"))  %>% mutate(year = c(2012)) %>% filter(luth ==1)
l10 <- cces10 %>% count(luth = V225, wt = V101) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA"))  %>% mutate(year = c(2010)) %>% filter(luth ==1)
l08 <- cces08 %>% count(luth = V225, wt = V201) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2008)) %>% filter(luth ==1)

u16 <- cces16 %>% count(umc = religpew_methodist, wt = commonweight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2016)) %>% filter(umc==1)
u14 <- cces14 %>% count(umc = religpew_methodist, wt = weight) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2014)) %>% filter(umc==1)
u12 <- cces12 %>% count(umc = religpew_methodist, wt = weight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC"))  %>% mutate(year = c(2012)) %>% filter(umc==1)
u10 <- cces10 %>% count(umc = V223, wt = V101) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC"))  %>% mutate(year = c(2010)) %>% filter(umc==1)
u08 <- cces08 %>% count(umc = V223, wt = V201) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2008)) %>% filter(umc==1)

p16 <- cces16 %>% count(pres = religpew_presby, wt = commonweight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2016)) %>% filter(pres==1)
p14 <- cces14 %>% count(pres = religpew_presby, wt = weight) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2014)) %>% filter(pres==1)
p12 <- cces12 %>% count(pres = religpew_presby, wt = weight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA"))  %>% mutate(year = c(2012)) %>% filter(pres==1)
p10 <- cces10 %>% count(pres = V226, wt = V101) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA"))  %>% mutate(year = c(2010)) %>% filter(pres==1)
p08 <- cces08 %>% count(pres = V226, wt = V201) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2008)) %>% filter(pres==1)


e16 <- cces16 %>% count(ep = religpew_episcop, wt = commonweight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2016)) %>% filter(ep==1)
e14 <- cces14 %>% count(ep = religpew_episcop, wt = weight) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2014)) %>% filter(ep==1)
e12 <- cces12 %>% count(ep = religpew_episcop, wt = weight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA"))  %>% mutate(year = c(2012)) %>% filter(ep==1)
e10 <- cces10 %>% count(ep = V228, wt = V101) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA"))  %>% mutate(year = c(2010)) %>% filter(ep==1)
e08 <- cces08 %>% count(ep = V228, wt = V201) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2008)) %>% filter(ep==1)


c16 <- cces16 %>% count(ucc = religpew_congreg, wt = commonweight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2016)) %>% filter(ucc==1)
c14 <- cces14 %>% count(ucc = religpew_congreg, wt = weight) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2014)) %>% filter(ucc==1)
c12 <- cces12 %>% count(ucc = religpew_congreg, wt = weight_vv) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC"))  %>% mutate(year = c(2012)) %>% filter(ucc==1)
c10 <- cces10 %>% count(ucc = V230, wt = V101) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC"))  %>% mutate(year = c(2010)) %>% filter(ucc==1)
c08 <- cces08 %>% count(ucc = V230, wt = V201) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2008)) %>% filter(ucc==1)


long <- bind_rows(l08, l10, l12, l14, l16, u08, u10, u12, u14, u16, p08, p10, p12, p14, p16, e08, e10, e12, e14, e16, c08, c10, c12, c14, c16) %>% select(year, denom, pct)

long %>% 
  ggplot(., aes(x=year, y = pct, color = denom)) + 
  geom_line(size = 2) +  
  scale_y_continuous(labels = scales::percent) +
  labs(x="Year", y = "Percent of the Population", title = "Shifts in the Mainline Traditions", caption = "Data: CCES 2008-2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=40, family="KerkisSans")) + 
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) 

ggsave(file="mainline_shifts.png", type = "cairo-png", width = 15, height = 12)


l16 <- cces16 %>% filter(religpew_lutheran ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2016))
l14 <- cces14 %>% filter(religpew_lutheran ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2014))
l12 <- cces12 %>% filter(religpew_lutheran ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2012))
l10 <- cces10 %>% filter(V225 ==1) %>% filter(V211 ==1) %>% filter(V212d < 8) %>% summarise(mean = mean(V212d)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2010))
l08 <- cces08 %>% filter(V225 ==1) %>% filter(V211 ==1) %>% filter(CC307a < 8) %>% summarise(mean = mean(CC307a)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2008))


u16 <-cces16 %>% filter(religpew_methodist ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2016))
u14 <-cces14 %>% filter(religpew_methodist ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2014))
u12 <-cces12 %>% filter(religpew_methodist ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2012))
u10 <-cces10 %>% filter(V223 ==1) %>% filter(V211 ==1) %>% filter(V212d < 8) %>% summarise(mean = mean(V212d)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2010))
u08 <-cces08 %>% filter(V223 ==1) %>% filter(V211 ==1) %>% filter(CC307a < 8) %>% summarise(mean = mean(CC307a)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2008))


p16 <- cces16 %>% filter(religpew_presby ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2016))
p14 <-cces14 %>% filter(religpew_presby ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2014))
p12 <-cces12 %>% filter(religpew_presby ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2012))
p10 <-cces10 %>% filter(V226 ==1) %>% filter(V211 ==1) %>% filter(V212d < 8) %>% summarise(mean = mean(V212d)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2010))
p08 <-cces08 %>% filter(V226 ==1) %>% filter(V211 ==1) %>% filter(CC307a < 8) %>% summarise(mean = mean(CC307a)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2008))


e16 <-cces16 %>% filter(religpew_episcop ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2016))
e14 <-cces14 %>% filter(religpew_episcop ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2014))
e12 <-cces12 %>% filter(religpew_episcop ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2012))
e10 <-cces10 %>% filter(V228 ==1) %>% filter(V211 ==1) %>%  filter(V212d < 8) %>% summarise(mean = mean(V212d)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2010))
e08 <-cces08 %>% filter(V228 ==1) %>% filter(V211 ==1) %>% filter(CC307a < 8) %>% summarise(mean = mean(CC307a)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2008))


c16 <- cces16 %>% filter(religpew_congreg ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2016))
c14 <- cces14 %>% filter(religpew_congreg ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2014))
c12 <- cces12 %>% filter(religpew_congreg ==1) %>% filter(race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2012))
c10 <- cces10 %>% filter(V230 ==1) %>% filter(V211 ==1) %>% filter(V212d < 8) %>% summarise(mean = mean(V212d)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2010))
c08 <- cces08 %>% filter(V230 ==1) %>% filter(V211 ==1) %>% filter(CC307a < 8) %>% summarise(mean = mean(CC307a)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2008))

pid <- bind_rows(l08, l10, l12, l14, l16, u08, u10, u12, u14, u16, p08, p10, p12, p14, p16, e08, e10, e12, e14, e16, c08, c10, c12, c14, c16) %>% select(year, denom, mean)

a16 <- cces16 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2016))
a14 <- cces14 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2014))
a12 <- cces12 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% filter(pid7 < 8) %>% summarise(mean = mean(pid7)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2012))
a10 <- cces10 %>% filter(V219 ==1 & V215 ==2 & V211 ==1) %>% filter(V212d < 8) %>% summarise(mean = mean(V212d)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2010))
a08 <- cces08 %>% filter(V219 ==1 & V215 ==2 & V211 ==1) %>% filter(CC307a < 8) %>% summarise(mean = mean(CC307a)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2008))

pid <- bind_rows(pid, a16, a14, a12, a10, a08)

pid %>% 
  ggplot(., aes(x=year,y=mean, color  = denom)) +
  geom_line(size = 2) +  
  labs(x="Year", y = "Party Identification", title = "Shifts in the Mainline Traditions", caption = "Data: CCES 2008-2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=32, family="KerkisSans")) + 
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) +
  coord_flip() + 
  scale_y_continuous(limits = c(2,5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) +
  scale_color_manual(values = c("#F8766D", "#C49A00","black","#53B400", "#00C094", "#FB61D7", "#A58AFF")) 

ggsave(file="mainline_shifts_pid.png", type = "cairo-png", width = 15, height = 12)

piddot<- pid %>% group_by(denom) %>% summarise(mean = mean(mean)) %>% mutate(variable = c("PID"))

piddot %>% 
  ggplot(., aes(x = mean, y = variable))  +
  geom_point(shape=21, size =4, aes(fill = factor(denom))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  + scale_fill_manual(values = c("#F8766D", "#C49A00","black","#53B400", "#00C094", "#FB61D7", "#A58AFF")) + 
  theme(text=element_text(size=16, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Mean Party Identification", y ="", title = "Mainline Denominations and Party Identification") +
  scale_x_continuous(limits = c(2,5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) 

ggsave(file="mainline_pid_dotplot.png", type = "cairo-png", width = 15, height = 3)

piddot <- piddot %>% 
  add_row(denom = "All White ML Denoms", mean = 3.84794, variable = "PID")

piddot %>% 
  filter(denom == "All White ML Denoms" | denom == "Non-BA White Prot.") %>% 
  ggplot(., aes(x = mean, y = variable))  +
  geom_point(shape=21, size =4, aes(fill = factor(denom))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  + scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF")) + 
  theme(text=element_text(size=16, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Mean Party Identification", y ="", title = "Mainline Denominations and Party Identification") +
  scale_x_continuous(limits = c(2,5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) 

ggsave(file="mainline_pid_dotplot_2dots.png", type = "cairo-png", width = 15, height = 3)


cces16 <- cces16 %>% mutate(white = recode(race, "1=1; else =0"), black = recode(race, "2=1; else=0"), bagain = recode(pew_bornagain, "1=1; else =0"),  noba = recode(pew_bornagain, "2=1; else =0"), prot = recode(religpew, "1=1; else=0"))

cces16 <- cces16 %>% mutate(mainline = white + noba + prot) %>%  mutate(mainline = recode(mainline, "3=1; else=0"))


cces16$p1 <- to_factor(cces16$religpew_protestant)
mldenom <- cces16 %>% filter(mainline ==1) %>% count(p1, wt = commonweight_vv) %>% mutate(pct = prop.table(n)) %>% select(-n)

mldenom %>% 
  ggplot(., aes(x=reorder(p1,pct),y=pct)) +
  geom_col(fill = "darkorchid3", color = "black") +  
  labs(x="Denomination", y = "Percent of White Not Born Again Protestants", title = "Who are White Not Born Again Protestants?", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=32, family="KerkisSans")) + 
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) + coord_flip() +  
  scale_y_continuous(labels = scales::percent)
  
ggsave(file="whtnonbaprot_breakdown_2.png", type = "cairo-png", width = 15, height = 12)



bb16 <- cces16 %>% filter(religpew_lutheran ==1) %>%  filter(CC16_335 < 3) %>% count(CC16_335) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2016)) %>% filter(CC16_335 == 2)
bb14 <- cces14 %>% filter(religpew_lutheran ==1) %>% filter(CC14_327 < 3) %>% count(CC14_327) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2014)) %>% filter(CC14_327 == 2)
bb12 <- cces12 %>% filter(religpew_lutheran ==1) %>% filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2012)) %>% filter(CC326 ==2)
bb10 <- cces10 %>% filter(V225 ==1) %>%  filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ELCA")) %>% mutate(year = c(2010)) %>% filter(CC326 ==1)
bb08 <- cces08 %>% filter(V225 ==1) %>% filter(CC316f < 3) %>% count(CC316f) %>% mutate(pct = prop.table(n)) %>%  mutate(denom = c("ELCA")) %>% mutate(year = c(2008))  %>% filter(CC316f ==1)


ba16 <- cces16 %>% filter(religpew_methodist ==1) %>%  filter(CC16_335 < 3) %>% count(CC16_335) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2016)) %>% filter(CC16_335 == 2)
ba14 <- cces14 %>% filter(religpew_methodist ==1) %>% filter(CC14_327 < 3) %>% count(CC14_327) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2014)) %>% filter(CC14_327 == 2)
ba12 <- cces12 %>% filter(religpew_methodist ==1) %>% filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2012)) %>% filter(CC326 ==2)
ba10 <- cces10 %>% filter(V223 ==1) %>%  filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UMC")) %>% mutate(year = c(2010)) %>% filter(CC326 ==1)
ba08 <- cces08 %>% filter(V223 ==1) %>% filter(CC316f < 3) %>% count(CC316f) %>% mutate(pct = prop.table(n)) %>%  mutate(denom = c("UMC")) %>% mutate(year = c(2008))  %>% filter(CC316f ==1)


bc16 <- cces16 %>% filter(religpew_presby ==1) %>%  filter(CC16_335 < 3) %>% count(CC16_335) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2016)) %>% filter(CC16_335 == 2)
bc14 <- cces14 %>% filter(religpew_presby ==1) %>% filter(CC14_327 < 3) %>% count(CC14_327) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2014)) %>% filter(CC14_327 == 2)
bc12 <- cces12 %>% filter(religpew_presby ==1) %>% filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2012)) %>% filter(CC326 ==2)
bc10 <- cces10 %>% filter(V226 ==1) %>%  filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("PCUSA")) %>% mutate(year = c(2010)) %>% filter(CC326 ==1)
bc08 <- cces08 %>% filter(V226 ==1) %>% filter(CC316f < 3) %>% count(CC316f) %>% mutate(pct = prop.table(n)) %>%  mutate(denom = c("PCUSA")) %>% mutate(year = c(2008))  %>% filter(CC316f ==1)


bd16 <- cces16 %>% filter(religpew_episcop ==1) %>%  filter(CC16_335 < 3) %>% count(CC16_335) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2016)) %>% filter(CC16_335 == 2)
bd14 <- cces14 %>% filter(religpew_episcop ==1) %>% filter(CC14_327 < 3) %>% count(CC14_327) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2014)) %>% filter(CC14_327 == 2)
bd12 <- cces12 %>% filter(religpew_episcop ==1) %>% filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2012)) %>% filter(CC326 ==2)
bd10 <- cces10 %>% filter(V228 ==1) %>%  filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("ECUSA")) %>% mutate(year = c(2010)) %>% filter(CC326 ==1)
bd08 <- cces08 %>% filter(V228 ==1) %>% filter(CC316f < 3) %>% count(CC316f) %>% mutate(pct = prop.table(n)) %>%  mutate(denom = c("ECUSA")) %>% mutate(year = c(2008))  %>% filter(CC316f ==1)


be16 <- cces16 %>% filter(religpew_congreg ==1) %>%  filter(CC16_335 < 3) %>% count(CC16_335) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2016)) %>% filter(CC16_335 == 2)
be14 <- cces14 %>% filter(religpew_congreg ==1) %>% filter(CC14_327 < 3) %>% count(CC14_327) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2014)) %>% filter(CC14_327 == 2)
be12 <- cces12 %>% filter(religpew_congreg ==1) %>% filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2012)) %>% filter(CC326 ==2)
be10 <- cces10 %>% filter(V230 ==1) %>%  filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("UCC")) %>% mutate(year = c(2010)) %>% filter(CC326 ==1)
be08 <- cces08 %>% filter(V230 ==1) %>% filter(CC316f < 3) %>% count(CC316f) %>% mutate(pct = prop.table(n)) %>%  mutate(denom = c("UCC")) %>% mutate(year = c(2008))  %>% filter(CC316f ==1)


b16 <- cces16 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>%  filter(CC16_335 < 3) %>% count(CC16_335) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2016)) %>% filter(CC16_335 == 2)
b14 <- cces14 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% filter(CC14_327 < 3) %>% count(CC14_327) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2014)) %>% filter(CC14_327 == 2)
b12 <- cces12 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2012)) %>% filter(CC326 ==2)
b10 <- cces10 %>% filter(V219 ==1 & V215 ==2 & V211 ==1) %>%  filter(CC326 < 3) %>% count(CC326) %>% mutate(pct = prop.table(n)) %>% mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2010)) %>% filter(CC326 ==1)
b08 <- cces08 %>% filter(V219 ==1 & V215 ==2 & V211 ==1) %>% filter(CC316f < 3) %>% count(CC316f) %>% mutate(pct = prop.table(n)) %>%  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2008))  %>% filter(CC316f ==1)

gay <- bind_rows(b08, b10, b12, b14, b16, ba08, ba10, ba12, ba14, ba16, bb08, bb10, bb12, bb14, bb16, bc08, bc10, bc12, bc14, bc16, bd08, bd10, bd12, bd14, bd16, be08, be10, be12, be14, be16) %>% select(year, denom, pct)


gay %>% 
  ggplot(., aes(x=year,y=pct, color  = denom)) +
  geom_line(size = 2) +  
  labs(x="Year", y = "Percent Opposed to Gay Marriage", title = "Opposition to Gay Marriage", caption = "Data: CCES 2008-2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=32, family="KerkisSans")) + 
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) +
  scale_color_manual(values = c("#F8766D", "#C49A00","black","#53B400", "#00C094", "#FB61D7", "#A58AFF"))  +  
  scale_y_continuous(labels = scales::percent)

ggsave(file="gay_marriage_mainline_denoms.png", type = "cairo-png", width = 15, height = 12)



ci1 <- cces16 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% 
  filter(pid7 < 8) %>%
  summarise(mean = mean(pid7),
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2016))

ci2 <- cces16 %>% filter(religpew_methodist ==1 | religpew_lutheran ==1 | religpew_presby==1| religpew_episcop ==1 | religpew_congreg==1) %>%  
  filter(race ==1) %>% 
  filter(pid7 < 8) %>%
  summarise(mean = mean(pid7),
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2016))


ci3 <- cces14 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% 
  filter(pid7 < 8) %>%
  summarise(mean = mean(pid7),
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2014))

ci4 <- cces14 %>% filter(religpew_methodist ==1 | religpew_lutheran ==1 | religpew_presby==1| religpew_episcop ==1 | religpew_congreg==1) %>%  
  filter(race ==1) %>% 
  filter(pid7 < 8) %>%
  summarise(mean = mean(pid7),
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2014))

ci5 <- cces12 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% 
  filter(pid7 < 8) %>%
  summarise(mean = mean(pid7),
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2012))

ci6 <- cces12 %>% filter(religpew_methodist ==1 | religpew_lutheran ==1 | religpew_presby==1| religpew_episcop ==1 | religpew_congreg==1) %>%  
  filter(race ==1) %>% 
  filter(pid7 < 8) %>%
  summarise(mean = mean(pid7),
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2012))


ci7 <- cces10 %>% filter(V219 ==1 & V215 ==2 & V211 ==1) %>% filter(V212d < 8) %>% 
  filter(V212d < 8) %>%
  summarise(mean = mean(V212d),
            sd = sd(V212d), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2010))


ci8 <- cces10 %>% filter(V225 ==1 | V223 ==1 | V226==1| V228 ==1 | V230==1) %>%  
  filter(V211 ==1) %>% 
  filter(V212d < 8) %>%
  summarise(mean = mean(V212d),
            sd = sd(V212d), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2010))


ci9 <- cces08 %>% filter(V219 ==1 & V215 ==2 & V211 ==1) %>%  
  filter(CC307a < 8) %>%
  summarise(mean = mean(CC307a),
            sd = sd(CC307a), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2008))


ci10 <- cces08 %>% filter(V225 ==1 | V223 ==1 | V226==1| V228 ==1 | V230==1) %>%  
  filter(V211 ==1) %>% 
  filter(CC307a < 8) %>%
  summarise(mean = mean(CC307a),
            sd = sd(CC307a), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2008))


ci <- bind_rows(ci1, ci2, ci3, ci4, ci5, ci6, ci7, ci8, ci9, ci10) %>% mutate(variable = c("PID"))


ci %>% 
  ggplot(., aes(x = mean, y = year))  +
  geom_point(shape=21, size =4, aes(fill = factor(denom))) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper), height=.125, size = 1) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF")) + 
  theme(text=element_text(size=28, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Mean Party Identification", y ="", title = "Two Measures of Mainline Protestants", caption = "Data: CCES 2008-2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(2,5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) 

ggsave(file="mainline_pid_with_cis.png", type = "cairo-png", width = 15, height = 12)





ci1 <- cces16 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% 
  mutate(gaym = recode(CC16_335, "2=1; else=0")) %>% 
  # filter(pid7 < 8) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2016))

ci2 <- cces16 %>% filter(religpew_methodist ==1 | religpew_lutheran ==1 | religpew_presby==1| religpew_episcop ==1 | religpew_congreg==1) %>%  
  filter(race ==1) %>% 
  mutate(gaym = recode(CC16_335, "2=1; else=0")) %>% 
  # filter(pid7 < 8) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2016))


ci3 <- cces14 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% 
  mutate(gaym = recode(CC14_327, "2=1; else=0")) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2014))

ci4 <- cces14 %>% filter(religpew_methodist ==1 | religpew_lutheran ==1 | religpew_presby==1| religpew_episcop ==1 | religpew_congreg==1) %>%  
  filter(race ==1) %>% 
  mutate(gaym = recode(CC14_327, "2=1; else=0")) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2014))

ci5 <- cces12 %>% filter(religpew ==1 & pew_bornagain ==2 & race ==1) %>% 
  mutate(gaym = recode(CC326, "2=1; else=0")) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2012))

ci6 <- cces12 %>% filter(religpew_methodist ==1 | religpew_lutheran ==1 | religpew_presby==1| religpew_episcop ==1 | religpew_congreg==1) %>%  
  filter(race ==1) %>% 
  mutate(gaym = recode(CC326, "2=1; else=0")) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2012))


ci7 <- cces10 %>% filter(V219 ==1 & V215 ==2 & V211 ==1) %>% filter(V212d < 8) %>% 
  mutate(gaym = recode(CC326, "1=1; else=0")) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2010))


ci8 <- cces10 %>% filter(V225 ==1 | V223 ==1 | V226==1| V228 ==1 | V230==1) %>%  
  filter(V211 ==1) %>% 
  mutate(gaym = recode(CC326, "1=1; else=0")) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2010))


ci9 <- cces08 %>% filter(V219 ==1 & V215 ==2 & V211 ==1) %>%  
  mutate(gaym = recode(CC316f, "1=1; else=0")) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("Non-BA White Prot.")) %>% mutate(year = c(2008))


ci10 <- cces08 %>% filter(V225 ==1 | V223 ==1 | V226==1| V228 ==1 | V230==1) %>%  
  filter(V211 ==1) %>% 
  mutate(gaym = recode(CC316f, "1=1; else=0")) %>%
  summarise(mean = mean(gaym),
            sd = sd(gaym), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(denom = c("White Mainline Denoms.")) %>% mutate(year = c(2008))


ci <- bind_rows(ci1, ci2, ci3, ci4, ci5, ci6, ci7, ci8, ci9, ci10) %>% mutate(variable = c("Gay Marriage"))


ci %>% 
  ggplot(., aes(x = mean, y = year))  +
  geom_point(shape=21, size =4, aes(fill = factor(denom))) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper), height=.125, size = 1) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF")) + 
  theme(text=element_text(size=28, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Percent Opposed to Gay Marriage", y ="", title = "Two Measures of Mainline Protestants", caption = "Data: CCES 2008-2016", subtitle = "95% Confidence Intervals") 
  # scale_x_continuous(limits = c(2,5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) 

ggsave(file="mainline_ssm_with_cis.png", type = "cairo-png", width = 15, height = 12)





