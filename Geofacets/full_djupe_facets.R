library(tidyverse)
library(geofacet)
library(car)

states <- read_csv("https://raw.githubusercontent.com/ryanburge/cces/master/states.csv")

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

ml16 <- cces16 %>% 
  group_by(inputstate) %>% 
  count(mainline, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(inputstate) %>% 
  filter(mainline ==1) %>% 
  mutate(year = c("2016")) %>% 
  rename(fips = inputstate) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  ungroup(inputzip) %>% 
  select(state, weight, year)  

cces12$mlbaptist <- Recode(cces12$religpew_baptist, "2=1; 4=1; else=0")
cces12$mlmeth <- Recode(cces12$religpew_methodist, "1=1; 90=1; else=0")
cces12$mlluth <- Recode(cces12$religpew_lutheran, "1=1; 4=1; else=0")
cces12$mlpres <- Recode(cces12$religpew_presby, "1:5=1; 90=1; else=0")
cces12$mlchrist <- Recode(cces12$religpew_christian, "2=1; else=0")
cces12$mlcong <- Recode(cces12$religpew_congreg, "1=1; 3=1; else=0")
cces12$mlreform <- Recode(cces12$religpew_reformed, "1:90=1; else=0")
cces12$episp <- Recode(cces12$religpew_episcop, "1:90=1; else=0")

cces12$mainline <- cces12$mlbaptist + cces12$mlmeth + cces12$mlluth + cces12$mlpres + cces12$mlchrist + cces12$mlcong + cces12$mlreform + cces12$episp
cces12$mainline <- Recode(cces12$mainline, "1:4=1; else=0")

ml12 <- cces12 %>% 
  group_by(inputstate) %>% 
  count(mainline, wt = weight_vv) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(inputstate) %>% 
  filter(mainline ==1) %>% 
  mutate(year = c("2012")) %>% 
  rename(fips = inputstate) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  select(state, weight, year)   

cces08$mlbaptist <- Recode(cces08$V222, "2=1; 4=1; else=0")
cces08$mlmeth <- Recode(cces08$V223, "1=1; 90=1; else=0")
cces08$mlluth <- Recode(cces08$V225, "1=1; 4=1; else=0")
cces08$mlpres <- Recode(cces08$V226, "1:5=1; 90=1; else=0")
cces08$mlchrist <- Recode(cces08$V229, "2=1; else=0")
cces08$mlreform <- Recode(cces08$V232, "1:90=1; else=0")
cces08$episp <- Recode(cces08$V228, "1:90=1; else=0")

cces08$mainline <- cces08$mlbaptist + cces08$mlmeth + cces08$mlluth + cces08$mlpres + cces08$mlchrist +  cces08$mlreform + cces08$episp
cces08$mainline <- Recode(cces08$mainline, "1:4=1; else=0")
  
ml08 <- cces08 %>% 
  group_by(V206) %>% 
  count(mainline, wt = V201) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(mainline ==1) %>% 
  ungroup(V206) %>% 
  mutate(year = c("2008")) %>% 
  rename(fips = V206) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  select(state, weight, year)   
  
  
m1 <- bind_rows(ml08, ml12, ml16) %>% mutate(label = c("Mainline")) %>% rename(value = weight)

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

ev16 <- cces16 %>% 
  filter(race ==1) %>% 
  group_by(inputstate) %>% 
  count(evangelical, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(inputstate) %>% 
  filter(evangelical ==1) %>% 
  mutate(year = c("2016")) %>% 
  rename(fips = inputstate) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  ungroup(inputzip) %>% 
  select(state, weight, year) 

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

cces12$evangelical <- cces12$evanbaptist + cces12$evanmeth + cces12$evannd + cces12$evanluth + cces12$evanpres + cces12$pente + cces12$evanchrist + cces12$evancong + cces12$evanholy + cces12$evanadvent
cces12$evangelical <- Recode(cces12$evangelical, "1:4=1; else=0")

ev12 <- cces12 %>% 
  filter(race ==1) %>% 
  group_by(inputstate) %>% 
  count(evangelical, wt = weight_vv) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(inputstate) %>% 
  filter(evangelical ==1) %>% 
  mutate(year = c("2012")) %>% 
  rename(fips = inputstate) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  select(state, weight, year)     


cces2008$evanbaptist <- Recode(cces2008$V222, "1=1; 5:90=1; else=0")
cces2008$evanmeth <- Recode(cces2008$V223, "2=1; else=0")
cces2008$evannd <- Recode(cces2008$V224, "1:90=1; else=0")
cces2008$evanluth <- Recode(cces2008$V225, "2:3=1; else=0")
cces2008$evanpres <- Recode(cces2008$V226, "6=1; else=0")
cces2008$pente <- Recode(cces2008$V227, "1:90=1; else=0")
cces2008$evanchrist <- Recode(cces2008$V229, "1=1; 3:4=1; else=0")
cces2008$evancong <- Recode(cces2008$V230, "2=1; else=0")
cces2008$evanholy <- Recode(cces2008$V231, "1:90=1; else=0")
cces2008$evanreform <- Recode(cces2008$V232, "2=1; else=0")
cces2008$evanadvent <- Recode(cces2008$V233, "1:90=1; else=0")

cces2008$evangelical <- cces2008$evanbaptist + cces2008$evanmeth + cces2008$evannd + cces2008$evanluth + cces2008$evanpres + cces2008$pente + cces2008$evanchrist + cces2008$evancong + cces2008$evanholy + cces2008$evanadvent + cces2008$evanreform
cces2008$evangelical <- Recode(cces2008$evangelical, "1:4=1; else=0")

ev08 <- cces08 %>% 
  filter(V211 ==1) %>% 
  group_by(V206) %>% 
  count(evangelical, wt = V201) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(evangelical ==1) %>% 
  ungroup(V206) %>% 
  mutate(year = c("2008")) %>% 
  rename(fips = V206) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  select(state, weight, year)   

ev1 <- bind_rows(ev08, ev12, ev16) %>% mutate(label = c("Evangelical")) %>% rename(value = weight)

nones <- read_csv("https://raw.githubusercontent.com/ryanburge/cces/master/nones_states.csv") %>% rename(abb = stabbr)
nones <- melt(nones, id.vars =c("abb"))

none <- left_join(nones, states) %>% rename(year = variable) %>% mutate(label = c("Nones"))

t2 <- bind_rows(none, m1, ev1)

cces16$catholic <- Recode(cces16$religpew, "2=1; else=0")

cath16 <- cces16 %>% 
  group_by(inputstate) %>% 
  count(catholic, wt = commonweight) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(inputstate) %>% 
  filter(catholic ==1) %>% 
  mutate(year = c("2016")) %>% 
  rename(fips = inputstate) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  ungroup(inputzip) %>% 
  select(state, weight, year)

cces12$catholic <- Recode(cces12$religpew, "2=1; else=0")

cath12 <- cces12 %>% 
  group_by(inputstate) %>% 
  count(catholic, wt = weight_vv) %>% 
  mutate(weight = prop.table(n)) %>% 
  ungroup(inputstate) %>% 
  filter(catholic ==1) %>% 
  mutate(year = c("2012")) %>% 
  rename(fips = inputstate) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  select(state, weight, year)  

cces08$catholic <- Recode(cces08$V219, "2=1; else=0")

cath08 <- cces08 %>% 
  group_by(V206) %>% 
  count(catholic, wt = V201) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(catholic ==1) %>% 
  ungroup(V206) %>% 
  mutate(year = c("2008")) %>% 
  rename(fips = V206) %>%
  mutate(fips = as.numeric(fips)) %>% 
  left_join(states) %>% 
  select(state, weight, year)   

cath1 <- bind_rows(cath08, cath12, cath16) %>% mutate(label = c("Catholic")) %>% rename(value = weight)

t2 <- bind_rows(none, m1, ev1, cath1)

ggplot(t2, aes(x=year, y=value, fill =label, group = label, color = label)) + geom_line() + facet_geo(~state) +
  scale_x_discrete(breaks = c(2008, 2012, 2016), labels = function(x) paste0("'", substr(x, 3, 6))) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "The Changing Religious Landscape",
       caption = "Data Source: CCES 2008-2016",
       x = "Year",
       y = "% of Population") +
  theme(strip.text.x = element_text(size = 6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme_grey(base_size = 16) + theme(legend.title=element_blank())


ggsave(file="full_djupe_facet_with_catholic.png", type = "cairo-png", width = 20, height =12)

write.csv(t2, "djupe_data.csv")

