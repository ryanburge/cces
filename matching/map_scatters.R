
# state <- cces16 %>% 
  # group_by(inputstate) %>% 
  # count()


ml <- cces16 %>% 
    filter(religpew_methodist ==1 | religpew_lutheran ==1 | religpew_presby==1| religpew_episcop ==1 | religpew_congreg==1) %>%  
    filter(race ==1) %>% 
    group_by(inputstate) %>% 
    count() %>% 
    rename(ml =n)


ml1 <- cces16 %>% 
  filter(religpew_methodist ==1 | religpew_lutheran ==1 | religpew_presby==1| religpew_episcop ==1 | religpew_congreg==1) %>%  
  filter(race ==1) %>% 
  filter(pew_bornagain ==1) %>% 
  group_by(inputstate) %>% 
  count() %>% 
  rename(ba =n)

mlwhite <- left_join(ml, ml1) %>% mutate(mlpct = ba/ml)


ba <- cces16 %>% 
  filter(religpew ==1) %>%  
  filter(race ==1) %>% 
  group_by(inputstate) %>% 
  count(ba =pew_bornagain) %>% 
  filter(ba == 1)

prot <- cces16 %>% 
  filter(religpew ==1) %>%  
  filter(race ==1) %>% 
  group_by(inputstate) %>% 
  count() %>% 
  rename(prot =n)
  
bawhite <- left_join(ba, prot) %>% mutate(bapct = n/prot)

# whiteml <- right_join(mlwhite, bawhite)

whiteml <- merge(mlwhite, bawhite, by= "inputstate")

whiteml <- to_factor(whiteml)

ggplot(whiteml, aes(x=bapct, y=mlpct)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  geom_text_repel(aes(label = inputstate)) + 
  theme(text=element_text(size=28, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x= "Percent of the White Population that is Born Again", y = "Percent of White Mainline Prot. Who Are Born Again", title = "Misclassification of Mainliners Among White Respondents", caption = "Data: CCES 2016")

ggsave(file="mainline_scatter_white.png", type = "cairo-png", width = 15, height = 12)

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


evan <- cces16 %>% 
  filter(evangelical==1) %>%  
  filter(race ==1) %>% 
  group_by(inputstate) %>% 
  count() %>% 
  rename(evan =n)


evan1 <- cces16 %>% 
  filter(evangelical==1) %>%  
  filter(race ==1) %>% 
  filter(pew_bornagain ==2) %>% 
  group_by(inputstate) %>% 
  count() %>% 
  rename(ba =n)

evanwhite <- left_join(evan, evan1) %>% mutate(evanpct = ba/evan)


ba <- cces16 %>% 
  filter(religpew ==1) %>%  
  filter(race ==1) %>% 
  group_by(inputstate) %>% 
  count(ba =pew_bornagain) %>% 
  filter(ba == 2)

prot <- cces16 %>% 
  filter(religpew ==1) %>%  
  filter(race ==1) %>% 
  group_by(inputstate) %>% 
  count() %>% 
  rename(prot =n)

bawhite <- left_join(ba, prot) %>% mutate(bapct = n/prot)

# whiteml <- right_join(mlwhite, bawhite)

whiteevan <- merge(evanwhite, bawhite, by= "inputstate")

whiteevan <- to_factor(whiteevan)

ggplot(whiteevan, aes(x=bapct, y=evanpct)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  geom_text_repel(aes(label = inputstate)) + 
  theme(text=element_text(size=28, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x= "Percent of the White Population that is Not Born Again", y = "Percent of the White Evangelicals Who Are Not Born Again", title = "Misclassification of Evangelicals Among White Respondents", caption = "Data: CCES 2016")

ggsave(file="evangelical_scatter_white.png", type = "cairo-png", width = 15, height = 12)

