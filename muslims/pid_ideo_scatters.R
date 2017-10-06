
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

trad <- trad %>% 
  mutate(reltrad = recode(reltrad, "'muslim'= 'Muslim';
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

scatter <- trad %>% 
  group_by(reltrad) %>% 
  filter(pid7 < 8) %>%
  filter(ideo5 < 6) %>% 
  summarise(pid = mean(pid7, na.rm = TRUE), ideo = mean(ideo5, na.rm = TRUE))

scatter %>% 
  ggplot(., aes(x=pid, y=ideo)) + geom_point() + geom_smooth(method = "lm") +
  geom_text_repel(aes(pid, ideo, label = reltrad), family = "KerkisSans", fontface = "bold", size =10) +
  labs(x = "Self Described Party Identification", y = "Self Described Ideology", title = "Relationship Between Party Identification and Political Ideology", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=32, family="KerkisSans")) +  
  theme(plot.title = element_text(face="bold"))  + theme(legend.position="none") +
  scale_x_continuous(limits = c(2.25,5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) +
  scale_y_continuous(limits = c(2,4), breaks = c(1,2,3,4,5), labels = c("Very Liberal", "Liberal", "Moderate", "Conservative", "Very Conservative")) + 
  geom_segment(aes(x = 2.75, y = 3.25, xend = 2.3, yend = 2.925), arrow = arrow(length = unit(.5, "cm")), size = 1.5)

 

ggsave(file="D:/cces/muslims/ideo_pid_scatter.png", type = "cairo-png", width = 18, height = 12)


trad %>% 
  filter(pid7 < 8) %>%
  filter(ideo5 < 6) %>% 
  mutate(pid7 = as.numeric(pid7), ideo = as.numeric(ideo5)) %>% 
ggplot(., aes(x=pid7, y=ideo)) + geom_point() + geom_jitter() + geom_smooth(method = "lm") + facet_grid(reltrad ~.)
  

ggsave(file="D:/cces/muslims/ideo_pid_facets.png", type = "cairo-png", width = 18, height = 18)

