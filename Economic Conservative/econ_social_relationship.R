

cces16$abort1 <- recode(cces16$CC16_332a, "2=1; else=0")
cces16$abort2 <- recode(cces16$CC16_332b, "2=1; else=0")
cces16$abort3 <- recode(cces16$CC16_332c, "1=1; else=0")
cces16$abort4 <- recode(cces16$CC16_332d, "1=1; else=0")
cces16$abort5 <- recode(cces16$CC16_332e, "1=1; else=0")
cces16$abort6 <- recode(cces16$CC16_332f, "1=1; else=0")
cces16$gay <- recode(cces16$CC16_335, "2=1; else=0")
cces16 <- cces16 %>% mutate(social = abort1 + abort2 + abort3 + abort4 + abort5 + abort6 + gay)

cces16 %>% 
  filter(evangelical ==1 & race ==1 & CC16_415r <= 100) %>% 
  ggplot(., aes(x=social, y= CC16_415r, group =1)) + geom_point(shape =1, size =.5,   position=position_jitter(width=.5,height=5)) + 
  stat_smooth(method = loess) + 
  labs(x= "Social Conservatism Scale", y = "Economic Conservatism" , title="The Relationship Between Social Conservatism and Economic Conservatism (White Evangelicals)")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="econ_conserve_evan_scatter.png", type = "cairo-png", width = 20, height =12)

cces16 %>% 
  sample_frac(.35) %>% 
  filter(CC16_415r <= 100) %>% 
  ggplot(., aes(x=social, y= CC16_415r)) + geom_point(shape =1, size =.5,  position=position_jitter(width=.5,height=5)) + 
  geom_smooth(method = loess) + 
  labs(x= "Social Conservatism Scale", y = "Economic Conservatism" , title="The Relationship Between Social Conservatism and Economic Conservatism (Entire Sample)")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="econ_conserve_full_sample_scatter.png", type = "cairo-png", width = 20, height =12)


hist <- cces16 %>% filter(evangelical ==1 & race ==1 & CC16_415r <= 100) %>% tabyl(CC16_415r)
ggplot(hist, aes(x=CC16_415r, y= n)) + geom_col() + labs(x= "Economic Conservatism" , y= "Number of Respondents", title= "Economic Conservatism Among White Evangelicals")


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

jplot <- trad %>% 
  select(reltrad, CC16_415r) %>% 
  filter(CC16_415r <= 100) %>% 
  mutate(econ = as.numeric(CC16_415r), trad = as.factor(reltrad)) %>% 
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
  summarise(mean = mean(econ)) %>% 
  arrange(mean) %>% 
  left_join(jplot)

jplot %>% filter(trad != "Buddhist" & trad != "Hindu" & trad != "Muslim") %>%  
  ggplot(.,aes(x=econ, y=fct_reorder(trad, mean), group=trad,  height=..density.., fill = trad)) +
  geom_joy(scale=4, alpha =0.6) +
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_continuous(expand=c(0, 0), limits = c(0,100)) + 
  labs(x= "  <- Raise Taxes to Balance Budget: Reduce Spending to Balance Budget ->", y= "Religious Tradition", title= "Economic Conservatism and Religious Tradition in the United States", caption = "Data: CCES 2016" ) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=24, family="KerkisSans")) + theme(legend.position="none")

ggsave(file="econ_conserve_joyplot.png", type = "cairo-png", width = 20, height =12)


ggplot(econ, aes(x=issue, y=pct/100, fill = issue)) + 
  geom_col() + 
  facet_grid(. ~label) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Issue Area", y = "Percent Who Are Liberal", title = "Liberalism in the American Population") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=24, family="KerkisSans"))

ggsave(file="econ_conserve_histogram.png", type = "cairo-png", width = 12, height =12)


