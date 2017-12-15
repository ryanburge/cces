


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

attgraph <- trad %>% 
  group_by(reltrad) %>% 
  count(pew_churatd, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(attend = to_factor(pew_churatd)) %>% 
  filter(pew_churatd == 6) %>% 
  arrange(-pct) %>% 
  select(-pew_churatd, -n, -attend)




theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 40, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =32),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =32), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =24, angle = 45, hjust = 1)
)
  
}




attgraph %>% 
  ggplot(., aes(x=reorder(reltrad, -pct), y = pct)) + geom_col(fill = "cornflowerblue", color = "black") + theme_rb() + scale_y_continuous(labels = scales::percent) +
  labs(x= "Religious Tradition", y ="Percent of Respondents", title = "What Percentage of Each Tradition Never Attends Church?", caption = "Data: CCES 2016")

ggsave(file="D://cces/never_attend.png", type = "cairo-png", width = 20, height =12)

