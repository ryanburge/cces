
cces16$vote16 <- as.numeric(cces16$CC16_410a)

cces16$vote16<-Recode(cces16$vote16,"1='Donald Trump';
                      2='Hillary Clinton';
                      3='Gary Johnson';
                      4='Jill Stein';
                      5= 'Other';
                      6= 'Not Vote';
                      7= 'Not Sure';
                      8= 'Evan McMullin'; else = NA")



attend <- cces16 %>% 
  filter(white ==1 & religpew ==1 & pew_bornagain ==1) %>% 
  group_by(pew_churatd) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(vote16 == "Donald Trump" | vote16 == "Hillary Clinton") %>% 
  ungroup(pew_churatd) %>% 
  mutate(attend = as.numeric(pew_churatd)) %>% 
  filter(attend <=6) %>% 
  mutate(attend = recode(attend, "1 = 'Weekly+';
                                       2=  'Weekly';
                                       3= 'Monthly';
                                       4= 'Yearly';
                                       5= 'Seldom';
                                        6= 'Never'")) 




attend$attend <- factor(attend$attend , levels=unique(attend$attend ))

Palette <- c("firebrick3", "dodgerblue3")


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

ggplot(attend,aes(x=attend, y=pct, fill = factor(vote16))) + 
  geom_col(aes(fill=vote16), color = "black", position = "dodge") + 
  scale_fill_manual(values = Palette) +
  labs(x="Church Attendance", y="Percentage of Respondents", title=" Evangelical (self-ID) White Protestant Vote Choice by Attendance", caption="Data from CCES 2016") + 
  theme(legend.position="bottom") + labs(fill="") +
  scale_y_continuous(labels = scales::percent) +
   theme_rb()





ggsave(file="attendance_vote16_whtbaprot.png", type = "cairo-png", width = 20, height = 12)

educ<- cces16 %>% 
  filter(white ==1 & religpew ==1 & pew_bornagain ==1) %>% 
  group_by(educ) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(vote16 == "Donald Trump" | vote16 == "Hillary Clinton") %>% 
  ungroup(educ) %>% 
  mutate(educ = as.numeric(educ)) %>% 
  filter(educ <=6) %>% 
  mutate(educ = recode(educ, "1 = 'Less than HS';
                       2=  'HS Grad';
                       3= 'Some College';
                       4= 'Associates';
                       5= 'Bachelors';
                       6= 'Graduate'")) 

educ$educ <- factor(educ$educ  , levels=unique(educ$educ))


ggplot(educ,aes(x=educ, y=pct, fill = factor(vote16))) + 
  geom_col(aes(fill=vote16), color = "black", position = "dodge") + 
  scale_fill_manual(values = Palette) +
  labs(x="Level of Education", y="Percentage of Respondents", title="Born Again White Protestant Voters", caption="Data from CCES 2016") + 
  theme(legend.position="bottom") + labs(fill="") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=40, family="KerkisSans"))


ggsave(file="educ_vote16_whtbaprot.png", type = "cairo-png", width = 15, height = 12)
