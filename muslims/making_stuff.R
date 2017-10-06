library(haven)
library(tidyverse)
library(janitor)
library(scales)
library(labelled)

cces08 <- read_dta("D://cces/data/cces2008.dta")
cces10 <- read_dta("D://cces/data/cces10.dta")
cces12 <- read_dta("D://cces/data/cces12.dta")
cces14 <- read_dta("D://cces/data/cces14.dta")
cces16 <- read_dta("D://cces/data/cces16.dta")

cces16 <- cces16 %>% mutate(relig = to_factor(religpew))

gen <- cces16 %>% 
  filter(relig != "Something Else" | relig != "Eastern or Greek Orthodox" | relig != "Skipped") %>% 
  count(relig, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n))
# 
# gen <- gen[-4,]
# gen <- gen[-11,]
# gen <- gen[-11,]

gen <- gen %>% mutate(newpct = n/60449.7) %>% select(relig, newpct) %>% rename(pct = newpct) %>% mutate(pct = round(pct, digits =3))

gen %>% 
  ggplot(., aes(x=reorder(relig, pct), y=pct)) + geom_col(fill = "darkolivegreen4", color = "black") + coord_flip()  +
  labs(x = "Religious Tradition", y = "Percentage of the Population", title = "Muslims Make Up Less than 1% of the American Population", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=32, family="KerkisSans")) +  
  scale_y_continuous(labels = scales::percent)  +
  theme(plot.title = element_text(face="bold")) +
  geom_text(aes(x=relig, y=pct, label= paste0(pct*100,"%")), hjust = 0, nudge_x = 0.05, nudge_y = .0025, family = "KerkisSans", fontface = "bold", size =8) 
               
ggsave(file="D:/cces/muslims/bar_graph.png", type = "cairo-png", width = 17, height = 12)

cces16$CC16_410a <- as.numeric(cces16$CC16_410a)
cces16$vote16<-Recode(cces16$CC16_410a,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA")


vote <- cces16 %>% 
  filter(religpew == 6 & complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% 
  filter(vote16 != "Not Sure")

vote$vote16 <- factor(vote$vote16, levels=unique(vote$vote16))

ggplot(vote, aes(1, pct)) + geom_col(aes(fill= fct_rev(vote16)), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Voting Behavior of Muslims") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=32, family="KerkisSans")) + 
  scale_fill_manual(values=c("goldenrod1","darkgrey", "forestgreen", "firebrick1", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")   +  
  scale_y_continuous(labels = scales::percent)  +
  theme(plot.title = element_text(face="bold"))

ggsave(file="D:/cces/muslims/votes_graph.png", type = "cairo-png", width = 12, height = 3)

dots <- cces16 %>% group_by(religpew) %>%  
  filter(pid7 < 8) %>%
  summarise(mean = mean(pid7),
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(religpew = to_factor(religpew)) %>% 
  mutate(label = c("PID"))

pd <- position_dodge(width = 0.15)

dots %>% 
  filter(religpew != "Skipped") %>% filter(religpew != "Eastern or Greek Orthodox") %>% 
  ggplot(., aes(x = mean, y = reorder(religpew, mean)))  +
  geom_point(shape=21, size =4, aes(fill = factor(religpew))) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper), height=.25, size = 1) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + 
  #scale_fill_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF")) + 
  theme(text=element_text(size=28, family="KerkisSans"))   +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Mean Party Identification", y ="", title = "Muslims Are the Most Liberal Religious Group", caption = "Data: CCES 2016", subtitle = "95% Confidence Intervals") +
  scale_x_continuous(limits = c(2,5.5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) + 
  theme(legend.position="none")
# geom_text_repel(aes(x=mean, y=label, label= religpew) , hjust = 0, nudge_x = -0.075, nudge_y = .025, family = "KerkisSans", fontface = "bold", size =6, force = 2, max.iter = 3e3)

ggsave(file="D:/cces/muslims/stacked_dot_pid_cis.png", type = "cairo-png", width = 18, height = 12)


type <- cces16 %>% 
  filter(religpew_muslim != 98) %>% 
  count(religpew_muslim, wt =  commonweight_vv) %>% 
  rename(muslim = religpew_muslim) %>% 
  mutate(muslim = to_factor(muslim))

# # A tibble: 4 x 2
# muslim         n
# <fctr>     <dbl>
#   1                          Sunni 315.87285
# 2                           Shia  49.41209
# 3 Nation of Islam (Black Muslim)  96.75191
# 4                   Other Muslim  53.70877

type %>% 
  mutate(muslim = recode(muslim, "'Nation of Islam (Black Muslim)' = 'Nation of Islam'")) %>% 
  ggplot(., aes(x=reorder(muslim, -percent), y=percent)) + geom_col(fill = "darkolivegreen4", color = "black") +
  labs(x = "Muslim Tradition", y = "Percentage of the Population", title = "Muslim Groups in the United States", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=32, family="KerkisSans")) +  
  scale_y_continuous(labels = scales::percent)  +
  theme(plot.title = element_text(face="bold"))

ggsave(file="D:/cces/muslims/type_bar_graph.png", type = "cairo-png", width = 15, height = 12)


cces16 %>% 
  group_by(religpew_muslim) %>% 
  filter(complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(religpew_muslim) %>% 
  rename(muslim = religpew_muslim) %>% 
  mutate(muslim = to_factor(muslim)) %>% 
  select(-n) %>% 
  as.data.frame()

# muslim          vote16         pct
# 1                           Sunni    Donald Trump 0.074520872
# 2                           Sunni    Gary Johnson 0.003816324
# 3                           Sunni Hillary Clinton 0.901181087
# 4                           Sunni      Jill Stein 0.007445242
# 5                           Sunni        Not Sure 0.003964184
# 6                           Sunni           Other 0.009072292
# 7                            Shia    Donald Trump 0.126480137
# 8                            Shia Hillary Clinton 0.837462283
# 9                            Shia      Jill Stein 0.036057580
# 10 Nation of Islam (Black Muslim)    Donald Trump 0.328441855
# 11 Nation of Islam (Black Muslim)    Gary Johnson 0.005407560
# 12 Nation of Islam (Black Muslim) Hillary Clinton 0.545983274
# 13 Nation of Islam (Black Muslim)      Jill Stein 0.120167311
# 14                   Other Muslim    Donald Trump 0.485680502
# 15                   Other Muslim Hillary Clinton 0.435708219
# 16                   Other Muslim      Jill Stein 0.065667512
# 17                   Other Muslim           Other 0.012943767
# 18                        Skipped    Donald Trump 0.457307562
# 19                        Skipped   Evan McMullin 0.003127129
# 20                        Skipped    Gary Johnson 0.027402770
# 21                        Skipped Hillary Clinton 0.478772594
# 22                        Skipped      Jill Stein 0.011801931
# 23                        Skipped        Not Sure 0.002829109
# 24                        Skipped        Not Vote 0.001354232
# 25                        Skipped           Other 0.017404672




imm <- cces16 %>% 
  filter(religpew == 6) %>% 
  count(immstat, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(immstat !=8) %>% 
  mutate(immstat = to_factor(immstat)) 

imm %>% 
  ggplot(., aes(x=immstat, y= pct)) + geom_col(fill = "darkolivegreen4", color = "black") +
  labs(x = "Immigration Status", y = "Percentage of the Muslim Population", title = "Nearly Half the Muslims in the Sample are Immigrants", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=32, family="KerkisSans")) +  
  scale_y_continuous(labels = scales::percent)  +
  theme(plot.title = element_text(face="bold")) 

ggsave(file="D:/cces/muslims/immigration_bar_graph.png", type = "cairo-png", width = 17, height = 12)
