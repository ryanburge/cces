library(tidyverse)
library(extrafont)
library(haven)
library(car)
library(janitor)
library(labelled)


cces16 <- read_dta("D://cces/data/cces16.dta")

e1<- cces16 %>% 
  group_by(inputstate) %>% 
  count(evangelical, wt =commonweight_vv) %>% 
  mutate(group = c("Evangelicals")) %>% 
  filter(evangelical ==1)

e2<- cces16 %>% 
   group_by(inputstate) %>% 
  count(wt =commonweight_vv) %>% 
  mutate(group = c("Entire Population"))


graph <- bind_cols(e1, e2) %>% mutate(pct = n/n1) %>% mutate(state = to_factor(inputstate)) 



theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.x =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 18),
       plot.title = element_text(family = "IBM Plex Serif", size = 32, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24)  
)
  
}


graph %>% 
  ggplot(., aes(x=reorder(state,pct), y= pct)) + geom_col(fill = "firebrick3") + scale_y_continuous(labels = scales::percent) + coord_flip() + theme_rb() +
  labs(x= "State", y= "Percent of Population that is Evangelical", title = "What States are the Most Evangelical?", caption = "Data: CCES 2016")


g1 <- cces16 %>% 
  group_by(inputstate) %>% 
  filter(evangelical ==1) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  arrange(-pct) %>% 
  mutate(state = to_factor(inputstate)) %>% 
  head(10)


g1 %>% 
  ggplot(., aes(x=reorder(state,pct), y= pct, fill = state)) + geom_col(fill = "firebrick3") + scale_y_continuous(labels = scales::percent) + coord_flip() + theme_rb() +
  labs(x= "State", y= "Percent of Evangelical Population that Voted for Trump", title = "What States has the Strongest Trump Evangelicals?", caption = "Data: CCES 2016")




abort <- cces16 %>% 
  mutate(abort = recode(CC16_301b, "1:2 =1; 3:5 =2; else =99")) %>% 
  filter(race ==1 & pew_bornagain==1 & religpew ==1 & abort != 99) %>% 
  group_by(inputstate) %>% 
  count(abort, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(abort ==1) %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(n > 35)


abort$color <- c("two", "one", "one", "one", "one", "one" , "one", "one", "one", "one", "one", "one", "one", "one", "one", "one" , "one", "one", "one", "one", "one")



theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.x =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 36, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =24),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24)  
)
  
}


abort %>% 
  ggplot(., aes(x=reorder(state,pct), y= pct, fill=color))  +scale_fill_manual(values = c("black", "firebrick1")) + geom_col() + scale_y_continuous(labels = scales::percent) + coord_flip() + theme_rb() +
  labs(x= "State", y= "Percent of White Evangelicals", title = "What States are the Most Pro-Life?", caption = "Data: CCES 2016", subtitle = "Among White Evangelicals")  + theme(legend.position="none")

ggsave(file="D://cces/abort_alabama.png", type = "cairo-png", width = 20, height =12)





cces16 %>% 
  mutate(gaym = recode(CC16_301n, "1:2 =1; 3:5 =2; else =99")) %>% 
  filter(race ==1 & pew_bornagain==1 & religpew ==1 & gaym != 99) %>% 
  group_by(inputstate) %>% 
  count(gaym, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(gaym ==1) %>% 
  arrange(-pct) %>% 
  mutate(state = to_factor(inputstate)) %>% as.data.frame() 


pid <- cces16 %>% 
  filter(race ==1 & pew_bornagain==1 & religpew ==1) %>% 
  filter(pid7 <8) %>% 
  group_by(inputstate) %>% 
  summarise(mean = mean(pid7), 
            sd = sd(pid7), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(state = to_factor(inputstate))
  


theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"), 
       panel.grid.minor.x =  element_line(colour = "gray48", size = .25),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 36, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =32),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =32)  
)
  
}

pid$color <- c("two", "one", "one", "one", "one", "one" , "one", "one", "one", "one", "one", "one", "one", "one", "one", "one" , "one", "one", "one", "one", "one", "one", "one", "one", "one", "one" , "one", "one", "one", "one", "one", "one", "one", "one", "one", "one" , "one", "one", "one", "one", "one","one", "one", "one", "one", "one" , "one", "one", "one", "one", "one")


pid %>% 
  filter(n >50) %>% 
  filter(state != "Nebraska") %>% 
  ggplot(., aes(x = mean, y = reorder(state, -mean), fill=color))  +scale_fill_manual(values = c("black", "firebrick1")) + scale_color_manual(values = c("black", "firebrick1")) +
  geom_point(shape=21, size =4, aes(fill = factor(color)), show.legend = TRUE) +  
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(color)), height=0, size = 1, show.legend = FALSE) + 
  # scale_color_manual(values = c("firebrick1", "black","#53B400", "#00C094", "#FB61D7", "#A58AFF", "grey", "red", "green")) +
  labs(x = "Party Identification", y ="", title = "Average Party ID of White Evangelicals", caption = "Data: CCES 2016", subtitle = "95% Confidence Intervals") + theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Democrat", "Not Strong Democrat", "Lean Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican"))  +  theme_rb( ) + theme(legend.position="none")
  
ggsave(file="D://cces/pid_alabama.png", type = "cairo-png", width = 20, height =15)



