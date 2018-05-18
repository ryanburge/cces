cces16 <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = age/100) %>% 
  mutate(income = recode(faminc, "31:99 = NA")) %>% 
  mutate(income = income/16) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else = NA")) %>% 
  mutate(attend = attend/6) %>% 
  mutate(male = recode(gender, "1=1; 2=0; else = NA")) %>% 
  mutate(repid = recode(pid7, "8:99 = NA")) %>% 
  mutate(repid = repid/7) %>% 
  mutate(ed = educ/6) %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  mutate(abort = recode(CC16_332a, "1=1; else=0")) %>% 
  mutate(gay = recode(CC16_335, "1=1; else=0"))
  

## Entire Sample ####


regress_a <- cces16 %>% 
  select(age2, income, ed, male, white, attend, abort, repid, evangelical, lgb)

regress_g <- cces16 %>% 
  select(age2, income, ed, male, white, attend, gay, repid, evangelical, lgb)

reg1_a <- glm(abort ~ ., data = regress_a)
dwplot(reg1_a)

reg1_g <- glm(gay ~ ., data = regress_g)
dwplot(reg1_g)

## Evangelical Subgroup ####

regress_a <- cces16 %>% 
  filter(evangelical ==1) %>% 
  select(age2, income, ed, male, white, attend, abort, repid, lgb)

regress_g <- cces16 %>% 
  filter(evangelical ==1) %>% 
  select(age2, income, ed, male, white, attend, gay, repid, lgb)

reg1_a <- glm(abort ~ ., data = regress_a)
reg1_g <- glm(gay ~ ., data = regress_g)




theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "azure3", size = .25, linetype = "dashed"), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       panel.grid.major.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"), 
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 24, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =18, angle = 45, hjust  =1),
       axis.text.y = element_text(family = "IBM Plex Serif", size =12, angle = 0, hjust  =1),
       legend.justification=c(0, 0), 
       legend.position=c(.75, .85),
       legend.background = element_rect(colour="grey80"), 
       legend.title = element_blank(), 
       legend.text=element_text(size=18)
       
)
  
}


a <- dwplot(reg1_a) %>% 
  relabel_predictors(c(age2 = "Age",
                       ed = "Education",
                       income = "Income",
                       male = "Male",
                       white = "White",
                       attend = "Church Attendance",
                       lgb = "LGB",
                       repid = "Republican ID")) + 
  geom_vline(xintercept = 0, colour = "black", linetype = "solid") +
  scale_x_continuous(limits = c(-.45,.4)) +
  labs(x= "Coefficient Estimate", y = "Variables", title = "Abortion Support") +
  theme_rb() + 
  scale_color_grey()  + theme(legend.position="none")


g <- dwplot(reg1_g) %>% 
  relabel_predictors(c(age2 = "Age",
                       ed = "Education",
                       income = "Income",
                       male = "Male",
                       white = "White",
                       attend = "Church Attendance",
                       lgb = "LGB",
                       repid = "Republican ID")) + 
  geom_vline(xintercept = 0, colour = "black", linetype = "solid") +
  scale_x_continuous(limits = c(-.45,.4)) +
  labs(x= "Coefficient Estimate", y = "", title = "SSM Support") +
  theme_rb() + 
  scale_color_grey()  + theme(legend.position="none")


grid.arrange(a, g, ncol =2)

## LGB Subgroup ####

regress_a <- cces16 %>% 
  filter(lgb ==1) %>% 
  select(age2, income, ed, male, white, attend, abort, repid, evangelical)

regress_g <- cces16 %>% 
  filter(lgb ==1) %>% 
  select(age2, income, ed, male, white, attend, gay, repid, evangelical)

reg1_a <- glm(abort ~ ., data = regress_a)
reg1_g <- glm(gay ~ ., data = regress_g)




theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "azure3", size = .25, linetype = "dashed"), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       panel.grid.major.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"), 
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 24, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =18, angle = 45, hjust  =1),
       axis.text.y = element_text(family = "IBM Plex Serif", size =12, angle = 0, hjust  =1),
       legend.justification=c(0, 0), 
       legend.position=c(.75, .85),
       legend.background = element_rect(colour="grey80"), 
       legend.title = element_blank(), 
       legend.text=element_text(size=18)
       
)
  
}


a <- dwplot(reg1_a) %>% 
  relabel_predictors(c(age2 = "Age",
                       ed = "Education",
                       income = "Income",
                       male = "Male",
                       white = "White",
                       attend = "Church Attendance",
                       evangelical = "Evangelical",
                       repid = "Republican ID")) + 
  geom_vline(xintercept = 0, colour = "black", linetype = "solid") +
  scale_x_continuous(limits = c(-.45,.4)) +
  labs(x= "Coefficient Estimate", y = "Variables", title = "Abortion Support") +
  theme_rb() + 
  scale_color_grey()  + theme(legend.position="none")


g <- dwplot(reg1_g) %>% 
  relabel_predictors(c(age2 = "Age",
                       ed = "Education",
                       income = "Income",
                       male = "Male",
                       white = "White",
                       attend = "Church Attendance",
                       evangelical = "Evangelical",
                       repid = "Republican ID")) + 
  geom_vline(xintercept = 0, colour = "black", linetype = "solid") +
  scale_x_continuous(limits = c(-.45,.4)) +
  labs(x= "Coefficient Estimate", y = "", title = "SSM Support") +
  theme_rb() + 
  scale_color_grey()  + theme(legend.position="none")


grid.arrange(a, g, ncol =2)