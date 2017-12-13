library(dotwhisker)
library(tidyr)
library(broom)

cces16 <- cces16 %>% 
  mutate(age = 2017 - birthyr) %>% 
  mutate(age2 = age/100) %>% 
  mutate(income = recode(faminc, "31:99 = NA")) %>% 
  mutate(income = income/16) %>% 
  mutate(attend = recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else = NA")) %>% 
  mutate(attend = attend/6) %>% 
  mutate(male = recode(gender, "1=1; 2=0; else = NA")) %>% 
  mutate(clinton = recode(CC16_410a, "1=0; 2=1; 3:8=0; else = NA")) %>% 
  mutate(repid = recode(pid7, "8:99 = NA")) %>% 
  mutate(repid = repid/7) %>% 
  mutate(ed = educ/6) %>% 
  mutate(gayimp = recode(CC16_301n, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(gayimp = gayimp/5) %>% 
  mutate(relimp = recode(pew_religimp, "1=4; 2=3; 3=2; 4=1; else = NA")) %>% 
  mutate(relimp = relimp/4) %>% 
  mutate(white = recode(race, "1=1; else=0"))
           

evanlgbt <- cces16 %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  filter(evangelical ==1) %>% 
  select(age2, income, ed, male, white, clinton, attend, relimp, gayimp)

evan <- cces16 %>% 
    filter(evangelical ==1) %>% 
    select(age2, income, ed, male, white, clinton, attend, relimp, gayimp)

lgbt <- cces16 %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  select(age2, income, ed, male, white, clinton, attend, relimp, gayimp)

reg1 <- glm(clinton ~ ., data = evanlgbt)
reg1 <- tidy(reg1) %>% mutate(model = c("LGBT + Evangelical"))  

reg2 <- glm(clinton ~ ., data = evan)
reg2 <- tidy(reg2) %>% mutate(model = c("Evangelical"))  

reg3 <- glm(clinton ~ ., data = lgbt)
reg3 <- tidy(reg3) %>% mutate(model = c("LGBT"))  

plot <- bind_rows(reg1, reg2, reg3)



theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "azure3", size = .25, linetype = "dashed"), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       panel.grid.major.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"), 
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 40, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =18, angle = 45, hjust  =1),
       legend.justification=c(0, 0), 
       legend.position=c(.75, .85),
       legend.background = element_rect(colour="grey80"), 
       legend.title = element_blank(), 
       legend.text=element_text(size=18)
       
)
  
}


dwplot(plot) %>% 
  relabel_predictors(c(gayimp = "Gay Marriage Importance",
                       age2 = "Age",
                       ed = "Education",
                       income = "Income",
                       male = "Male",
                       white = "White",
                       attend = "Church Attendance",
                       relimp = "Religion Importance")) + 
  geom_vline(xintercept = 0, colour = "black", linetype = "solid") +
  labs(x= "Coefficient Estimate", y = "Variables", title = "Which Factors Predict a Democrat Vote in 2016?") +
  theme_rb() + guides(colour = guide_legend(reverse=T)) + 
  scale_color_grey() 


  ggsave(file="D://cces/gay_evangelical/regression_full.png", type = "cairo-png", width = 20, height =12)


  
  reg1 <- glm(clinton ~ ., data = evanlgbt)
  reg2 <- glm(clinton ~ ., data = evan)
  reg3 <- glm(clinton ~ ., data = lgbt)
    
library(stargazer)    
stargazer(reg1, reg2, reg3, type = "text", title = "Regression Model for LGBT + Evangelical", dep.var.labels = c("Predicting a Vote for Hillary Clinton"),
            covariate.labels = c("Age", "Income", "Education", "Male", "White", "Church Attendance","Religion Importance", "Gay Marriage Importance"), column.labels = c("LGBT + Evangelical", "Evangelical", "LGBT"),
            star.cutoffs = c(0.05), out = "regression1.htm")

