library(tidyverse)
library(extrafont)
library(haven)
library(car)
library(janitor)
library(labelled)
source("D://cces/ggthemes.R")

cces16 <- read_dta("D://cces/data/cces16.dta")

## Run RELTRAD evangelical

lgbtevan <- cces16 %>% 
  filter(pew_religimp <8) %>% 
  filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5) %>% 
  count(pew_religimp, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGB + Evangelical")) %>% 
  mutate(relimp = to_factor(pew_religimp)) %>% 
  select(-n, -pew_religimp)

lgbt <- cces16 %>% 
  filter(pew_religimp <8) %>% 
  # filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5) %>% 
  count(pew_religimp, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGB")) %>% 
  mutate(relimp = to_factor(pew_religimp)) %>% 
  select(-n, -pew_religimp)

evan <- cces16 %>% 
  filter(pew_religimp <8) %>% 
  filter(evangelical ==1) %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(pew_religimp, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("Evangelical")) %>% 
  mutate(relimp = to_factor(pew_religimp)) %>% 
  select(-n, -pew_religimp)

relimp <- bind_rows(lgbtevan, lgbt, evan)

relimp <- relimp %>% mutate(pct = round(pct,3))


theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 36),
       plot.title = element_text(family = "IBM Plex Serif", size = 40, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =18, angle = 45, hjust  =1)
)
  
}

Palette <- c("gray48", "black", "gray87")

relimp %>% 
  ggplot(.,aes(x=fct_rev(relimp), y=pct, fill = type)) + 
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  scale_fill_manual(values = Palette) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 8, family = "IBM Plex Serif") +
  labs(x= "Level of Importance", y = "Percent of Each Sample", title = "The Importance of Religion", caption = "Data: CCES 2016", subtitle = "") +
  theme_rb()

ggsave(file="D://cces/gay_evangelical/relig_imp_new_2016.png", type = "cairo-png", width = 20, height =12)
