lgbtevan <- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(pid7, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(pid7 !=8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(type = c("LGBT + Evangelical")) %>% 
  select(-n)
  
lgbt <- cces16 %>% 
  # filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(pid7, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(pid7 !=8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(type = c("LGBT")) %>% 
  select(-n)

evan <- cces16 %>% 
  filter(evangelical ==1) %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(pid7, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(pid7 !=8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(type = c("Evangelical")) %>% 
  select(-n)


pid7 <- bind_rows(lgbtevan,lgbt, evan)

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
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =18, angle = 45, hjust  =1)
)
  
}

Palette <- c("gray48", "black", "gray87")

pid7 %>% 
  filter(pid7 != "Skipped") %>% 
  filter(pid7 != "Not Asked") %>% 
  ggplot(.,aes(x=pid7, y=pct, fill = type)) + 
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  scale_fill_manual(values = Palette) +
  labs(x= "Party Identification", y = "Percent of Each Sample", title = "The Political Ideology of LGBT Evangelicals", caption = "Data: CCES 2016", subtitle = "Mean Party Identification: Evangelical = 4.6, LGBT = 2.8, LGBT Evangelical = 3.2")+ theme_rb()

ggsave(file="D://cces/gay_evangelical/pid7_2016.png", type = "cairo-png", width = 20, height =12)


cces16 <- cces16 %>%  mutate(vote16 = as.numeric(CC16_410a)) %>%  mutate(vote16 = recode(vote16,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA"))


lgbtevan <- cces16 %>% 
  filter(complete.cases(vote16)) %>% 
  filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(vote16, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGBT + Evangelical")) %>% 
  select(-n)

lgbt <- cces16 %>% 
  filter(complete.cases(vote16)) %>%
  # filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(vote16, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGBT")) %>% 
  select(-n)

evan <- cces16 %>% 
  filter(complete.cases(vote16)) %>%
  filter(evangelical ==1) %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(vote16, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("Evangelical")) %>% 
  select(-n)

vote16 <- bind_rows(lgbtevan,lgbt, evan) %>% 
  filter(vote16 == "Donald Trump" | vote16 == "Hillary Clinton")

vote16 <- add_row(vote16, vote16 = "All Others", pct = .0324, type = "LGBT + Evangelical") 
vote16 <- add_row(vote16, vote16 = "All Others", pct = .0876, type = "LGBT") 
vote16 <- add_row(vote16, vote16 = "All Others", pct = .0508, type = "Evangelical")


Palette <- c("gray48", "black", "gray87")


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
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =24)
)
  
}


vote16 <- vote16 %>% mutate(vote16 = fct_relevel(vote16, "Donald Trump", "Hillary Clinton", "All Others"))

vote16 %>% 
  ggplot(.,aes(x=vote16, y=pct, fill = type)) + 
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  scale_fill_manual(values = Palette) +
  labs(x= "Vote Choice for President in 2016", y = "Percent of Each Sample", title = "Vote Choice in 2016 by LGBT Evangelicals", caption = "Data: CCES 2016", subtitle = "")+ theme_rb()

ggsave(file="D://cces/gay_evangelical/vote16_2016.png", type = "cairo-png", width = 20, height =12)


lgbtevan <- cces16 %>% 
  filter(CC16_301n <8) %>% 
  filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(CC16_301n, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGBT + Evangelical")) %>% 
  mutate(gayimp = to_factor(CC16_301n)) %>% 
  select(-n, -CC16_301n)

lgbt <- cces16 %>% 
  filter(CC16_301n <8) %>% 
  # filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(CC16_301n, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGBT")) %>% 
  mutate(gayimp = to_factor(CC16_301n)) %>% 
  select(-n, -CC16_301n)

evan <- cces16 %>% 
  filter(CC16_301n <8) %>% 
  filter(evangelical ==1) %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(CC16_301n, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("Evangelical")) %>% 
  mutate(gayimp = to_factor(CC16_301n)) %>% 
  select(-n, -CC16_301n)

gayimp <- bind_rows(lgbtevan, lgbt, evan)


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

gayimp %>% 
  ggplot(.,aes(x=gayimp, y=pct, fill = type)) + 
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  scale_fill_manual(values = Palette) +
  labs(x= "Level of Importance", y = "Percent of Each Sample", title = "The Importance of Gay Marriage", caption = "Data: CCES 2016", subtitle = "")+ theme_rb()

ggsave(file="D://cces/gay_evangelical/gayimp_2016.png", type = "cairo-png", width = 20, height =12)


lgbtevan <- cces16 %>% 
  filter(pew_religimp <8) %>% 
  filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(pew_religimp, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGBT + Evangelical")) %>% 
  mutate(gayimp = to_factor(pew_religimp)) %>% 
  select(-n, -pew_religimp)

lgbt <- cces16 %>% 
  filter(pew_religimp <8) %>% 
  # filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(pew_religimp, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGBT")) %>% 
  mutate(gayimp = to_factor(pew_religimp)) %>% 
  select(-n, -pew_religimp)

evan <- cces16 %>% 
  filter(pew_religimp <8) %>% 
  filter(evangelical ==1) %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(pew_religimp, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("Evangelical")) %>% 
  mutate(gayimp = to_factor(pew_religimp)) %>% 
  select(-n, -pew_religimp)

gayimp <- bind_rows(lgbtevan, lgbt, evan)


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

gayimp %>% 
  ggplot(.,aes(x=gayimp, y=pct, fill = type)) + 
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  scale_fill_manual(values = Palette) +
  labs(x= "Level of Importance", y = "Percent of Each Sample", title = "The Importance of Religion", caption = "Data: CCES 2016", subtitle = "")+ theme_rb()

ggsave(file="D://cces/gay_evangelical/relig_imp_2016.png", type = "cairo-png", width = 20, height =12)


cces16 %>% 
  group_by(new) %>% 
  # filter(new != NA) %>% 
  summarise(mean = mean(pid7))



