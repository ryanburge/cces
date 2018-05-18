## Vote Choice ####

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
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5) %>% 
  count(vote16, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGB + Evangelical")) %>% 
  select(-n)

lgbt <- cces16 %>% 
  filter(complete.cases(vote16)) %>%
  # filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5) %>% 
  count(vote16, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGB")) %>% 
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

vote16 <- add_row(vote16, vote16 = "All Others", pct = .039, type = "LGB + Evangelical") 
vote16 <- add_row(vote16, vote16 = "All Others", pct = .062, type = "LGB") 
vote16 <- add_row(vote16, vote16 = "All Others", pct = .050, type = "Evangelical")


Palette <- c("gray48", "black", "gray87")
vote16 <- vote16 %>% mutate(pct = round(pct,3))


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
  geom_text(aes(y = pct + .020, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 7, family = "IBM Plex Serif") +
  labs(x= "Vote Choice for President in 2016", y = "Percent of Each Sample", title = "Vote Choice in 2016", caption = "Data: CCES 2016", subtitle = "")+ theme_rb()

ggsave(file="D://cces/gay_evangelical/vote16_2016_new.png", type = "cairo-png", width = 20, height =12)
