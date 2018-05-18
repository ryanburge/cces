
lgbtevan <- cces16 %>% 
  filter(CC16_301n <8) %>% 
  filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(CC16_301n, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGB + Evangelical")) %>% 
  mutate(gayimp = to_factor(CC16_301n)) %>% 
  select(-n, -CC16_301n)

lgbt <- cces16 %>% 
  filter(CC16_301n <8) %>% 
  # filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(CC16_301n, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(type = c("LGB")) %>% 
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


gayimp <- gayimp %>% mutate(pct = round(pct,3))

Palette <- c("gray48", "black", "gray87")

gayimp %>% 
  ggplot(.,aes(x=fct_rev(gayimp), y=pct, fill = type)) + 
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  scale_fill_manual(values = Palette) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 7, family = "IBM Plex Serif") +
  labs(x= "Level of Importance", y = "Percent of Each Sample", title = "The Importance of Gay Marriage", caption = "Data: CCES 2016", subtitle = "")+ theme_rb()

ggsave(file="D://cces/gay_evangelical/gayimp_2016_new.png", type = "cairo-png", width = 20, height =12)