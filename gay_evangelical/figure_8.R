### Gay Marriage Importance


cces16 <- cces16 %>% 
  mutate(gayimp = recode(CC16_301n, "1:2='High Importance'; 3:5='Low Importance'; else=0", as.factor = TRUE)) 

relvote <- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5) %>% 
  filter(complete.cases(vote16)) %>% 
  filter(gayimp != 0) %>% 
  group_by(gayimp) %>%  
  count(vote16, wt = commonweight_vv_lgbt) %>% 
  mutate(pct = prop.table(n))

cs <- cces16 %>% filter(evangelical ==1) %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5) %>% 
  as_survey_design(weights = commonweight_vv_lgbt)


h1 <- cs %>%
  filter(gayimp == "High Importance") %>% 
  summarize(prop  = survey_mean(vote16, na.rm = TRUE, vartype = "ci")) %>% 
  mutate(imp = c("High Importance")) %>% 
  filter(prop > .05) %>% 
  add_column(candidate = c("Trump", "Clinton"))

h2 <- cs %>%
  filter(gayimp == "Low Importance") %>% 
  summarize(prop  = survey_mean(vote16, na.rm = TRUE, vartype = "ci")) %>% 
  mutate(imp = c("Low Importance")) %>%
  filter(prop > .05) %>% 
  add_column(candidate = c("Trump", "Clinton"))

graph <- bind_rows(h1, h2) %>% filter(candidate == "Trump" |  candidate == "Clinton")


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
       axis.text.x = element_text(family = "IBM Plex Serif", size =18)
)
  
}

Palette <- c("gray87", "black", "gray87")


ggplot(graph, aes(x=imp, y=prop, fill = candidate)) + geom_col(position = "dodge", color = "black")+ 
  geom_errorbar(aes(ymin = prop_low, ymax=prop_upp), width = .25, position=position_dodge(.9), color = "azure4") +
  scale_fill_manual(values = Palette) + 
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse = FALSE)) + labs(x = "How Important is Gay Marriage?", y= "Vote Share", fill="", caption = "Data: CCES 2016", title = "Gay Marriage Importance and Vote Choice", subtitle = "Among LGBT Evangelicals") + theme_rb()

ggsave(file="D://cces/gay_evangelical/gay_imp_vote16.png", type = "cairo-png", width = 20, height =12)