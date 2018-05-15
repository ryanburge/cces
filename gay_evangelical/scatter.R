
cces16 <- cces16 %>% 
  mutate(imp_gay = recode(CC16_301n, "1=5; 2=4; 3=3; 4=2; 5=1; else=99")) %>% 
  mutate(imp_rel = recode(pew_religimp, "1=4; 2=3; 3=2; 4=1; else=99")) %>% 
  mutate(imp_gay = as.numeric(imp_gay), imp_rel = as.numeric(imp_rel))



theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       # panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       panel.grid.major.x =  element_line(colour = "gray48", size = .25),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 36, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =18),
       axis.text.y = element_text(family = "IBM Plex Serif", size =18)
)
  
}

Palette <- c("gray42", "gray42", "gray42")


cces16 %>% 
  filter(imp_gay < 99) %>% 
  filter(imp_rel < 99) %>% 
  filter(new != "NA") %>% 
  ggplot(., aes(x=imp_gay, y=imp_rel, fill = as.factor(new))) + 
  # geom_point(inherit.aes = TRUE) + 
  # geom_jitter(height = .25) + 
  geom_smooth(method = lm, inherit.aes = TRUE) +
  scale_fill_manual(values = Palette) + 
  theme_rb() +
  annotate("text", label = "Evangelical", x = 3.5, y = 3.63, color = "black", size =8, family = "IBM Plex Serif") +
  annotate("text", label = "LGB Evangelical", x = 3.5, y = 3.35, color = "black", size =8, family = "IBM Plex Serif") +
  annotate("text", label = "LGB", x = 3.5, y = 2.35, color = "black", size =8, family = "IBM Plex Serif") +
  labs(x= "Importance of Gay Marriage", y = "Importance of Religion", title = "Relationship between Gay Marriage and Religion Importance", caption = "Data: CCES 2016")  + 
  theme(legend.position="none") +
  scale_x_continuous(limits = c(1,5), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_y_continuous(limits = c(1,4), breaks = c(1, 2, 3, 4), labels= c("Not at All", "Not too Important", "Somewhat Important", "Very Important"))

ggsave(file="D://cces/gay_evangelical/scatter11.png", type = "cairo-png", width = 20, height =12)

  
