cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(immstat == 1 | immstat == 2) %>% 
  ct(race) 



parts <- c(`White`= 39 , `Black`= 28, `Hispanic`= 21, `Asian` = 10, `Native American` = 1, `All Others` = 1)

ev <- waffle(parts, legend_pos = "bottom", rows = 5) + 
  scale_fill_manual(values = c('White' = "#E41A1C", 'Black' = "#377EB8", 'Hispanic' = "#4DAF4A", 'Asian' = "#F781BF", 'Native American' = "#984EA3", 'Mixed' = "#A65628", 'Middle Eastern' = "#FFFF33", 'All Others' = "azure3")) +
  theme(text=element_text(size=104, family="font")) + theme(legend.title=element_blank()) +
  labs(title = "Racial Composition of Immigrants", subtitle = "Evangelical", caption = "Data: CCES 2016") + 
  theme(plot.title = element_text(family = "font", size = 234, vjust =2, face = "bold")) +
  theme(plot.margin=unit(c(2,1,2,1),"cm"))

ggsave(file="D://cces/immigrants/waffles_evan.png", type = "cairo-png",  width = 18, height = 12,  ev) 

cces16 %>% 
  filter(catholic ==1) %>% 
  filter(immstat == 1 | immstat == 2) %>% 
  ct(race) 

parts <- c(`White`= 33 , `Black`= 8, `Hispanic`= 42, `Asian` = 12, `Native American` = 0, `Mixed` = 2, `All Others` = 3)

cath <- waffle(parts, legend_pos = "bottom", rows = 5) + 
  scale_fill_manual(values = c('White' = "#E41A1C", 'Black' = "#377EB8", 'Hispanic' = "#4DAF4A", 'Asian' = "#F781BF", 'Native American' = "#984EA3", 'Mixed' = "#A65628", 'Middle Eastern' = "#FFFF33", 'All Others' = "azure3")) +
  theme(text=element_text(size=104, family="font")) + theme(legend.title=element_blank()) +
  labs(title = "Racial Composition of Immigrants", subtitle = "Catholic", caption = "Data: CCES 2016") + 
  theme(plot.title = element_text(family = "font", size = 234, vjust =2, face = "bold")) +
  theme(plot.margin=unit(c(2,1,2,1),"cm"))

ggsave(file="D://cces/immigrants/waffles_cath.png", type = "cairo-png", width = 18, height = 12, cath) 


cces16 %>% 
  filter(other ==1) %>% 
  filter(immstat == 1 | immstat == 2) %>% 
  ct(race) 

parts <- c(`White`= 22 , `Black`= 11, `Hispanic`= 10, `Asian` = 49, `Native American` = 0, `Mixed` = 2, `Middle Eastern` = 4, `All Others` = 2)

font_add_google("Oswald", "font")
showtext_auto()

other <- waffle(parts, legend_pos = "bottom", rows = 5) + 
  scale_fill_manual(values = c('White' = "#E41A1C", 'Black' = "#377EB8", 'Hispanic' = "#4DAF4A", 'Asian' = "#F781BF", 'Native American' = "#984EA3", 'Mixed' = "#A65628", 'Middle Eastern' = "#FFFF33", 'All Others' = "azure3")) +
  theme(text=element_text(size=104, family="font")) + theme(legend.title=element_blank()) +
  labs(title = "Racial Composition of Immigrants", subtitle = "Other Faith", caption = "Data: CCES 2016") + 
  theme(plot.title = element_text(family = "font", size = 234, vjust =2, face = "bold")) +
  theme(plot.margin=unit(c(2,1,2,1),"cm"))
 
ggsave(file="D://cces/immigrants/waffles_other.png", type = "cairo-png", width = 18, height = 12, other) 


cces16 %>% 
  filter(none ==1) %>% 
  filter(immstat == 1 | immstat == 2) %>% 
  ct(race) 

parts <- c(`White`= 35 , `Black`= 13, `Hispanic`= 20, `Asian` = 27, `Native American` = 0, `Mixed` = 2, `Middle Eastern` = 2, `All Others` = 1)

font_add_google("Oswald", "font")
showtext_auto()

none <- waffle(parts, legend_pos = "bottom", rows = 5) + 
  scale_fill_manual(values = c('White' = "#E41A1C", 'Black' = "#377EB8", 'Hispanic' = "#4DAF4A", 'Asian' = "#F781BF", 'Native American' = "#984EA3", 'Mixed' = "#A65628", 'Middle Eastern' = "#FFFF33", 'All Others' = "azure3")) +
  theme(text=element_text(size=104, family="font")) + theme(legend.title=element_blank()) +
  labs(title = "Racial Composition of Immigrants", subtitle = "No Faith", caption = "Data: CCES 2016") + 
  theme(plot.title = element_text(family = "font", size = 234, vjust =2, face = "bold")) +
  theme(plot.margin=unit(c(2,1,2,1),"cm"))

ggsave(file="D://cces/immigrants/waffles_other.png", type = "cairo-png", width = 18, height = 12, none) 



