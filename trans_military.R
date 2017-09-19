library(tidyverse)
library(car)

active <- cces16 %>% group_by(trans) %>% 
  filter(complete.cases(milstat_1) & complete.cases(trans)) %>% 
  count(milstat_1, wt = commonweight) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(milstat_1 ==1) %>% 
  ungroup(trans) %>% 
  mutate(trans = as.factor(trans), milstat_1 = as.factor(milstat_1)) %>% 
  mutate(trans = recode(trans, "0= 'Do not Know';
                                        1= 'Transgender';
                                        2= 'Cisgender';
                                        3= 'Prefer Not to Say';
                                        4= 'Monthly';
                                        5= 'Weekly';
                                        6= 'More than Weekly'")) %>% 
  mutate(type = recode(milstat_1, "1= 'Active Duty'")) %>% 
  select(trans, pct, type) 
  

imfam <- cces16 %>% group_by(trans) %>% 
  filter(complete.cases(milstat_2) & complete.cases(trans)) %>% 
  count(milstat_2, wt = commonweight) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(milstat_2 ==1) %>% 
  ungroup(trans) %>% 
  mutate(trans = as.factor(trans), milstat_2 = as.factor(milstat_2)) %>% 
  mutate(trans = recode(trans, "0= 'Do not Know';
                                        1= 'Transgender';
                                        2= 'Cisgender';
                                        3= 'Prefer Not to Say';
                                        4= 'Monthly';
                                        5= 'Weekly';
                                        6= 'More than Weekly'")) %>% 
  mutate(type = recode(milstat_2, "1= 'Have Immediate Family'")) %>% 
  select(trans, pct, type)

vet <- cces16 %>% group_by(trans) %>% 
  filter(complete.cases(milstat_3) & complete.cases(trans)) %>% 
  count(milstat_3, wt = commonweight) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(milstat_3 ==1) %>% 
  ungroup(trans) %>% 
  mutate(trans = as.factor(trans), milstat_3 = as.factor(milstat_3)) %>% 
  mutate(trans = recode(trans, "0= 'Do not Know';
                                        1= 'Transgender';
                                        2= 'Cisgender';
                                        3= 'Prefer Not to Say';
                                        4= 'Monthly';
                                        5= 'Weekly';
                                        6= 'More than Weekly'")) %>% 
  mutate(type = recode(milstat_3, "1= 'Military Veteran'")) %>% 
  select(trans, pct, type)

vetfam <- cces16 %>% group_by(trans) %>% 
  filter(complete.cases(milstat_4) & complete.cases(trans)) %>% 
  count(milstat_4, wt = commonweight) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(milstat_4 ==1) %>% 
  ungroup(trans) %>% 
  mutate(trans = as.factor(trans), milstat_4 = as.factor(milstat_4)) %>% 
  mutate(trans = recode(trans, "0= 'Do not Know';
                                        1= 'Transgender';
                                        2= 'Cisgender';
                                        3= 'Prefer Not to Say';
                                        4= 'Monthly';
                                        5= 'Weekly';
                                        6= 'More than Weekly'")) %>% 
  mutate(type = recode(milstat_4, "1= 'Veteran in Family'")) %>% 
  select(trans, pct, type)

no <- cces16 %>% group_by(trans) %>% 
  filter(complete.cases(milstat_5) & complete.cases(trans)) %>% 
  count(milstat_5, wt = commonweight) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(milstat_5 ==1) %>% 
  ungroup(trans) %>% 
  mutate(trans = as.factor(trans), milstat_5 = as.factor(milstat_5)) %>% 
  mutate(trans = recode(trans, "0= 'Do not Know';
                                        1= 'Transgender';
                                        2= 'Cisgender';
                                        3= 'Prefer Not to Say';
                                        4= 'Monthly';
                                        5= 'Weekly';
                                        6= 'More than Weekly'")) %>% 
  mutate(type = recode(milstat_5, "1= 'No Military Service'")) %>% 
  select(trans, pct, type)

total <- bind_rows(active, imfam, vet, vetfam, no)

total$type <- factor(total$type, levels=unique(total$type))


ggplot(total, aes(x=type, y= pct, fill = fct_reorder(trans, -pct))) + geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent)  +  
  theme(axis.ticks = element_blank()) + ylab("Percent of Sample") + 
  theme(legend.position="bottom") +
  ggtitle("Transgender and Military Service") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("chartreuse4","darkorange1", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Type of Military Service") +
  labs(caption = "Data from CCES 2016")

ggsave(file="trans_military.png", type = "cairo-png", width = 15, height = 10)



