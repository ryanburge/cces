library(haven)
library(tidyverse)
library(janitor)
library(scales)

cces08 <- read_dta("D://cces/data/cces2008.dta")
cces10 <- read_dta("D://cces/data/cces10.dta")
cces12 <- read_dta("D://cces/data/cces12.dta")
cces14 <- read_dta("D://cces/data/cces14.dta")
cces16 <- read_dta("D://cces/data/cces16.dta")



nd16 <- cces16 %>% mutate(relig = as_factor(religpew_protestant)) %>% 
  filter(relig != "Skipped") %>%
  count(relig, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% mutate(pct = round(pct,2)) %>% 
  mutate(year = c(2016))

nd14 <- cces14 %>% mutate(relig = as_factor(religpew_protestant)) %>% 
  filter(relig != "Skipped") %>%
  count(relig, wt = weight) %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% mutate(pct = round(pct,2)) %>% 
  mutate(year = c(2014))

nd12 <- cces12 %>% mutate(relig = as_factor(religpew_protestant)) %>% 
  filter(relig != "Skipped") %>%
  count(relig, wt = weight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% mutate(pct = round(pct,2)) %>% 
  mutate(year = c(2012))

nd10 <- cces10 %>% mutate(relig = as_factor(V220)) %>% 
  filter(relig != "Skipped") %>%
  count(relig, wt = V101) %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% mutate(pct = round(pct,2)) %>% 
  mutate(year = c(2010))

nd08 <- cces08 %>% mutate(relig = as_factor(V220)) %>% 
  filter(relig != "Skipped") %>%
  count(relig, wt = V201) %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% mutate(pct = round(pct,2)) %>% 
  mutate(year = c(2008))


total <- bind_rows(nd08, nd10, nd12, nd14, nd16)

total %>% filter(pct > .10) %>% 
ggplot(., aes(x=year, y=pct, group=relig, label = relig, color = relig)) + 
  geom_line(size = 2) +  
  scale_y_continuous(labels = scales::percent) +
  labs(x="Year", y = "Percent of Protestant Respondents", title = "Shifts in the Largest Protestant Traditions", caption = "Data: CCES 2008-2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + 
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) 


ggsave(file="protestant_shifts.png", type = "cairo-png", width = 15, height = 12)



## Plotting Individual Years

cces16 %>% mutate(relig = as_factor(religpew_protestant)) %>% 
  filter(relig != "Skipped") %>%
  count(relig, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% mutate(pct = round(pct,2)) %>% 
  ggplot(., aes(x=reorder(relig, pct), y=pct)) + 
  geom_col(fill = "cornflowerblue", color = "black") + 
  coord_flip() +  
  scale_y_continuous(labels = scales::percent) +
  labs(x="Protestant Denomination", y = "Percent of Protestant Respondents", title = "The Landscape of Protestant Christianity", caption = "Data: CCES 2016")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))
  
ggsave(file="protestant_breakdown_2016.png", type = "cairo-png", width = 15, height = 12)

cces14 %>% mutate(relig = as_factor(religpew_protestant)) %>% 
  filter(relig != "Skipped") %>%
  count(relig, wt = weight) %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% mutate(pct = round(pct,2)) %>% 
  ggplot(., aes(x=reorder(relig, pct), y=pct)) + 
  geom_col(fill = "cornflowerblue", color = "black") + 
  coord_flip() +  
  scale_y_continuous(labels = scales::percent) +
  labs(x="Protestant Denomination", y = "Percent of Protestant Respondents", title = "The Landscape of Protestant Christianity", caption = "Data: CCES 2014")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))