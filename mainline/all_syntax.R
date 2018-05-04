library(fst)
library(tidyverse)
library(car)
library(extrafont)

gss <- read.fst("C://gss.fst")

bib <- gss %>% 
  filter(bible != 4) %>% 
  filter(bible != "NA") %>% 
  group_by(year, reltrad) %>% 
  count(bible) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(bible ==1) %>% 
  select(year, pct) %>% 
  ungroup(year, reltrad) %>% 
  filter(reltrad ==1 | reltrad == 2) %>%
  mutate(reltrad = recode(reltrad, "1 = 'Evangelical'; 2 = 'Mainline'"))

bib %>% 
  ggplot(., aes(x=year, y=pct, color = reltrad, label = reltrad, group =reltrad)) + 
  geom_line(size=2) + long_rb() +
  scale_y_continuous(labels = scales::percent) +
  geom_point(colour = "black", size =2, shape =21, stroke =1.5, show.legend = F) +
  labs(x= "Year", y = "Percent of the Population", title = "Biblical Literalism", caption = "Data: GSS (1972-2016)") +
  scale_color_brewer(palette = "Set1") + theme(legend.text=element_text(size=36)) + theme(plot.title = element_text(size=64)) + 
  annotate("text", x = 2010, y = .32, label = "Mainline Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2010.15, y = .625, label = "Evangelical Protestant", size = 10, family = "Product Sans") +
  theme(legend.position="none")

ggsave(file="mainline_literal_long.png", type = "cairo-png", width = 18, height = 10)


pid <- gss %>% 
  filter(reltrad ==1 | reltrad ==2) %>% 
  filter(partyid != "NA") %>% 
  group_by(year, reltrad) %>% 
  summarise(mean = mean(partyid)) %>% 
  mutate(reltrad = as.factor(reltrad))


pid %>% 
  ggplot(., aes(x=year, y=mean, color = reltrad, label = reltrad, group =reltrad)) + 
  geom_line(size=2) + long_rb() +
  geom_point(colour = "black", size =2, shape =21, stroke =1.5, show.legend = F) +
  labs(x= "Year", y = "Mean Party ID", title = "Party Identification", caption = "Data: GSS (1972-2016)") +
  scale_color_brewer(palette = "Set1") + theme(legend.text=element_text(size=36)) + theme(plot.title = element_text(size=64)) + 
  # annotate("text", x = 2010, y = .32, label = "Mainline Protestant", size = 10, family = "Product Sans") +
  # annotate("text", x = 2010.15, y = .625, label = "Evangelical Protestant", size = 10, family = "Product Sans") +
  theme(legend.position="none") + 
  scale_y_continuous(limits = c(2,4), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Democrat", "Independent", "Ind., Near Republican", "Not Strong Republican", "Strong Republican")) +
  annotate("text", x = 2010, y = 3.05, label = "Mainline Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2010.15, y = 3.6, label = "Evangelical Protestant", size = 10, family = "Product Sans") 
  
ggsave(file="mainline_pid_long.png", type = "cairo-png", width = 18, height = 10)

ge <- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(CC16_335 ==1 | CC16_335 ==2) %>% 
  count(CC16_335) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Evangelical")) %>% 
  mutate(issue = c("Gay Marriage")) %>% 
  rename(favor  = CC16_335)

gm <- cces16 %>% 
  filter(mainline ==1) %>% 
  filter(CC16_335 ==1 | CC16_335 ==2) %>% 
  count(CC16_335) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Mainline")) %>% 
  mutate(issue = c("Gay Marriage")) %>% 
  rename(favor  = CC16_335)

ae <- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(CC16_332a ==1 | CC16_332a ==2) %>% 
  count(CC16_332a) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Evangelical")) %>% 
  mutate(issue = c("Abortion")) %>% 
  rename(favor  = CC16_332a)

am <- cces16 %>% 
  filter(mainline ==1) %>% 
  filter(CC16_332a ==1 | CC16_332a ==2) %>% 
  count(CC16_332a) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(group = c("Mainline")) %>% 
  mutate(issue = c("Abortion")) %>% 
  rename(favor  = CC16_332a)
  
bar <- bind_rows(ge, gm, ae, am) %>% 
  filter(favor ==1) %>% 
  select(group, issue, pct) %>%
  mutate(pct = round(pct,3))


bar %>% 
  ggplot(., aes(x=group, y = pct, fill = group)) + geom_col(color = "black") + bar_rb() +
  theme(axis.text.x = element_text(family = "Product Sans", size =24)) +
  scale_y_continuous(labels = scales::percent) +  
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "", y = "Percent Who Are in Favor", title = "Social Issues") +
  theme(plot.title = element_text(size=42)) +
  scale_fill_brewer(palette = "Set1") + theme(legend.position="none") + 
  facet_grid(.~ issue)


ggsave(file="mainline_ssm_ab.png", type = "cairo-png", width = 18, height = 10)







