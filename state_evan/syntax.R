## Package Loading and Data Loading ####
 
library(haven)
library(tidyverse)
library(labelled)
library(geofacet)
library(ggridges)
library(fiftystater)
library(sf)
library(extrafont)
library(patchwork)


cces16 <- read_dta("D://cces/data/cces16.dta")

## Making Reltrad ####

source("D://measuring_evangelicals/reltrad16.R")

cces16 <- cces16 %>%  mutate(reltrad = recode(reltrad, "'evangelical'= 'Evangelical';
                          'mainline'= 'Mainline';
                          'catholic'= 'Catholic';
                          'jewish'= 'Jewish';
                          'bprot' = 'Black Protestant';
                          'other' = 'Other Faith';
                          'none' = 'No Faith'")) 

cces16$reltrad <- cces16$reltrad %>% fct_relevel("Evangelical", "Mainline", "Catholic", "No Faith", "Jewish", "Other Faith", "Black Protestant") 


## Making the geofacet ridge plot ####
cces16 %>% 
  filter(pid7 < 8) %>% 
  filter(reltrad != "NA") %>% 
  mutate(state = to_factor(inputstate)) %>%
  ggplot(., aes(x= pid7, y = fct_rev(reltrad), fill = reltrad, color = reltrad)) +
  geom_density_ridges(alpha = .8) +
  facet_geo(~state, grid = "us_state_grid4") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(plot.title = element_text(face = "bold", size = 32, family = "Product Sans"),
        text = element_text(family = "Product Sans"),
        legend.position = "bottom") + 
  theme(axis.text.x = element_text(family = "Product Sans", angle = 45, hjust = 1)) +
  theme(legend.title=element_blank()) + 
  theme(plot.caption = element_text(family = "Product Sans", size =16)) +
  scale_x_continuous(limits = c(-.5,9), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Dem", "", "", "Independent", "", "", "Strong Rep")) +
  labs(x = "Party Identification", y = "Religious Tradition", title = "States' Party Identification by Religious Tradition", caption = "Data: CCES 2016")

ggsave("state_evan/geofacet_pid7_new.png", height = 12, width = 17)

## Making the 50 state Trump vote map ####

map50 <- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(CC16_410a <3) %>% 
  mutate(vote16 = CC16_410a) %>% 
  mutate(state = to_factor(inputstate)) %>% 
  group_by(state) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(vote16 ==1) %>% 
  ungroup(state) %>% 
  mutate(state = str_to_lower(state))

map50 <- map50 %>% filter(state != "district of columbia")
map50 <- map50 %>% filter(state != "vermont")

map50 <- map50 %>%
  mutate(id = state) %>% 
  filter(n > 10)

sf_fifty <- sf::st_as_sf(fifty_states, coords = c("long", "lat")) %>% 
  group_by(id, piece) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

ggplot(map50, aes(map_id = id)) + 
  geom_map(aes(fill = pct), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())  + 
  scale_fill_continuous(low = "dodgerblue1", high = "firebrick1", labels = percent) +
  geom_sf(data = sf_fifty, col = "black", alpha = 0, size = 1) +
  coord_sf(datum=NA)  + theme(legend.key.width=unit(2,"cm")) +
  labs(title = "The Evangelical Vote for Trump", subtitle = "                                                                                                                   Percent of Evangelicals Who Voted for Trump in 2016", caption = "Data: CCES 2016", fill = "") +
  theme(legend.text =  element_text(family = "Product Sans", size =16)) +
  theme(plot.title = element_text(family = "Product Sans", size =32, face = "bold", vjust = 2))  +
  theme(plot.caption = element_text(family = "Product Sans", size =16))  + 
  theme(plot.subtitle = element_text(family = "Product Sans", size =16, vjust = -3.5)) +
  theme(legend.title = element_text(family = "Product Sans", size =16, vjust = 2)) +
  theme(legend.position = "top")


ggsave("state_evan/fiftystate_trump_evan.png", height = 17, width = 17)


### Individual States ####

my.cols <- brewer.pal(7, "RdBu")

my.cols[4] <- "#777676"

ca <- cces16 %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(state == "California") %>% 
  filter(evangelical ==1) %>% 
  filter(pid7 <8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  count(pid7, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(state = "California")

ca <- ca %>% mutate(pct = round(pct,3))

ca1 <- ca %>% 
  ggplot(., aes(x= pid7, y = pct, fill = pid7)) + geom_col(color = "black") + 
  scale_fill_manual(values = rev(my.cols)) + bar_rb() + theme(legend.position="none")  +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  scale_y_continuous(labels = scales::percent) +  
  labs(x= "", y = "", title = "California")


ny <- cces16 %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(state == "New York") %>% 
  filter(evangelical ==1) %>% 
  filter(pid7 <8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  count(pid7, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(state = "New York")


ny <- ny %>% mutate(pct = round(pct,3))

ny1 <- ny %>% 
  ggplot(., aes(x= pid7, y = pct, fill = pid7)) + geom_col(color = "black") + 
  scale_fill_manual(values = rev(my.cols)) + bar_rb() + theme(legend.position="none")  +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  scale_y_continuous(labels = scales::percent) +  
  labs(x= "", y = "", title = "New York")


oh <- cces16 %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(state == "Ohio") %>% 
  filter(evangelical ==1) %>% 
  filter(pid7 <8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  count(pid7, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(state = "Ohio")


oh <- oh %>% mutate(pct = round(pct,3))

oh1 <- oh %>% 
  ggplot(., aes(x= pid7, y = pct, fill = pid7)) + geom_col(color = "black") + 
  scale_fill_manual(values = rev(my.cols)) + bar_rb() + theme(legend.position="none")  +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  scale_y_continuous(labels = scales::percent) +  
  labs(x= "", y = "", title = "Ohio")


sc <- cces16 %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(state == "South Carolina") %>% 
  filter(evangelical ==1) %>% 
  filter(pid7 <8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  count(pid7, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(state = "South Carolina")

sc <- sc %>% mutate(pct = round(pct,3))


sc1 <- sc %>% 
  ggplot(., aes(x= pid7, y = pct, fill = pid7)) + geom_col(color = "black") + 
  scale_fill_manual(values = rev(my.cols)) + bar_rb() + theme(legend.position="none")  +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  scale_y_continuous(labels = scales::percent) +  
  labs(x= "", y = "", title = "South Carolina", caption = "Data: CCES 2016")


patch <- ca1+ny1+oh1+sc1


cces16 %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(state == "Arizona") %>% 
  filter(evangelical ==1) %>% 
  filter(pid7 <8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  count(pid7, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(state = "Arizona")

cces16 %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(state == "New Mexico") %>% 
  filter(evangelical ==1) %>% 
  filter(pid7 <8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  count(pid7, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(state = "New Mexico")

cces16 %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(state == "Alaska") %>% 
  filter(evangelical ==1) %>% 
  filter(pid7 <8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  count(pid7, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(state = "Alaska")

cces16 %>% 
  mutate(state = to_factor(inputstate)) %>% 
  filter(state == "District of Columbia") %>% 
  filter(evangelical ==1) %>% 
  filter(pid7 <8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  count(pid7, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(state = "DC")

ggsave(file="D://cces/state_evan/patchwork_4states.png", type = "cairo-png", width = 21, height = 15, patch)

