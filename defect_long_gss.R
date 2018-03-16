test <- gss %>% 
  select(id, year, relig16, relig, denom16, denom) %>% 
  na.omit()

test$newid <- paste(test$id, test$year, sep = "_")

count <- test %>% group_by(relig16) %>% 
  count(relig) %>% mutate(pct =  prop.table(n)) %>% ungroup(relig16)

test <- test %>% 
  mutate(religdiff = relig16 - relig, denomdiff = denom16 - denom)

test$class <- ifelse((test$denomdiff) == 0, "Stayed", "Defected")
test$rdiff <- ifelse((test$religdiff) == 0, "Stayed", "Defected")


test %>% 
  group_by(year) %>% 
  count(class) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(class == "Defected")

sbc <- test %>% 
  filter(denom16 == 14) %>% 
  group_by(year) %>% 
  count(class) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(class == "Defected")  %>% 
  mutate(group = c("Southern Baptist"))

umc <- test %>% 
  filter(denom16 == 22) %>% 
  group_by(year) %>% 
  count(class) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(class == "Defected") %>% 
  mutate(group = c("United Methodist"))

bdk <- test %>% 
  filter(denom16 == 18) %>% 
  group_by(year) %>% 
  count(class) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(class == "Defected") %>% 
  mutate(group = c("Baptist, Don't Know")) 

# mdk <- test %>% 
#   filter(denom16 == 28) %>% 
#   group_by(year) %>% 
#   count(class) %>% 
#   mutate(pct = prop.table(n)) %>% 
#   filter(class == "Defected") %>% 
#   mutate(group = c("Methodist, Don't Know")) 

other <- test %>% 
  filter(denom16 == 60) %>% 
  group_by(year) %>% 
  count(class) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(class == "Defected") %>% 
  mutate(group = c("Other Protestant")) 

cath <- gss %>% 
  filter(relig16 ==2) %>% 
  group_by(year) %>% 
  count(relig) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(relig ==2) %>% 
  mutate(defect = 1- pct) %>% 
  select(-pct) %>% 
  rename(pct = defect) %>% 
  select(year, pct) %>% 
  mutate(group = c("Catholic"))

jew <- gss %>% 
  filter(relig16 ==3) %>% 
  group_by(year) %>% 
  count(relig) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(relig ==3) %>% 
  mutate(defect = 1- pct) %>% 
  select(-pct) %>% 
  rename(pct = defect) %>% 
  select(year, pct) %>% 
  mutate(group = c("Jewish"))

none <- gss %>% 
  filter(relig16 ==4) %>% 
  group_by(year) %>% 
  count(relig) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(relig ==4) %>% 
  mutate(defect = 1- pct) %>% 
  select(-pct) %>% 
  rename(pct = defect) %>% 
  select(year, pct) %>% 
  mutate(group = c("Nones"))






long <- bind_rows(sbc, umc, bdk, other) %>% select(year, pct, group)
long <- bind_rows(long, cath, jew, none)

my.cols <- brewer.pal(7, "Set1")
my.cols[1] <- "black"


long %>% 
  ggplot(., aes(x=year, y=pct, color = group, label = group, group =group)) + 
  geom_smooth(size =2, se = FALSE) + long_rb() +
  scale_y_continuous(labels = scales::percent) +
  # geom_point(aes(colour = group), size =2, shape =21, stroke =1.5, show.legend = F) +
  labs(x= "Year", y = "Percent of Tradition that Defected", title = "The Defection Rate of Major Religious Traditions", subtitle = "Comparing Religion at 16 Years Old to Adult Religion", caption = "Data: GSS (1972-2016)") +
  scale_color_manual(values = c("black" ,  "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",   "#A65628")) + theme(legend.text=element_text(size=36)) + theme(plot.title = element_text(size=64))



ggsave(file="D://cces_panel/defect_long_gss.png", type = "cairo-png", width = 21, height = 15)

none <- gss %>% 
  filter(relig16 ==4) %>% 
  group_by(year) %>% 
  count(relig) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(relig ==4) %>% 
  mutate(defect = 1- pct) %>% 
  select(-pct) %>% 
  rename(pct = defect) %>% 
  select(year, pct) %>% 
  mutate(group = c("Nones"))

none %>% 
  ggplot(., aes(x=year, y=pct, color = group, label = group, group =group)) + 
  geom_smooth(size =2) + long_rb() +
  scale_y_continuous(limits = c(.2,.8), labels = scales::percent) +
  # geom_point(aes(colour = group), size =2, shape =21, stroke =1.5, show.legend = F) +
  labs(x= "Year", y = "Percent of Tradition that Defected", title = "The Defection Rate of Major Religious Traditions", subtitle = "Comparing Religion at 16 Years Old to Adult Religion", caption = "Data: GSS (1972-2016)") +
  scale_fill_manual(values = rev(my.cols)) + theme(legend.text=element_text(size=36)) + theme(plot.title = element_text(size=64))

ggsave(file="D://cces_panel/defect_long_gss_nones.png", type = "cairo-png", width = 21, height = 15)


