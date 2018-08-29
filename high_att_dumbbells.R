#### Racial Groups ####

a1 <- cces16 %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Entire Sample")) %>% 
  mutate(attend = c("All Levels"))

a2 <-cces16 %>% 
  filter(CC16_410a < 3) %>% 
  mutate(pres2 = recode(CC16_410a, "1=1; else=0")) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>% 
  ungroup(pew_churatd) %>% 
  mutate(group = c("Entire Sample")) %>% 
  mutate(attend = c("High Attenders"))

a3 <-cces16 %>% 
  filter(CC16_410a < 3) %>%
  filter(race ==2) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Black")) %>% 
  mutate(attend = c("All Levels"))

a4 <-cces16 %>% 
  filter(race ==2) %>% 
  filter(CC16_410a < 3) %>% 
  mutate(pres2 = recode(CC16_410a, "1=1; else=0")) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>% 
  ungroup(pew_churatd) %>% 
  mutate(group = c("Entire Black Sample")) %>% 
  mutate(attend = c("High Attenders"))

a5 <- cces16 %>% 
  filter(race ==3) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Hispanic")) %>% 
  mutate(attend = c("All Levels"))

a6 <- cces16 %>% 
  filter(CC16_410a < 3) %>% 
  filter(race ==3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>% 
  ungroup(pew_churatd) %>% 
  mutate(group = c("Entire Hispanic Sample")) %>% 
  mutate(attend = c("High Attenders"))

a7 <- cces16 %>% 
  filter(race ==4) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Asian")) %>% 
  mutate(attend = c("All Levels"))

a8 <- cces16 %>% 
  filter(CC16_410a < 3) %>% 
  filter(race ==4) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Entire Asian Sample")) %>% 
  mutate(attend = c("High Attenders"))

a9 <- cces16 %>% 
  filter(race ==1) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("White")) %>% 
  mutate(attend = c("All Levels"))

a10 <- cces16 %>% 
  filter(CC16_410a < 3) %>% 
  filter(race ==1) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Entire White Sample")) %>% 
  mutate(attend = c("High Attenders"))


race1 <- bind_rows(a1, a3, a5, a7, a9) %>% 
  select(pct, group) %>% 
  rename(all_pct = pct)


race2 <- bind_rows(a2, a4, a6, a8, a10) %>% 
  select(pct, group) %>% 
  rename(high_pct = pct)

race <- bind_cols(race1, race2)

race$group <- factor(race$group, levels = c("Asian", "Hispanic", "Black", "White", "Entire Sample"))


race %>% 
  ggplot(., aes(x=all_pct, xend=high_pct, y=group, group=group)) + 
  geom_dumbbell(size_x = 4, size_xend = 4, size = 1, color="azure3", 
                colour_x = "darkorchid", colour_xend = "tomato",
                dot_guide=FALSE, dot_guide_size=0.05)  + mean_rb() +
  theme(plot.title = element_text(size=24)) +
  scale_x_continuous(labels = scales::percent) +
  geom_text(data=filter(race, group == "Entire Sample"), aes(x=all_pct, y=group, label="All Levels"), color="darkorchid", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  geom_text(data=filter(race, group == "Entire Sample"), aes(x=high_pct, y=group, label="Weekly+ Attenders"), color="tomato", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  labs(x = "Percent of Two Party Vote for Trump", y = "Racial Group", subtitle = "", caption = "Data: CCES 2016", title = "The Impact of Frequent Attendance on Vote Choice by Race")

ggsave(file="D://cces/high_att_dumbbell_race.png", type = "cairo-png", width = 12, height = 6)


#### Denoms ####

b1 <- cces16 %>% 
  filter(religpew ==2) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Catholics")) %>% 
  mutate(attend = c("All Levels"))

b2 <- cces16 %>% 
  filter(CC16_410a < 3) %>% 
  filter(religpew == 2) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Catholics")) %>% 
  mutate(attend = c("High Attenders"))

b3 <- cces16 %>% 
  filter(religpew_baptist ==1) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Southern Baptists")) %>% 
  mutate(attend = c("All Levels"))

b4 <- cces16 %>% 
  filter(religpew_baptist == 1) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Southern Baptists")) %>% 
  mutate(attend = c("High Attenders"))

b5 <- cces16 %>% 
  filter(religpew_protestant ==3) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Nondenominational")) %>% 
  mutate(attend = c("All Levels"))

b6 <- cces16 %>% 
  filter(religpew_protestant == 3) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Nondenominational")) %>% 
  mutate(attend = c("High Attenders"))

b7 <- cces16 %>% 
  filter(religpew_methodist ==1) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("United Methodist")) %>% 
  mutate(attend = c("All Levels"))

b8 <- cces16 %>% 
  filter(religpew_methodist ==1) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("United Methodist")) %>% 
  mutate(attend = c("High Attenders"))

b9 <- cces16 %>% 
  filter(religpew_lutheran ==1) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("ELCA")) %>% 
  mutate(attend = c("All Levels"))

b10 <- cces16 %>% 
  filter(religpew_lutheran ==1) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("ELCA")) %>% 
  mutate(attend = c("High Attenders"))

b11 <- cces16 %>% 
  filter(religpew ==3) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Mormon")) %>% 
  mutate(attend = c("All Levels"))

b12 <- cces16 %>% 
  filter(religpew ==3) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Mormon")) %>% 
  mutate(attend = c("High Attenders"))



denom1 <- bind_rows(b1, b3, b5, b7, b9, b11) %>% 
  select(pct, group) %>% 
  rename(all_pct = pct)


denom2 <- bind_rows(b2, b4, b6, b8, b10, b12) %>% 
  select(pct, group) %>% 
  rename(high_pct = pct)

denom <- bind_cols(denom1, denom2)

denom$group <- factor(denom$group, levels = c("Mormon", "ELCA", "United Methodist", "Nondenominational", "Southern Baptists", "Catholics"))

denom %>% 
  ggplot(., aes(x=all_pct, xend=high_pct, y=group, group=group)) + 
  geom_dumbbell(size_x = 4, size_xend = 4, size = 1, color="azure3", 
                colour_x = "darkorchid", colour_xend = "tomato",
                dot_guide=FALSE, dot_guide_size=0.05)  + mean_rb() +
  theme(plot.title = element_text(size=24)) +
  scale_x_continuous(limits = c(.45,.85), labels = scales::percent) +
  geom_text(data=filter(denom, group == "Catholics"), aes(x=all_pct, y=group, label="All Levels"), color="darkorchid", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  geom_text(data=filter(denom, group == "Catholics"), aes(x=high_pct, y=group, label="Weekly+ Attenders"), color="tomato", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  labs(x = "Percent of Two Party Vote for Trump", y = "Religious Group", subtitle = "", caption = "Data: CCES 2016", title = "The Impact of Frequent Attendance on Vote Choice by Denomination")+
  theme(plot.title = element_text(size =18))

ggsave(file="D://cces/high_att_dumbbell_denoms1.png", type = "cairo-png", width = 12, height = 6)

#### Education ####

c1 <- cces16 %>% 
  filter(educ ==5) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("College Degree")) %>% 
  mutate(attend = c("All Levels"))

c2 <- cces16 %>% 
  filter(educ ==5) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("College Degree")) %>% 
  mutate(attend = c("High Attenders"))

c3 <- cces16 %>% 
  filter(educ ==1 | educ ==2) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("High School or Less")) %>% 
  mutate(attend = c("All Levels"))

c4 <- cces16 %>% 
  filter(educ ==1 | educ ==2) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("High School or Less")) %>% 
  mutate(attend = c("High Attenders"))

c5 <- cces16 %>% 
  filter(educ ==6) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Post Grad")) %>% 
  mutate(attend = c("All Levels"))

c6 <- cces16 %>% 
  filter(educ ==6) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Post Grad")) %>% 
  mutate(attend = c("High Attenders"))


ed1 <- bind_rows(c1, c3, c5) %>% 
  select(pct, group) %>% 
  rename(all_pct = pct)

ed2 <- bind_rows(c2, c4, c6) %>% 
  select(pct, group) %>% 
  rename(high_pct = pct)

ed <- bind_cols(ed1, ed2)

ed$group <- factor(ed$group, levels = c("Post Grad", "College Degree", "High School or Less"))

ed %>% 
  ggplot(., aes(x=all_pct, xend=high_pct, y=group, group=group)) + 
  geom_dumbbell(size_x = 4, size_xend = 4, size = 1, color="azure3", 
                colour_x = "darkorchid", colour_xend = "tomato",
                dot_guide=FALSE, dot_guide_size=0.05)  + mean_rb() +
  theme(plot.title = element_text(size=24)) +
  scale_x_continuous(limits = c(.35,.85), labels = scales::percent) +
  geom_text(data=filter(ed, group == "High School or Less"), aes(x=all_pct, y=group, label="All Levels"), color="darkorchid", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  geom_text(data=filter(ed, group == "High School or Less"), aes(x=high_pct, y=group, label="Weekly+ Attenders"), color="tomato", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  labs(x = "Percent of Two Party Vote for Trump", y = "", subtitle = "", caption = "Data: CCES 2016", title = "The Impact of Frequent Attendance on Vote Choice by Education") +
  theme(plot.title = element_text(size =18))

ggsave(file="D://cces/high_att_dumbbell_educ1.png", type = "cairo-png", width = 12, height = 6)


#### Ideology ####

d1 <- cces16 %>% 
  filter(ideo5 == 1 | ideo5 ==2) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Liberals")) %>% 
  mutate(attend = c("All Levels"))

d2 <- cces16 %>% 
  filter(ideo5 == 1 | ideo5 ==2) %>%
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Liberals")) %>% 
  mutate(attend = c("High Attenders"))

d3 <- cces16 %>% 
  filter(ideo5 == 3) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Moderates")) %>% 
  mutate(attend = c("All Levels"))

d4 <- cces16 %>% 
  filter(ideo5 == 3) %>%
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Moderates")) %>% 
  mutate(attend = c("High Attenders"))

d5 <- cces16 %>% 
  filter(ideo5 == 4 | ideo5 ==5) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  mutate(group = c("Conservatives")) %>% 
  mutate(attend = c("All Levels"))

d6 <- cces16 %>% 
  filter(ideo5 == 4 | ideo5 ==5) %>%
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  mutate(group = c("Conservatives")) %>% 
  mutate(attend = c("High Attenders"))



ideo1 <- bind_rows(d1, d3, d5) %>% 
  select(pct, group) %>% 
  rename(all_pct = pct)


ideo2 <- bind_rows(d2, d4, d6) %>% 
  select(pct, group) %>% 
  rename(high_pct = pct)

ideo <- bind_cols(ideo1, ideo2)

ideo$group <- factor(ideo$group, levels = c("Liberals", "Moderates", "Conservatives"))

ideo %>% 
  ggplot(., aes(x=all_pct, xend=high_pct, y=group, group=group)) + 
  geom_dumbbell(size_x = 4, size_xend = 4, size = 1, color="azure3", 
                colour_x = "darkorchid", colour_xend = "tomato",
                dot_guide=FALSE, dot_guide_size=0.05)  + mean_rb() +
  theme(plot.title = element_text(size=24)) +
  scale_x_continuous(limits = c(.05,.95), labels = scales::percent) +
  geom_text(data=filter(ideo, group == "Liberals"), aes(x=all_pct, y=group, label="All Levels"), color="darkorchid", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  geom_text(data=filter(ideo, group == "Liberals"), aes(x=high_pct, y=group, label="Weekly+ Attenders"), color="tomato", size=4, vjust=-1, fontface="bold", family="Product Sans") +
  labs(x = "Percent of Two Party Vote for Trump", y = "", subtitle = "", caption = "Data: CCES 2016", title = "The Impact of Frequent Attendance on Vote Choice by Ideology") +
  theme(plot.title = element_text(size = 22))

ggsave(file="D://cces/high_att_dumbbell_ideo1.png", type = "cairo-png", width = 12, height = 6)


#### Testing an Idea ####

e1 <- cces16 %>% 
  filter(ideo5 == 1 | ideo5 == 2) %>% 
  filter(mainline ==1) %>% 
  filter(CC16_410a < 3) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  # filter(CC16_410a ==1) %>% 
  # mutate(group = c("Conservatives")) %>% 
  mutate(attend = c("All Levels"))


e2 <- cces16 %>% 
  filter(ideo5 == 1 | ideo5 == 2) %>% 
  filter(mainline ==1) %>% 
  filter(CC16_410a < 3) %>% 
  group_by(pew_churatd) %>% 
  count(CC16_410a, wt = commonweight_vv_post) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(CC16_410a ==1) %>% 
  filter(pew_churatd ==1) %>%
  ungroup(pew_churatd) %>% 
  # mutate(group = c("Conservatives")) %>% 
  mutate(attend = c("High Attenders"))

