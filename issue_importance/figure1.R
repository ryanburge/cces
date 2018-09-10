
compare <- function(df, var, ques){
  
  var <- enquo(var)
  ques <- enquo(ques)
  
  df <- df %>% mutate(newvar = recode(!! var, "1=5; 2=4; 3=3; 4=2; 5=1; else =99"))
  
  dd1 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 18 & age <= 35) %>% 
    filter(evangelical ==1) %>% 
    filter(race ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "18-35")
  
  dd2 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 36 & age <= 44) %>% 
    filter(evangelical ==1) %>% 
    filter(race ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "36-44")
  
  dd3 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 45 & age <= 54) %>% 
    filter(evangelical ==1) %>% 
    filter(race ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "45-54")
  
  dd4 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 55 & age <= 64) %>% 
    filter(evangelical ==1) %>% 
    filter(race ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "55-64")
  
  dd5 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 65 & age <= 74) %>% 
    filter(evangelical ==1) %>% 
    filter(newvar != 99) %>% 
    filter(race ==1) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "65-74")
  
  dd6 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 75 & age <= 100) %>% 
    filter(evangelical ==1) %>% 
    filter(newvar != 99) %>% 
    filter(race ==1) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "75 and Older")
  
   all <- bind_rows(dd1, dd2, dd3, dd4, dd5, dd6)
   all
  
 
}

ee1 <- cces16 %>% compare(CC16_301a, "Gun Control")
ee2 <- cces16 %>% compare(CC16_301b, "Abortion")
ee3 <- cces16 %>% compare(CC16_301c, "Taxes")
ee4 <- cces16 %>% compare(CC16_301d, "Immigration")
ee5 <- cces16 %>% compare(CC16_301e, "Budget Deficit")
ee6 <- cces16 %>% compare(CC16_301f, "Defense Spending")
ee7 <- cces16 %>% compare(CC16_301g, "Social Security")
ee8 <- cces16 %>% compare(CC16_301h, "Environment")
ee9 <- cces16 %>% compare(CC16_301i, "Jobs")
ee10 <- cces16 %>% compare(CC16_301j, "Crime")
ee11 <- cces16 %>% compare(CC16_301k, "National Security")
ee12 <- cces16 %>% compare(CC16_301l, "Race Relations")
ee13 <- cces16 %>% compare(CC16_301m, "Health Care")
ee14 <- cces16 %>% compare(CC16_301n, "Gay Marriage")
ee15 <- cces16 %>% compare(CC16_301o, "Govt. Corruption")


graph <- bind_df("ee")


font_add_google("Oswald", "font")
showtext_auto()

graph %>% 
  # filter(group == "18-35" | group == "45-54" | group == "75 and Older")  %>% 
  ggplot(., aes(y=mean, x= fct_reorder(topic, mean), color = group)) +
  geom_point(position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Issue Importance Among White Evangelicals", x = "Issue Area", y = "Level of Importance", caption = "Data: CCES 2016") +
  scale_y_continuous(limits = c(0.85,5.05), breaks = c(1,2,3,4,5), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  scale_color_npg() + 
  theme(legend.position = "bottom") +  
  theme(legend.title=element_blank()) +
  theme(text=element_text(size=64, family="font")) +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  geom_vline(xintercept = 2.5, linetype = "dotdash") +
  geom_vline(xintercept = 3.5, linetype = "dotdash") +
  geom_vline(xintercept = 4.5, linetype = "dotdash") +
  geom_vline(xintercept = 5.5, linetype = "dotdash") +
  geom_vline(xintercept = 6.5, linetype = "dotdash") +
  geom_vline(xintercept = 7.5, linetype = "dotdash") +
  geom_vline(xintercept = 8.5, linetype = "dotdash") +
  geom_vline(xintercept = 9.5, linetype = "dotdash") +
  geom_vline(xintercept = 10.5, linetype = "dotdash") +
  geom_vline(xintercept = 11.5, linetype = "dotdash") +
  geom_vline(xintercept = 12.5, linetype = "dotdash") +
  geom_vline(xintercept = 13.5, linetype = "dotdash") +
  geom_vline(xintercept = 14.5, linetype = "dotdash") 

ggsave("images/issue_imp_ages.png", type ="cairo-png", width = 14, height = 10)


rank <- function(df, var, ques){
  
  var <- enquo(var)
  ques <- enquo(ques)
  
  df <- df %>% mutate(newvar = recode(!! var, "1=5; 2=4; 3=3; 4=2; 5=1; else =99"))
  
  dd1 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 18 & age <= 35) %>% 
    filter(evangelical ==1) %>% 
    filter(race ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "18-35")
  
  dd2 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 36 & age <= 44) %>% 
    filter(evangelical ==1) %>% 
    filter(race ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "36-44")
  
  dd3 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 45 & age <= 54) %>% 
    filter(evangelical ==1) %>% 
    filter(race ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "45-54")
  
  dd4 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 55 & age <= 64) %>% 
    filter(evangelical ==1) %>% 
    filter(race ==1) %>% 
    filter(newvar != 99) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "55-64")
  
  dd5 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 65 & age <= 74) %>% 
    filter(evangelical ==1) %>% 
    filter(newvar != 99) %>% 
    filter(race ==1) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "65-74")
  
  dd6 <- df %>% 
    mutate(age = 2016 - birthyr) %>% 
    filter(age >= 75 & age <= 100) %>% 
    filter(evangelical ==1) %>% 
    filter(newvar != 99) %>% 
    filter(race ==1) %>% 
    mean_ci(newvar) %>% 
    mutate(topic = !! ques) %>% 
    mutate(group = "75 and Older")
  
  all <- bind_rows(dd1, dd2, dd3, dd4, dd5, dd6)
  all <- all %>% 
    arrange(-mean) 
  all$rank <- c(1,2,3,4,5,6)
  
  
}

ff1 <- cces16 %>% rank(CC16_301a, "Gun Control")
ee2 <- cces16 %>% compare(CC16_301b, "Abortion")
ee3 <- cces16 %>% compare(CC16_301c, "Taxes")
ee4 <- cces16 %>% compare(CC16_301d, "Immigration")
ee5 <- cces16 %>% compare(CC16_301e, "Budget Deficit")
ee6 <- cces16 %>% compare(CC16_301f, "Defense Spending")
ee7 <- cces16 %>% compare(CC16_301g, "Social Security")
ee8 <- cces16 %>% compare(CC16_301h, "Environment")
ee9 <- cces16 %>% compare(CC16_301i, "Jobs")
ee10 <- cces16 %>% compare(CC16_301j, "Crime")
ee11 <- cces16 %>% compare(CC16_301k, "National Security")
ee12 <- cces16 %>% compare(CC16_301l, "Race Relations")
ee13 <- cces16 %>% compare(CC16_301m, "Health Care")
ee14 <- cces16 %>% compare(CC16_301n, "Gay Marriage")
ee15 <- cces16 %>% compare(CC16_301o, "Govt. Corruption")

