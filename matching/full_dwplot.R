
plot <- bind_rows(treg08, treg12, treg16)

plot <- plot[-6,]
plot <- plot[-8,]


dwplot(plot) %>% 
  relabel_predictors(c(treated = "White",
                      age = "Age",
                      educ = "Education",
                      income = "Income",
                      male = "Male",
                      imp = "Importance of Religion",
                      pid = "Republican ID",
                      attend = "Church Attendance",
                      pray = "Frequency of Prayer")) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, 0), legend.position=c(.85, .8),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank())  + guides(colour = guide_legend(reverse=T)) +
  labs(x= "Coefficient for Predicting Vote for the Republican", y = "", title = "What Motivates Born Again Protestants to Vote Republican? ")  +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=18, family="KerkisSans"))