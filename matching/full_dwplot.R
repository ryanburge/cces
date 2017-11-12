
treg08 <- read_csv("D://cces/matching/treg08.csv") %>% mutate(model= c("2008"))
treg12 <- read_csv("D://cces/matching/treg12.csv") %>% mutate(model= c("2012"))
treg16 <- read_csv("D://cces/matching/treg16.csv") %>% mutate(model= c("2016"))



plot <- bind_rows(treg08, treg12, treg16)

plot <- plot[-1,]
plot <- plot[-10,]
plot <- plot[-19,]


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
  labs(x= "Coefficient Predicting Vote for the Republican", y = "", title = "What Motivates Born Again Protestants to Vote Republican? ")  +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=32, family="KerkisSans"))


ggsave(file="D:/cces/matching/plot.png", type = "cairo-png", width = 15, height = 12)

regplot <- bind_rows(reg08, reg12, reg16) %>% rename(model = year)

write.csv(regplot, "regplot.csv")

dwplot(regplot, dot_args = list(size = 5)) + 
  # relabel_predictors(c(treated = "White",
  #                      age = "Age",
  #                      educ = "Education",
  #                      income = "Income",
  #                      male = "Male",
  #                      imp = "Importance of Religion",
  #                      pid = "Republican ID",
  #                      attend = "Church Attendance",
  #                      pray = "Frequency of Prayer")) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, 0), legend.position=c(.85, .8),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank())  + guides(colour = guide_legend(reverse=T)) +
  labs(x= "Coefficient Predicting Vote for the Republican", y = "", title = "What Motivates Born Again Protestants to Vote Republican? ")  +
  theme(plot.title = element_text(hjust = 0.5))  
  # theme(text=element_text(size=32, family="KerkisSans"))

ggsave(file="D:/cces/matching/regression_plot.png", type = "cairo-png", width = 15, height = 12)
