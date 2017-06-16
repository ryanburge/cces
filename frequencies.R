## This is how to make Figure 1 for the POQ Measuring Evangelicals Piece

ev16 <- filter(cces16, evangelical ==1 & white ==1)
##16.5
ba16 <- filter(cces16, bagain ==1 & white ==1 & protestant ==1)
##13.8
ev12 <- filter(cces12, evangelical ==1 & white ==1)
##19.1
ba12 <- filter(cces12, bagain ==1 & white ==1 & protestant ==1)
##17.6
ev08 <- filter(cces08, evangelical ==1 & white ==1)
##20.6
ba08 <- filter(cces08, bagain ==1 & white ==1 & protestant ==1)
##17.2

cces1 <- data.frame(survey = c("CCES 2008", "CCES 2008", "CCES 2012", "CCES 2012", "CCES 2016", "CCES 2016"),
                   sample = c("Evangelical", "BA + Prot.", "Evangelical", "BA + Prot.", "Evangelical", "BA + Prot."),
                   pct =c(20.6, 17.2, 19.1, 17.6, 16.5, 13.8))

ev10 <- filter(gss10, evangelical ==1 & white ==1)
##20.2
ba10 <- filter(gss10, bagain ==1 & white ==1 & protestant ==1)
##18.7
ev12 <- filter(gss12, evangelical ==1 & white ==1)
##20.2
ba12 <- filter(gss12, bagain ==1 & white ==1 & protestant ==1)
##19.8
ev14 <- filter(gss14, evangelical ==1 & white ==1)
##18.6
ba14 <- filter(gss14, bagain ==1 & white ==1 & protestant ==1)
##18.9

gss1 <- data.frame(survey = c("GSS 2010", "GSS 2010", "GSS 2012", "GSS 2012", "GSS 2014", "GSS 2014", "GSS 2016", "GSS 2016"),
                   sample = c("Evangelical", "BA + Prot.", "Evangelical", "BA + Prot.", "Evangelical", "BA + Prot.", "Evangelical", "BA + Prot."),
                   pct =c(20.2, 18.7, 20.2, 19.8, 18.6, 18.9, 19.0, 19.8 ))

total <- rbind(gss1, cces1)

total$moe <- c(4.31, 4.52, 4.39, 4.44, 4.07, 4.03, 3.78, 3.68, 1.06, 1.19, .86, .91, .87, .97)

limits <- aes(ymax = total$pct + total$moe, ymin = total$pct - total$moe)

total$max <- total$pct + total$moe
total$min <- total$pct - total$moe


ggplot(total, aes(x=survey, y=pct, fill = sample)) + geom_col(position = "dodge")+ 
  geom_errorbar(aes(ymin = min, ymax=max), width = .25, position=position_dodge(.9), color = "azure4") +
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme(legend.position="bottom") +
  ggtitle("Difference in Evangelical Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Survey and Year") 
   
