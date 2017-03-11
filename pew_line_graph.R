ggplot(data = graph, aes(x = year, y = rpct)) + geom_bar(aes(group = Dept),stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~Dept) + ylab("Personnel Expenses (in thousands)" ) + 
  geom_hline(yintercept = 0, linetype = "longdash") +
  ggtitle("College of Arts and Humanities")

repub <- filter(graph, party == "Republican")

ggplot(data = repub, aes(x = year, y = rpct, color = label, label = label)) +
  geom_line(aes(group = label), size=1, linetype = 1)  + ylab("Republican Candidate Percentage") + 
  xlab("Year") + ggtitle("Different Measures of Evangelical")  +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(text=element_text(size=18, family="KerkisSans")) + theme(legend.title = element_blank()) +  
  scale_colour_brewer("Colors in Spectral", palette="Set1")

