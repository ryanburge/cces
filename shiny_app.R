shiny <- read_csv("https://raw.githubusercontent.com/ryanburge/cces/master/shiny.csv")

shiny$vote16 <- factor(shiny$vote, levels = c("Donald Trump", "Hillary Clinton", "Gary Johnson", "Jill Stein", "Evan McMullin", "Not Sure", "Not Vote", "Other")) 


ggplot(shiny,aes(x=vote16, fill=factor(vote16)), color= factor(vote16))+
    geom_bar(aes(y = (..count..)/sum(..count..)), color = "black", position = position_dodge(preserve = "single")) + 
    theme(axis.text.x = element_text(angle = 90)) + 
    theme(legend.position="none")  + 
    bar_rb() +  theme(legend.position="none") +
    ggtitle("Political Ideology") +
    scale_fill_manual(values = c("Donald Trump" = "firebrick3", "Hillary Clinton" = "dodgerblue3", "Gary Johnson" = "goldenrod1", "Jill Stein" = "forestgreen", "Evan McMullin" = "azure3", "Not Sure" = "black", "Not Vote" = "grey", "Other" = "darkorchid"))+
    xlab("Party Identification") + ylab("Percent of Respondents") + scale_y_continuous(labels=percent)
