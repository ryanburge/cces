
dumbbell <- data.frame("class" =c("White Evangelical", "Mainline", "Black Protestant", "Mormon","Catholic", "Jewish", "Muslim", "Hindu", "Buddhist", "Atheist"), vote16 = c(75.8,51.5,8.5,45,49.2,26.9, 14.7, 24.1, 18, 18.3), label = c("Trump's Share"))
dumbbell$vote12 <- c(74.5,52.9, 4.9, 82.5, 50.1, 33.9, 13.2, 16.3, 12.6, 16.5)



dumbbell <- arrange(dumbbell, desc(vote12))

dumbbell <- mutate(dumbbell, class=factor(class, levels=rev(class)))


percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*1))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}

gg <- ggplot()

gg <- ggplot(data=dumbbell, aes(y=class, x=vote12, xend = vote16))

gg <- gg + geom_dumbbell(colour = "#686868", colour_x = "purple1", colour_xend = "firebrick1", size_x = 3, size_xend = 3, size = 1)

gg <- gg + scale_x_continuous(breaks=seq(5, 95, by=10))
gg <- gg + labs(x="Republican Nominee's Vote Share", y=NULL,
                title="How Did the Religious Landscape Shift in 2016?",
                caption="Data from CCES 2016")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.title.x=element_text(hjust=1, face="italic", margin=margin(t=-24)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=24)))

gg <- gg + geom_text(data=dumbbell, aes(x=vote12, y=class, label=percent_first(vote12)),
                     color="purple1", size=2.75, vjust=2.5, family="Calibri")
gg <- gg + geom_text(data=dumbbell, color="firebrick1", size=2.75, vjust=2.5, family="Calibri",
                     aes(x=vote16, y=class, label=percent_first(vote16)))

gg <- gg + geom_text(data=filter(dumbbell, class=="Mormon"),
                     aes(x=vote12, y=class, label="2012"),
                     color="purple1", size=3, vjust=-2, fontface="bold", family="Calibri")

gg <- gg + geom_text(data=filter(dumbbell, class=="Mormon"),
                     aes(x=vote16, y=class, label="2016"),
                     color="firebrick1", size=3, vjust=-2, fontface="bold", family="Calibri")

gg <- gg +  theme(text=element_text(size=18, family="KerkisSans"))




