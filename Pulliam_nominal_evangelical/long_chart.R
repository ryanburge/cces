library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(extrafont)

gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")

gss$bagain <- Recode(gss$reborn, "1=1;else=0")
gss$white <- Recode(gss$race, "1=1;else=0")
gss$protestant <- Recode(gss$relig, "1=1;else=0")

gss$whtevan <- gss$evangelical + gss$white
gss$whtbaprot <- gss$bagain + gss$white + gss$protestant

whtevan <- gss %>% filter(whtevan ==2 & year >=2004) %>% 
  group_by(year) %>% summarise(mean = mean(partyid, na.rm=TRUE)) %>% 
  mutate(label = c("Affiliation"))

whtbaprot <- gss %>% filter(whtbaprot ==3 & year >=2004) %>% 
  group_by(year) %>% summarise(mean = mean(partyid, na.rm=TRUE)) %>% 
  mutate(label = c("Self ID"))

pid <- rbind(whtevan, whtbaprot)

ggplot(pid, aes(x=year, y=mean, color = label, label = label)) + geom_line(aes(group=label), size = 1.5) + 
  coord_flip() + scale_y_continuous(limits = c(0,6), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=20, family="KerkisSans")) + 
  labs(x= "Year", y = "Mean Party Identification", title = "Comparing the Two Measures on Party Affiliation", caption = "Data from the GSS (2004-2016)")+
  scale_color_manual(values = c("black", "gray", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) 

ggsave(file="long_compare_poq_final.png", type = "cairo-png", width = 15, height = 10)