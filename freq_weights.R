cces16$whtevan <- cces16$evangelical + cces16$white
cces16$whtevan <- Recode(cces16$whtevan, "2=1; else=0")
cces16 %>% count(whtevan, wt = commonweight) %>% mutate(weight = prop.table(n))
## 18.7%

cces16$whtbaprot <- cces16$bagain + cces16$white + cces16$protestant
cces16$whtbaprot <- Recode(cces16$whtbaprot, "3=1; else=0")
cces16 %>% count(whtbaprot, wt = commonweight) %>% mutate(weight = prop.table(n))
## 16.2%

cces12$whtevan <- cces12$evangelical + cces12$white
cces12$whtevan <- Recode(cces12$whtevan, "2=1; else=0")
cces12 %>% count(whtevan, wt = weight_vv) %>% mutate(weight = prop.table(n))
## 19.6%

cces12$whtbaprot <- cces12$bagain + cces12$white + cces12$protestant
cces12$whtbaprot <- Recode(cces12$whtbaprot, "3=1; else=0")
cces12 %>% count(whtbaprot, wt = weight_vv) %>% mutate(weight = prop.table(n))
## 18.3%

cces08$whtevan <- cces08$evangelical + cces08$white
cces08$whtevan <- Recode(cces08$whtevan, "2=1; else=0")
cces08 %>% count(whtevan, wt = V201) %>% mutate(weight = prop.table(n))
## 18.1%

cces08$whtbaprot <- cces08$bagain + cces08$white + cces08$protestant
cces08$whtbaprot <- Recode(cces08$whtbaprot, "3=1; else=0")
cces08 %>% count(whtbaprot, wt = V201) %>% mutate(weight = prop.table(n))
## 14.2%

cces1 <- data.frame(survey = c("CCES 2008", "CCES 2008", "CCES 2012", "CCES 2012", "CCES 2016", "CCES 2016"),
                    sample = c("Evangelical", "BA + Prot.", "Evangelical", "BA + Prot.", "Evangelical", "BA + Prot."),
                    pct =c(18.1, 14.2, 19.6, 18.3, 18.7, 16.2))

gss10$whtevan <- gss10$evangelical + gss10$white
gss10$whtevan <- Recode(gss10$whtevan, "2=1; else=0")
gss10 %>% count(whtevan, wt = wtssall) %>% mutate(weight = prop.table(n))
## 20.4%

gss10$whtbaprot <- gss10$bagain + gss10$white + gss10$protestant
gss10$whtbaprot <- Recode(gss10$whtbaprot, "3=1; else=0")
gss10 %>% count(whtbaprot, wt = wtssall) %>% mutate(weight = prop.table(n))
## 18.5%

gss12$whtevan <- gss12$evangelical + gss12$white
gss12$whtevan <- Recode(gss12$whtevan, "2=1; else=0")
gss12 %>% count(whtevan, wt = wtss) %>% mutate(weight = prop.table(n))
## 19.8%

gss12$whtbaprot <- gss12$bagain + gss12$white + gss12$protestant
gss12$whtbaprot <- Recode(gss12$whtbaprot, "3=1; else=0")
gss12 %>% count(whtbaprot, wt = wtss) %>% mutate(weight = prop.table(n))
## 19.0%

gss14$whtevan <- gss14$evangelical + gss14$white
gss14$whtevan <- Recode(gss14$whtevan, "2=1; else=0")
gss14 %>% count(whtevan, wt = wtss) %>% mutate(weight = prop.table(n))
## 18.7%

gss14$whtbaprot <- gss14$bagain + gss14$white + gss14$protestant
gss14$whtbaprot <- Recode(gss14$whtbaprot, "3=1; else=0")
gss14 %>% count(whtbaprot, wt = wtss) %>% mutate(weight = prop.table(n))
## 18.7%

gss16$whtevan <- gss16$evangelical + gss16$white
gss16$whtevan <- Recode(gss16$whtevan, "2=1; else=0")
gss16 %>% count(whtevan, wt = wtssall) %>% mutate(weight = prop.table(n))
## 19.0%

gss16$whtbaprot <- gss16$bagain + gss16$white + gss16$protestant
gss16$whtbaprot <- Recode(gss16$whtbaprot, "3=1; else=0")
gss16 %>% count(whtbaprot, wt = wtssall) %>% mutate(weight = prop.table(n))
## 19.8%

gss1 <- data.frame(survey = c("GSS 2010", "GSS 2010", "GSS 2012", "GSS 2012", "GSS 2014", "GSS 2014", "GSS 2016", "GSS 2016"),
                   sample = c("Evangelical", "BA + Prot.", "Evangelical", "BA + Prot.", "Evangelical", "BA + Prot.", "Evangelical", "BA + Prot."),
                   pct =c(20.4, 18.5, 19.8, 19.0, 18.7, 18.7, 19.0, 19.8))

total <- rbind(gss1, cces1)

total$moe <- c(4.31, 4.52, 4.39, 4.44, 4.07, 4.03, 3.78, 3.68, 1.06, 1.19, .86, .91, .87, .97)

limits <- aes(ymax = total$pct + total$moe, ymin = total$pct - total$moe)

total$max <- total$pct + total$moe
total$min <- total$pct - total$moe


ggplot(total, aes(x=survey, y=pct, fill = sample)) + geom_col(position = "dodge")+ 
  geom_errorbar(aes(ymin = min, ymax=max), width = .25, position=position_dodge(.9), color = "azure4") +
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme(legend.position="bottom") +
  ggtitle("Difference in Measuring Evangelicals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Survey and Year") 

ggsave(file="freq_with_weights.png", type = "cairo-png", width = 12, height = 12)
