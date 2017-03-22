cces <- read_dta("C:/Users/Ryan Burge/Desktop/cces.dta")

abc <- filter(cces, religpew_baptist ==2)
wpct(abc$pew_bornagain, abc$commonweight_post)
##66.7%
sbc <- filter(cces, religpew_baptist ==1)
wpct(sbc$pew_bornagain, sbc$commonweight_post)

pcusa <- filter(cces, religpew_presby == 1)
wpct(pcusa$pew_bornagain, pcusa$commonweight_post)
## 27.8
elca <- filter(cces, religpew_lutheran ==1)
wpct(elca$pew_bornagain, elca$commonweight_post)
## 27.9
nd <- filter(cces, evannd ==1)
wpct(nd$pew_bornagain, nd$pew_bornagain)
## 51.7%
umc <- filter(cces, religpew_methodist ==1)
wpct(umc$pew_bornagain, umc$commonweight_post)
## 38.8%

doc <- filter(cces, religpew_christian ==2)
wpct(doc$pew_bornagain, doc$commonweight_post)
## 46.1% 

ucoc <- filter(cces, religpew_congreg ==1)
wpct(ucoc$pew_bornagain, ucoc$commonweight_post)
## 26.4%

pente <- filter(cces, pente ==1)
wpct(pente$pew_bornagain, pente$commonweight_post)
## 87.4%

episc <- filter(cces, religpew_protestant ==7)
wpct(episc$pew_bornagain, episc$commonweight_post)
#17.6%


ml <- data.frame("denom" =c("American Baptist", "Presby. Church (USA)", "Evangelical Lutheran", "Non-Denom.", "United Methodist", "Disc. of Christ", "United Church of Christ", "Episcopal"), 
                     pct = c(66.7,27.8,27.9, 51.7,38.8,46.1,26.4,17.6))


p1 <- ggplot(ml, aes(x=reorder(denom,pct), y=pct)) + geom_bar(fill = "#1A476F", color = "black", stat = "identity") + coord_flip() + theme_minimal(base_family="Arial Narrow") + labs(x="Denomination", y="Percent Identifying as Born Again or Evangelical", 
                                                   title="Are Mainliners Born Again? ",
                                                   subtitle="",
                                                   caption="Data from CCES 2016") + annotate("text", x = .75, y = 60, label = "religioninpublic.blog")
