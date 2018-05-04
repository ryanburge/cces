#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(shinymaterial)
library(car)
library(extrafont)

bar_rb <- function(base_size = 25, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 54, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =20),
       axis.title.y =  element_text(family = "Product Sans", size =20), 
       axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1), 
       legend.text=element_text(size=36)
)
  
}


cces <- read_csv("https://raw.githubusercontent.com/ryanburge/cces/master/shiny.csv")
age <- cces$age
cces$vote16 <- factor(cces$vote16, levels=unique(cces$vote16))
cces <- na.omit(cces)
cces$vote16 <- factor(cces$vote16, levels = c("Donald Trump", "Hillary Clinton", "Gary Johnson", "Jill Stein", "Evan McMullin", "Not Sure", "Not Vote", "Other")) 


ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      sliderInput("age",
                  "Age:",
                  min= min(age),
                  max= max(age),
                  value= c(18, 99), step = 1)), 
      selectInput("reltrad", "Tradition:", as.character(levels(as.factor(cces$reltrad))) , selectize=TRUE)),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot2")
    )
  )


    
  


# 
# ui <- shinyUI(fluidPage(
#   
#   titlePanel("Party Identification of Religious Groups over the Last Four Decades"),
#   
#   fluidRow(
#     column(4,
#            wellPanel(
#              
#              sliderInput("age", "Age:", 
#                          min= min(age),
#                          max= max(age),
#                          value= c(18, 99), step = 1)), 
#            selectInput("reltrad", "Tradition:", as.character(levels(as.factor(cces$reltrad)))
#                        , selectize=TRUE)),
#     
#     
#     h5("This Shiny App has a full write-up at", a("Religion in Public", href="https://religioninpublic.blog/2017/05/11/44-years-of-religion-and-politics-in-one-graph/")),
#     h5("Some years are not available from the GSS and therefore no plot will be displayed"),
#     h5("There’s also the option to hit the “play” button and watch a tradition’s partisan distribution change over the entire time period."), 
#     
#     helpText("Data from the General Social Survey (1972-2016)"),
#     
#     column(12, 
#            plotOutput("plot2")
#     )
#   )
# ))



server <- function(input,output){
  
  cces1<-reactive({
    cces %>% filter(reltrad == input$reltrad) %>%  
      filter(age == input$age) 
    })
  

  
output$plot2<-renderPlot({
    ggplot(cces1(),aes(x=vote16, fill=factor(vote16)), color= factor(vote16))+
      geom_bar(aes(y = (..count..)/sum(..count..)), color = "black", position = position_dodge(preserve = "single")) + 
      theme(axis.text.x = element_text(angle = 90)) + 
      theme(legend.position="none")  + 
      bar_rb() +  theme(legend.position="none") +
      ggtitle("Political Ideology") +
      scale_fill_manual(values = c("Donald Trump" = "firebrick3", "Hillary Clinton" = "dodgerblue3", "Gary Johnson" = "goldenrod1", "Jill Stein" = "forestgreen", "Evan McMullin" = "azure3", "Not Sure" = "black", "Not Vote" = "grey", "Other" = "darkorchid"))+
      xlab("Party Identification") + ylab("Percent of Respondents") + scale_y_continuous(labels=percent)})}



shinyApp(ui, server)
