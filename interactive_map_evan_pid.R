


library(dplyr)
library(ggplot2)
library(extrafont)
library(leaflet)
library(viridis)
library(spdplyr)
library(haven)
library(readr)
library(car)


cces16 <- read_dta("D://cces/data/cces16.dta")

cces16 %>% 
  filter(religpew ==1 & pew_bornagain ==1) %>% 
  filter(pid7 <8) %>% 
  # group_by(inputstate) %>% 
  summarise(mean = mean(pid7))

bystate <- cces16 %>% 
  filter(religpew ==1 & pew_bornagain ==1) %>% 
  filter(pid7 <8) %>% 
  group_by(inputstate) %>% 
  summarise(mean = mean(pid7)) %>% 
  mutate(diff = mean - 4.347239)
  
st <- rgdal::readOGR("state.json", "OGRGeoJSON")


bystate <- bystate %>% filter(inputstate != 11)


bystate$STATE <- sprintf("%02d", bystate$inputstate)
bystate$STATE <- as.factor(bystate$STATE)


bystate <- bystate %>% select(STATE, diff)


map <- left_join(st, bystate)

palette_rev <- rev(brewer.pal(10, "RdBu"))

pal <- colorNumeric(palette = palette_rev,  domain = map$diff)


myLabelFormat = function(..., reverse_order = FALSE){ 
  if(reverse_order){ 
    function(type = "numeric", cuts){ 
      cuts <- sort(cuts, decreasing = TRUE)
    } 
  }else{
    labelFormat(...)
  }
}



leaflet(map) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, weight = 1, smoothFactor = 0.3, fillOpacity = 1, color = "black",
              fillColor = ~pal(map$diff), 
              label = ~paste0(map$diff))%>%
  addLegend(pal = pal, values = rev(map$diff), opacity = 1.0, labFormat = myLabelFormat(reverse_order = T)) %>% 
  setView(-98.690940, 39.651426, zoom = 4)


  


