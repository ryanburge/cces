library(fst)

gss <- read.fst("C://gss.fst")

library(tidyverse)

df.us<-dplyr::filter(df,state=="US") %>%  select(date,hpi) %>% 
  mutate(day=as.numeric(date-min(date)+1),ease="linear")

plot_data_tween<-tween_elements(df.us, time = "day",  group="ease", ease="ease", nframes = nrow(df.us)*5)
df_tween_appear <- tween_appear(plot_data_tween, time='day', nframes = nrow(df.us)*5)

bib <- gss %>% 
  filter(bible != 4) %>% 
  filter(bible != "NA") %>% 
  group_by(year) %>% 
  count(bible) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(bible ==1) %>% 
  select(year, pct) %>% 
  ungroup(year) %>% 
  mutate(year=as.numeric(year),ease="linear")


plot_data_tween<-tween_elements(bib, time = "year",  group="ease", ease="ease", nframes = nrow(bib)*5)
df_tween_appear <- tween_appear(plot_data_tween, time='year', nframes = nrow(bib)*5)

# add pause at end of animation
df_tween_appear<- df_tween_appear %>% keep_state(20)

make_plot_appear <- function(i){
  plot_data <- 
    df_tween_appear %>% filter(.frame==i, .age> -3.5) 
  p<- plot_data %>% 
    ggplot()+
    geom_line(aes(x=year, y=pct),color="royalblue", size=1.3) +
    geom_point(data=. %>% filter(year==max(year)), mapping=aes(x=year, y=pct), size=3,color="red",stroke=1.5)+
    geom_point(data=. %>% filter(year==max(year)), mapping=aes(x=year, y=pct), color="white", size=2)+
    geom_text(data=. %>% filter(year==max(year)), mapping=aes(x=year, y=pct,label=pct),color="red",nudge_x=7,hjust=-0.4,fontface="bold")+
    geom_line(data=bib, aes(x=year,y=pct),alpha=0.1)
    return(p)
}

oopt<-ani.options(interval=1/20)
saveGIF({for (i in 1:max(df_tween_appear$.frame)){
  g<-make_plot_appear(i)
  print(g)
  print(paste(i,"out of",max(df_tween_appear$.frame)))
  ani.pause()
}
},movie.name="firsttry.gif",ani.width = 700, ani.height = 540)