install.packages("fiftystater")
library(fiftystater)


graph <- graph %>% mutate(state = tolower(state))


ggplot(graph, aes(map_id = state)) +
  geom_map(aes(fill = pct), map = fifty_states)  + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) + fifty_states_inset_boxes() +
  labs(title = "What States Are the Most Evangelical?") + theme(text=element_text(size=36, family="IBM Plex Serif")) + 
  scale_fill_gradientn(colours=c("deepskyblue","purple", "red"), na.value = "grey98", name = "% Evangelical") + theme(legend.key.width = unit(5, "cm"))

ggsave(file="D://cces/fifty_state_map.png", type = "cairo-png", width = 20, height =12)
