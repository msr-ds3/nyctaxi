library(ggmap)

library(maptools)
library(gganimate)
library(scales)
source("one_week_anaylsis.R")

nyc_map <- get_map(location = c(lon = -73.95, lat = 40.71), maptype = "terrain", zoom = 11, color="bw")

data <- taxi_clean %>% group_by(pickup_neighborhood, hour) %>% summarize(summary = n()/7)

plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>% 
  left_join(., data, by=c("id"="pickup_neighborhood"))

p <- ggmap(nyc_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=summary, frame=hour), color="black", size = 0.25, alpha=0.5) + 
  scale_fill_distiller(palette = "Spectral", trans = "log10", breaks = c(1 %o% 10^(0:5)), na.value = "#808080", labels = comma, name="avg num of trips") +

theme_nothing(legend = T)

gg_animate(p, pause=.5,ani.width =960, ani.height=960)
