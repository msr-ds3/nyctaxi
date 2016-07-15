source("one_week_analysis.R")
library(ggmap)
library(scales)


make_map <- function(data, map)
{
  plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>% 
    left_join(., data, by=c("id"="pickup_neighborhood"))
  
  ggmap(map) + 
    geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=summary), color="black", size = 0.25, alpha=0.5) + 
    scale_fill_distiller(palette = "Spectral", trans = "log10", breaks = c(1, 10, 100, 1000, 10000, 100000), na.value = "#808080") +
    theme_nothing(legend = T)
}

nyc_map <- get_map(location = c(lon = -73.95, lat = 40.71), maptype = "terrain", zoom = 10, color="bw")

data <- taxi_clean %>% group_by(pickup_neighborhood) %>% summarize(summary =n())
make_map(data, nyc_map)



