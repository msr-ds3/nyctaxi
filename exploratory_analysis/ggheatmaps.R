library(ggmap)
library(scales)

nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 10)

nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

data <- taxi_clean %>% group_by(pickup_neighborhood) %>% summarize(summary =n())

plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>% 
  inner_join(., data, by=c("id"="pickup_neighborhood")) %>%
  filter(!is.na(summary))

ggmap(nyc_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=summary), color="black", size = 0.25) + 
  scale_fill_distiller(palette = "Spectral", trans = "log10") +
  theme_nothing(legend = T)
