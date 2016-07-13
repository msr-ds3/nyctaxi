library(leaflet)
library(tigris)
source("one_week_analysis.R")

# summarize rides by pickup neighborhood
taxi_by_src_nbhd <- taxi_clean %>% group_by(pickup_neighborhood) %>% summarize(count = n()) 

map_data <- geo_join(nyc_neighborhoods, taxi_by_src_nbhd, "neighborhood", "pickup_neighborhood")

pal <- colorNumeric(palette = "Reds",
                    domain = range(map_data@data$count, na.rm=T))

leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~pal(count), popup = ~neighborhood) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)
