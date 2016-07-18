library(ggmap)
library(maptools)
library(scales)
library(tidyr)

sample_driver_trips <- taxi_clean %>% filter(hack_license == "938EF5D4604A7AB7F6C113C9C26FEAEE", day_of_the_week == "Mon") %>% mutate(index = 1:nrow(.))

#reshape data so that each observation has a dropoff/pickup row with the corresponding latitude and longitude (used to ease use of geom_path)
sample_driver_trips = sample_driver_trips %>%
  gather(key = "key", value = "coordinate", pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude) %>%
  extract(col = key, into = c("stop_type", "coord_type"), regex = "([a-zA-Z]+)_([a-zA-Z]+)") %>%
  spread(key = coord_type, value = coordinate) %>%
  arrange(pickup_datetime)

##############################################################
## Function to create visualization of trips on a NYC map

visualize_trips <- function(df, longitude, latitude, stop_type, index)
{
  nyc_map <- get_map(location = c(lon = -73.968227, lat = 40.777127), maptype = "terrain", zoom = 13, color="bw")
  
  ggmap(nyc_map) +  geom_point(data = df, mapping = aes(x=longitude, y=latitude, shape=stop_type), size=3)+  
    geom_path(data = df, mapping = aes(x=longitude, y=latitude, color=as.factor(index))) 
}


### Calling the function:
visualize_trips(sample_driver_trips, sample_driver_trips$longitude, sample_driver_trips$latitude,sample_driver_trips$stop_type, sample_driver_trips@index)
