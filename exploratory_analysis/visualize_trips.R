library(ggmap)
library(maptools)
library(scales)
library(tidyr)

sample_license <- sample(unique(taxi_clean$hack_license),1)

sample_driver_trips <- taxi_clean %>% filter(hack_license == sample_license, day_of_the_week == "Sat") %>% arrange (pickup_datetime) %>% mutate(index = 1:nrow(.))

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
  nyc_map <- get_map(location = c(lon = -73.968227, lat = 40.777127), maptype = "terrain", zoom = 12, color="bw")
  
  ggmap(nyc_map) +  geom_point(data = df, mapping = aes(x=longitude, y=latitude, shape=stop_type, color= index), size=3)+  
    geom_path(data = df, mapping = aes(x=longitude, y=latitude, color=index )) + scale_colour_gradientn(colours=rainbow(3))
}

### Calling the function:
visualize_trips(sample_driver_trips, sample_driver_trips$longitude, sample_driver_trips$latitude,sample_driver_trips$stop_type, sample_driver_trips$index)



