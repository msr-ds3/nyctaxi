library(ggmap)
library(maptools)
library(scales)
library(tidyr)
library(dplyr)
library(RColorBrewer)

nyc_map <- get_map(location = c(lon = -73.968227, lat = 40.777127), 
                                maptype = "terrain", 
                                zoom = 12, 
                                color="bw")

generate_map <- function(df)
{
  ggmap(nyc_map) + 
    geom_point(data = df, 
               mapping = aes(x=longitude, y=latitude, shape=stop_type, 
                             color= minutes_since_midnight), size=3) +  
  geom_path(data = df, mapping = aes(x=longitude, y=latitude, 
                                     color=minutes_since_midnight )) + 
    scale_colour_gradientn(colors = brewer.pal(11, "Spectral"))
}

filter_by_driver = function(df, hacklicense)
{
  df %>% 
    filter(hack_license == hacklicense) 
    
}
filter_by_day = function(df, day)
{
  df %>% 
    filter(day_of_the_week == day)
}

filter_by_shift = function(df, shift)
{
  df %>% 
    filter(index == shift)
}

reshape_location_data = function(df)
{
  
    df %>%
    gather(key = "key", value = "coordinate", 
           pickup_longitude, pickup_latitude, 
           dropoff_longitude, dropoff_latitude) %>%
    extract(col = key, into = c("stop_type", "coord_type"), 
            regex = "([a-zA-Z]+)_([a-zA-Z]+)") %>%
    spread(key = coord_type, value = coordinate) %>%
    arrange(pickup_datetime)
  
}

visualize_trips_by_day = function(df, hacklicense, day= NULL)
{
  df = filter_by_driver(df, hacklicense)
  if (!is.null(day))
  {
    df = filter_by_day(df, day)
  }
  df = df %>% arrange (pickup_datetime) %>% 
    mutate(minutes_since_midnight = pickup_hour *60 + pickup_minute)
  df = reshape_location_data(df)
  generate_map(df) + facet_wrap(~day_of_the_week)
  
}

visualize_trips_by_shift = function(df, hacklicense, shift = NULL)
{
  df = filter_by_driver(df, hacklicense)
  if (!is.null(shift))
  {
    df = filter_by_shift(df, shift)
  }
  df = df %>% arrange (pickup_datetime) %>% 
    mutate(minutes_since_midnight = pickup_hour *60 + pickup_minute)
  df = reshape_location_data(df)
  generate_map(df) + facet_wrap(~index)
  
}