load("../Rdata/one_month_taxi.Rdata")
library(dplyr)
library(tidyr)
library(httr)
library(rgdal)
library(broom)
library(maptools)
library(ggmap)
library(scales)
library(gganimate)

get_nyc_neighborhoods <- function(){
  r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
  return(readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F))
}
nyc_neighborhoods <- get_nyc_neighborhoods()

map <- get_map(c(-73.87, 40.70), zoom = 11, color="bw",maptype = "satellite")

df <- taxi_clean %>% 
  filter(pickup_neighborhood != dropoff_neighborhood) %>% 
  group_by(pickup_neighborhood, dropoff_neighborhood, pickup_hour) %>%
  summarize(total_passengers = sum(passenger_count),
            total_rides = n()) %>% 
  gather(key="type", value="neighborhood", pickup_neighborhood, dropoff_neighborhood) %>%
  mutate(local_score_rides = ifelse(type=="pickup_neighborhood", -total_rides, total_rides),
    local_score_passengers = ifelse(type == "pickup_neighborhood", -total_passengers, total_passengers)) %>% 
  group_by(neighborhood, pickup_hour) %>%
  summarize(passenger_score = sum(local_score_passengers),
            rides_score = sum(local_score_rides)) %>%
  arrange(pickup_hour) %>%
  mutate(cumsum_passenger = cumsum(passenger_score),
    cumsum_rides = cumsum(rides_score),
    adj_passenger_score = log10(abs(cumsum_passenger)+1) * sign(cumsum_passenger),
    adj_rides_score = log10(abs(cumsum_rides) + 1) * sign(cumsum_rides))

map_data <- tidy(nyc_neighborhoods, region="neighborhood") %>% 
  left_join(., df, by=c(id="neighborhood"))

p <- ggmap(map) + 
  geom_polygon(data=map_data, 
               aes(x=long, y=lat, group=group, fill=adj_passenger_score, frame=pickup_hour), 
               color="black", 
               size = 0.25, 
               alpha=0.5) + 
  scale_fill_distiller(palette = "RdGy",
                       na.value = "#808080",
                       labels = comma) + 
  theme_nothing(legend = T)

gg_animate(p, ani.width=960, ani.height=960, "cumsum_flow.gif")

save(df, file = "cumsum_flow.Rdata")

    
