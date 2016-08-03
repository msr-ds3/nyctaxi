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


flow <- taxi_clean %>% 
  filter(pickup_neighborhood != dropoff_neighborhood) %>% 
  mutate(is_weekend=ifelse(day_of_the_week == "Sun"| day_of_the_week == "Sat",
                           T,
                           F)) %>%
  group_by(ymd_pickup,
           pickup_neighborhood, 
           dropoff_neighborhood,
           pickup_hour,
           is_weekend) %>%
  summarize(total_passengers = sum(passenger_count),
            total_rides = n()) 

flow <- flow %>% gather(key="type",
                        value="neighborhood", 
                        pickup_neighborhood, 
                        dropoff_neighborhood) %>%
  mutate(local_score_rides = ifelse(type=="pickup_neighborhood",
                                    -total_rides,
                                    total_rides),
    local_score_passengers = ifelse(type == "pickup_neighborhood",
                                    -total_passengers,
                                    total_passengers))


flow <- flow %>% group_by(ymd_pickup,neighborhood, pickup_hour, is_weekend) %>%
  summarize(daily_score_rides = sum(local_score_rides),
            daily_score_passengers = sum(local_score_passengers))
  
flow <- flow %>%  group_by(neighborhood, pickup_hour, is_weekend) %>%
  summarize(mean_passenger_score = mean(daily_score_passengers),
            mean_rides_score = mean(daily_score_rides),
            median_passenger_score = median(daily_score_passengers),
            median_rides_score = median(daily_score_rides))
    
  flow <- flow %>%arrange(is_weekend, pickup_hour) %>%
    mutate(mean_cumsum_passenger = cumsum(mean_passenger_score),
         mean_cumsum_rides = cumsum(mean_rides_score),
         median_cumsum_passenger = cumsum(median_passenger_score),
         median_cumsum_rides = cumsum(median_rides_score),
         adj_mean_passenger_score = 
           log10(abs(mean_passenger_score)+1) * sign(mean_passenger_score),
         adj_mean_rides_score = 
           log10(abs(mean_rides_score) + 1) * sign(mean_rides_score),
         adj_median_passenger_score = 
           log10(abs(median_passenger_score)+1) * sign(median_passenger_score),
         adj_median_rides_score = 
           log10(abs(median_rides_score) + 1) * sign(median_rides_score),
         adj_mean_cumsum_passenger_score = 
           log10(abs(mean_cumsum_passenger)+1) * sign(mean_cumsum_passenger),
         adj_mean_cumsum_rides_score = 
           log10(abs(mean_cumsum_rides) + 1) * sign(mean_cumsum_rides),
         adj_median_cumsum_passenger_score = 
           log10(abs(median_cumsum_passenger)+1) * sign(median_cumsum_passenger),
         adj_median_cumsum_rides_score = 
           log10(abs(median_cumsum_rides) + 1) * sign(median_cumsum_rides))

nbhd_hours_wknd <- data.frame(neighborhood = unique(flow$neighborhood), pickup_hour=rep(0:23, length(unique(flow$neighborhood))), is_weekend = TRUE)
nbhd_hours_wkds <- data.frame(neighborhood = unique(flow$neighborhood), pickup_hour=rep(0:23, length(unique(flow$neighborhood))), is_weekend = FALSE)
nbhd_hours <- rbind(nbhd_hours_wkds, nbhd_hours_wknd)

flow_clean <- left_join(nbhd_hours, flow, by=c("neighborhood", "pickup_hour", "is_weekend")) %>%
  mutate_each(funs(replace(., which(is.na(.)), 0)))

flow_clean <- flow_clean %>% mutate(frame_data = ifelse(pickup_hour == 0, paste(12, "AM"),
                                            ifelse(pickup_hour == 12, paste(12, "PM"),
                                                   ifelse(pickup_hour < 12,
                                                          paste(pickup_hour, "AM"),
                                                          paste(pickup_hour%%12, "PM")))))  
flow_clean$frame_data <- factor(flow_clean$frame_data, levels = c("12 AM","1 AM","2 AM",
                                                      "3 AM","4 AM","5 AM",
                                                      "6 AM","7 AM","8 AM",
                                                      "9 AM","10 AM","11 AM",
                                                      "12 PM","1 PM","2 PM",
                                                      "3 PM","4 PM","5 PM",
                                                      "6 PM","7 PM","8 PM",
                                                      "9 PM","10 PM","11 PM"))



map_data <- tidy(nyc_neighborhoods, region="neighborhood") %>% 
  left_join(., flow_clean, by=c(id="neighborhood"))
map_data_weekend <- map_data %>% filter(is_weekend == T)
map_data_weekdays <- map_data %>% filter(is_weekend == F)

weekend_map <- ggmap(map) + 
  geom_polygon(data=map_data_weekend, 
               aes(x=long, 
                   y=lat, 
                   group=group, 
                   fill=adj_median_passenger_score, 
                   frame=pickup_hour), 
               color="black", 
               size = 0.25, 
               alpha=0.5) + 
  scale_fill_distiller(palette = "Spectral",
                       na.value = "#808080",
                       labels = comma) + 
  theme_nothing(legend = T) 

gg_animate(weekend_map, ani.width=960, ani.height=960, interval = .5, "../figures/weekend_cumsum_flow.gif")


weekdays_map <- ggmap(map) + 
  geom_polygon(data=map_data_weekdays, 
               aes(x=long,
                   y=lat,
                   group=group,
                   fill=adj_median_passenger_score, 
                   frame=frame_data), 
               color="black", 
               size = 0.25, 
               alpha=0.8) +  
  scale_fill_distiller(palette = "RdBu",
                       na.value = "#808080", guide = F)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 40, face = "bold")) +
  ggtitle("Time: ")
  
gg_animate(weekdays_map, ani.width=960, ani.height=960, interval = .5)
gg_animate(weekdays_map, ani.width=960, ani.height=960, interval = .5, "../figures/weekdays_cumsum_flow.gif")
save(flow, file = "flow.Rdata")

    
