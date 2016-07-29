library(dplyr)

## load n the daya
load("../Rdata/one_month_taxi.Rdata")

total_per_src_and_time <- taxi_clean %>%
  group_by(pickup_neighborhood, pickup_hour) %>%
  summarize(total = n())
# round drop lat/lng to 2 decimal places, and fi
probs_dst_given_src_and_time <- taxi_clean %>% 
  mutate(dropoff_lat = round(dropoff_latitude, 2), 
         dropoff_lng = round(dropoff_longitude, 2)) %>%
  group_by(pickup_neighborhood, pickup_hour, dropoff_lat, dropoff_lng) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(pickup_neighborhood)) # filtering out trips that go out of the city

probs_dst_given_src_and_time <- left_join(probs_dst_given_src_and_time,
                                          total_per_src_and_time, 
                                          by=c("pickup_neighborhood", 
                                               "pickup_hour")
                                          ) %>%
  mutate(probability = n/total)
  
  
