library(dplyr)

## load n the daya
load("../Rdata/one_month_taxi.Rdata")

# round drop lat/lng to 2 decimal places, and fi
probs_dst_given_src_and_time <- taxi_clean %>% 
  mutate(dropoff_lat = round(dropoff_latitude, 2), 
         dropoff_lng = round(dropoff_longitude, 2)) %>%
  group_by(pickup_neighborhood, pickup_hour, dropoff_lat, dropoff_lng) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(pickup_neighborhood)) %>%# filtering out trips that go out of the city
  group_by(pickup_neighborhood, pickup_hour) %>%
  mutate(total = sum(n), probability = n/total) 

  
