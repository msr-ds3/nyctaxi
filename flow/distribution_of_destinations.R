library(dplyr)

## load n the daya
load("../Rdata/one_month_taxi.Rdata")
taxi_clean <- taxi_clean %>% 
  mutate(dropoff_lat = round(dropoff_latitude, 2), 
         dropoff_lng = round(dropoff_longitude, 2),
         is_weekend=ifelse(day_of_the_week == "Sun"|
                             day_of_the_week == "Sat",
                           T,
                           F))
 
probs_dst_overall <- taxi_clean %>% 
  group_by(dropoff_lat, dropoff_lng, pickup_hour, is_weekend) %>% 
  summarize(overall_n = n()) %>% 
  group_by(pickup_hour, is_weekend) %>% 
  mutate(overall_total = sum(overall_n),
         overall_probability = overall_n/overall_total)
  

# round drop lat/lng to 2 decimal places, and fi
probs_dst_given_src_and_time <- taxi_clean %>%
  group_by(pickup_neighborhood,
           pickup_hour,
           dropoff_lat,
           dropoff_lng,
           is_weekend) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(pickup_neighborhood)) %>% # filtering out trips that go out of the city
  group_by(pickup_neighborhood, pickup_hour, is_weekend) %>%
  mutate(total = sum(n), probability = n/total) 

probability <- left_join (probs_dst_given_src_and_time, probs_dst_overall, 
                    by = c("pickup_hour",
                           "dropoff_lat",
                           "dropoff_lng",
                           "is_weekend"))

probability <- probability %>% 
  mutate(a = probability/ overall_probability,
         b = ((probability/(1-probability)) - (overall_probability/ (1-overall_probability))),
         c = log(probability/ (1-probability)) - log(overall_probability/ (1-overall_probability)))


### without hour
probs_dst_overall_without_hour <- taxi_clean %>% 
  group_by(dropoff_lat, dropoff_lng, is_weekend) %>% 
  summarize(overall_n = n()) %>% 
  group_by(is_weekend) %>% 
  mutate(overall_total = sum(overall_n),
         overall_probability = overall_n/overall_total)


# round drop lat/lng to 2 decimal places, and fi
probs_dst_given_src_and_time_without_hour<- taxi_clean %>%
  
  group_by(pickup_neighborhood, dropoff_lat, dropoff_lng, is_weekend) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(pickup_neighborhood)) %>%# filtering out trips that go out of the city
  group_by(pickup_neighborhood, is_weekend) %>%
  mutate(total = sum(n), probability = n/total) 

probability_without_hour <- left_join (probs_dst_given_src_and_time_without_hour, 
                                       probs_dst_overall_without_hour, 
                          by = c("dropoff_lat", "dropoff_lng", "is_weekend"))

probability_without_hour <- probability_without_hour %>% 
  mutate(a = probability/ overall_probability,
         b = ((probability/ (1-probability)) - (overall_probability/ (1-overall_probability))),
         c = log(probability/ (1-probability)) - log(overall_probability/ (1-overall_probability)))




  
