load("../Rdata/one_month_taxi.Rdata")
library(lubridate)
library(dplyr)
########
## look at the num pickup in the same neighborhood at the same time
#####

# settime  rounding factor to 5 minutes
time_rounding_factor <- 5*60

# setspace rounding factor to 2 decimal places
pickup_rounding_factor <- .002
dropoff_rounding_factor <- 2

# add rounded columns to taxi_clean
taxi_clean <- taxi_clean %>% 
             mutate(rounded_pickup_datetime = 
              as.POSIXct(round(
                as.numeric(pickup_datetime)/time_rounding_factor)*time_rounding_factor, 
                origin = origin , tz= "UTC"), 
                rounded_pickup_lat = round(pickup_latitude/pickup_rounding_factor)*pickup_rounding_factor,
                rounded_pickup_lng = round(pickup_longitude/pickup_rounding_factor)*pickup_rounding_factor,
                rounded_dropoff_lat = round(dropoff_latitude, dropoff_rounding_factor),
                rounded_dropoff_lng = round(dropoff_longitude, dropoff_rounding_factor))

# neighborhood to neghborhood within 5 minutes
rounded_time_df <- taxi_clean %>% 
group_by(pickup_neighborhood, dropoff_neighborhood, rounded_pickup_datetime) %>%
  summarize(count = n()) %>% filter(count >1) %>% 
  filter(pickup_neighborhood != dropoff_neighborhood)

## rounded lat/lng to neighborhood within 5 minutes
rounded_pickup_time_df <- taxi_clean %>% 
  filter(pickup_neighborhood != dropoff_neighborhood) %>%
  group_by(rounded_pickup_lat, 
           rounded_pickup_lng,
           dropoff_neighborhood,
           rounded_pickup_datetime) %>%
  summarize(count = n()) %>% 
  filter(count >1, !is.na(dropoff_neighborhood))

# rounded pickup lng/lat to  roubded dropoff lng/lat within 5 minutes
rounded_pickup_dropoff_time_df <- taxi_clean %>% 
  filter(pickup_neighborhood != dropoff_neighborhood) %>%
  group_by(rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat, 
           rounded_dropoff_lng,
           rounded_pickup_datetime) %>%
  summarize(count = n(),
            total_num_passenger = sum(passenger_count),
            passengers_per_ride = total_num_passenger / count,
            total_distance = sum(trip_distance),
            total_trip_time = sum(trip_time_in_secs/3600),
            avg_speed = total_distance/ total_trip_time,
            total_fare = sum(fare_amount),
            fare_per_ride = total_fare / count
          ) %>% filter(count > 1)

### 
overlapping_rides <- rounded_pickup_dropoff_time_df %>%
  mutate(hour = hour(rounded_pickup_datetime), 
         day = wday(rounded_pickup_datetime), 
         is_weekend=ifelse(day == 1 | day == 7, T, F)) %>%
  group_by(rounded_pickup_lat, rounded_pickup_lng, is_weekend, hour) %>%
  summarize(avg = mean(count),
            sd = sd(count))
