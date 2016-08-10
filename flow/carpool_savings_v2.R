load("../Rdata/one_month_taxi.Rdata")
library(lubridate)
library(dplyr)

# rounds date to nearest seconds
round_date_to <- function(x, seconds = 5*60){
  as.POSIXct(round(as.numeric(x)/seconds)*seconds, 
             origin = origin,
             tz = tz(x))
}

# rounds numbers to nearst rounding factor
round_to <- function(x, rounding_factor)
{
  round(x/rounding_factor)*rounding_factor
}

# point all airport lat and longs to one coordiante pair
airport_lat <-function(x, nbhd)
{
  ifelse(nbhd == "John F. Kennedy International Airport", 40.641,
  ifelse(nbhd == "LaGuardia Airport", 40.773,
  x))
}

airport_lng <-function(x, nbhd)
{
  ifelse(nbhd == "John F. Kennedy International Airport", -73.777,
  ifelse(nbhd == "LaGuardia Airport", 73.872,
  x))
}
  
taxi <- taxi_clean %>% 
  mutate(pickup_latitude = airport_lat(pickup_latitude, pickup_neighborhood),
         pickup_longitude = airport_lng(pickup_longitude, pickup_neighborhood),
         dropoff_latitude = airport_lat(dropoff_latitude, dropoff_neighborhood),
         dropoff_longitude = airport_lng(dropoff_longitude, dropoff_neighborhood))

# create a subset of relevant columns
taxi <- taxi %>%
  mutate(fare = fare_amount+surcharge) %>%
  select(pickup_datetime,
         passenger_count,
         pickup_longitude,
         pickup_latitude,
         dropoff_longitude,
         dropoff_latitude,
         pickup_neighborhood,
         dropoff_neighborhood,
         fare)
rm(taxi_clean)

# declare rounding factors
pickup_rf <- .002
dropoff_rf <- .01

# round the trips
taxi <- taxi %>% 
  mutate(rounded_datetime = round_date_to(pickup_datetime),
         pickup_lat = round_to(pickup_latitude, pickup_rf),
         pickup_lng = round_to(pickup_longitude, pickup_rf),
         dropoff_lat = round_to(dropoff_latitude, dropoff_rf),
         dropoff_lng = round_to(dropoff_longitude,dropoff_rf)) %>% 
  select(rounded_datetime,
         pickup_lat,
         pickup_lng,
         dropoff_lat,
         dropoff_lng,
         pickup_neighborhood,
         dropoff_neighborhood,
         passenger_count,
         fare)


carpooling_potentials <- taxi %>% 
  filter(pickup_neighborhood != dropoff_neighborhood) %>%
  group_by(rounded_datetime, 
           pickup_lat, 
           pickup_lng, 
           dropoff_lat, 
           dropoff_lat) %>%
  summarize(num_trips = n(),
            num_psgrs = sum(passenger_count),
            total_fare = sum(fare)) %>%
  ungroup() %>%
  filter(num_trips > 1) %>%
  mutate(avg_fare = total_fare/num_trips,
         min_trips_needed = ceiling(num_psgrs/4),
         trip_savings = num_trips - min_trips_needed,
         fare_savings = avg_fare * trip_savings) %>%
  filter(min_trips_needed > 0)

# calculate savings
sum(carpooling_potentials$trip_savings)/nrow(taxi_clean)
sum(carpooling_potentials$fare_savings)/(sum(taxi_clean$fare_amount)+sum(taxi_clean$surcharge))
