library(ggplot2)
library(dplyr)
library(tidyr)
#library(scales)

load('one_week_taxi.Rdata')

#Since gather doubles our observations below, we divide our sums by 2
#We convert seconds to hours for total_trip_time

shifts = taxi_clean %>% 
  gather(key = "type", value = "timestamp", pickup_hour, dropoff_hour) %>%
  group_by(hack_license, day_of_the_week) %>%  
  summarize(
            first = min(pickup_datetime), 
            last = max(pickup_datetime), 
            active_hours = n_distinct(timestamp), 
            num_of_trips = n()/2,                             
            total_revenue = sum(total_amount)/2,                    
            total_trip_time = sum(trip_time_in_secs)/2/60/60, 
            total_fare_amount = sum(fare_amount)/2,
            fare_per_active_hour = total_fare_amount/active_hours,
            fare_per_total_trip_time = total_fare_amount/total_trip_time,
            total_revenue_per_active_hour = total_revenue/active_hours,
            total_revenue_per_total_trip_time = total_revenue/total_trip_time
            )

# add threshold for minimum number of trip time and hours (to avoid divide by 0 
# AND to have more reliable data)

ggplot(shifts) + 
  geom_density(aes(x=total_revenue_per_active_hour)) + 
  xlim(0,100) +
  xlab("Total Revenue Per Active Hour")

ggplot(shifts) + 
  geom_density(aes(x=total_revenue_per_total_trip_time)) + 
  xlim(0,200)+ 
  xlab("Total Revenue Per Hour Driven")

ggplot(shifts) +
  geom_density(aes(x=fare_per_active_hour)) + 
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100))  + 
  xlab("Total Fare Amount Per Active Hour")
                                                                                                 
ggplot(shifts) + geom_density(aes(x=fare_per_total_trip_time))  + 
  scale_x_continuous(breaks = seq(0, 200, 10), limits = c(0,200)) +
  xlab("Total Fare Amount Per Hour Driven")

#HIGH VS. LOW EARNERS ANALYSIS
avg = mean(shifts$fare_per_active_hour)
sdv = sd(shifts$fare_per_active_hour)

source('map_visualization_functions.R')

low_earners = shifts %>% filter(fare_per_active_hour <= avg - sdv,
                                fare_per_active_hour >= avg - 2*sdv)
random = sample(1:nrow(low_earners), size = 1)
visualize_trips(taxi_clean, low_earners[random,]$hack_license, 
                low_earners[random,]$day_of_the_week)

high_earners = shifts %>% filter(fare_per_active_hour >= avg + sdv,
                                fare_per_active_hour <= avg + 2*sdv)
random = sample(1:nrow(high_earners), size = 1)
visualize_trips(taxi_clean, high_earners[random,]$hack_license, 
                high_earners[random,]$day_of_the_week) 
  









                                                                                        
