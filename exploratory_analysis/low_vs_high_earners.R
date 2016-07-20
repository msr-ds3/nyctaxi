library(ggplot2)
library(dplyr)
library(tidyr)

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

# threshold - no less than 1.5 SD's less than mean of the number of trips 
min_num_of_trips = ceiling(mean(shifts$num_of_trips) - 
                             1.5*sd(shifts$num_of_trips))
#filter by threshold
shifts_filtered = shifts %>% filter(num_of_trips >= min_num_of_trips)

ggplot(shifts_filtered) +
  geom_density(aes(x=fare_per_active_hour)) + 
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,100))  + 
  xlab("Total Fare Amount Per Active Hour")
                                                                                                 
ggplot(shifts_filtered) + geom_density(aes(x=fare_per_total_trip_time))  + 
  scale_x_continuous(breaks = seq(0, 200, 10), limits = c(0,200)) +
  xlab("Total Fare Amount Per Hour Driven")

#HIGH VS. LOW EARNERS ANALYSIS
avg = mean(shifts_filtered$fare_per_active_hour)
sdv = sd(shifts_filtered$fare_per_active_hour)

source('map_visualization_functions.R')

low_earners = shifts_filtered %>% filter(fare_per_active_hour <= avg - sdv,
                                fare_per_active_hour >= avg - 2*sdv)
random = sample(1:nrow(low_earners), size = 1)
visualize_trips(taxi_clean, low_earners[random,]$hack_license, 
                low_earners[random,]$day_of_the_week)

high_earners = shifts_filtered %>% filter(fare_per_active_hour >= avg + sdv,
                                fare_per_active_hour <= avg + 2*sdv)
random = sample(1:nrow(high_earners), size = 1)
visualize_trips(taxi_clean, high_earners[random,]$hack_license, 
                high_earners[random,]$day_of_the_week) 
  









                                                                                        
