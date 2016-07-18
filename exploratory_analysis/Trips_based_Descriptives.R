library(ggplot2)
library(dplyr)


# let's just use Jake's theme
theme_set(theme_bw())

# distribution of trip duration
ggplot(taxi_clean, aes(trip_time_in_secs/60 )) + geom_density()+ xlab("trip time in minutes") + xlim(0,100)



# distribution of trip fare amount
ggplot(taxi_clean, aes(fare_amount)) + geom_density() + xlab("fare amount")  + xlim(0, 200)


# distribution of tips
ggplot(taxi_clean, aes(tip_amount)) + geom_density() + xlab("tip") + xlim(0, 15)


# distribution of distance
ggplot(taxi_clean, aes(trip_distance)) + geom_density() + xlab("Distance in miles") + xlim(0,50)


# Time
## Number of Trips:
ggplot(taxi_clean , aes(pickup_hour)) + geom_bar(width = .8) + xlab("Pick Up Hour") + ylab("Num Trips") + facet_grid(~ day_of_the_week)


# Group by day of the week and pickup hour:
taxi_day_of_week = taxi_clean %>% group_by(day_of_the_week, pickup_hour) %>% summarize(numTrips = n(), 
                                                                     avg_distance = mean(trip_distance), 
                                                                     avg_tip = mean(tip_amount),
                                                                     avg_fare = mean(fare_amount),
                                                                     avg_time = mean(trip_time_in_secs))

## Avg Time
ggplot(taxi_day_of_week , aes(pickup_hour, avg_time)) + geom_line() + xlab("Pick Up Hour") + ylab("Avg time") + facet_grid(~ day_of_the_week)
                                                                    
## Avg Distance
ggplot(taxi_day_of_week , aes(pickup_hour, avg_distance)) + geom_line() + xlab("Pick Up Hour") + ylab("Avg distance") + facet_grid(~ day_of_the_week)

## Avg Speed
ggplot(taxi_day_of_week , aes(pickup_hour, avg_distance / avg_time)) + geom_line() + xlab("Pick Up Hour") + ylab("Avg speed") + facet_grid(~ day_of_the_week)

## Avg Fare
ggplot(taxi_day_of_week , aes(pickup_hour, avg_fare)) + geom_line() + xlab("Pick Up Hour") + ylab("Avg fare") + facet_grid(~ day_of_the_week)

## Avg tip
ggplot(taxi_day_of_week , aes(pickup_hour, avg_tip)) + geom_line() + xlab("Pick Up Hour") + ylab("Avg tip") + facet_grid(~ day_of_the_week)



