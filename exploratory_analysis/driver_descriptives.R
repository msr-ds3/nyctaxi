library(ggplot2)
library(dplyr)
library(scales)

source("one_week_analysis.R")

# let's just use Jake's theme
theme_set(theme_bw())

# distribution of Total time driving
taxi_drivers_count <- taxi_clean  %>% group_by(hack_license) %>% summarize(timedriving = sum(trip_time_in_secs))
ggplot(taxi_drivers_count , aes(timedriving /60 / 60)) + geom_density() + xlab("hours") + geom_vline(xintercept = 40)

# distribution of Avg length/distance
taxi_drivers_Avg_length = taxi_clean %>% group_by(hack_license) %>% summarize(avg_distance = mean(trip_distance))
ggplot(taxi_drivers_Avg_length, aes(avg_distance)) + geom_density() + xlab("Avg Distance") + xlim(0,20)

# distribution of Avg fare 
taxi_drivers_Avg_fare = taxi_clean %>% group_by(hack_license) %>% summarize(avg_fare = mean(fare_amount))
ggplot(taxi_drivers_Avg_fare,  aes(avg_fare)) + geom_density() + xlab("Avg Fare") + xlim(0,60)


# distribution of Avg tip 
taxi_drivers_Avg_tip = taxi_clean %>% group_by(hack_license) %>% summarize(avg_tips = mean(tip_amount))
ggplot(taxi_drivers_Avg_tip,  aes(avg_tips)) + geom_density() + xlab("Avg Tip") + xlim(0,10)


# distribution of Total fare amount
taxi_drivers_total_fare = taxi_clean %>% group_by(hack_license) %>% summarize( TotalAmount = sum(total_amount))
ggplot(taxi_drivers_total_fare,  aes(TotalAmount)) + geom_density() + xlab("Total Amount") + xlim(0,4000) #+ scale_y_log10(label=comma)

# distribution of num trips 
taxi_drivers_numTrip = taxi_clean %>% group_by(hack_license) %>% summarize(numTrips = n())
ggplot(taxi_drivers_numTrip,  aes(numTrips)) + geom_density() + xlab("num Trip") + xlim(0,300)

# distribution of num medallion
taxi_drivers_medallion= taxi_clean %>% group_by(hack_license, medallion) %>% summarize(count= n()) %>% ungroup() %>% group_by(hack_license) %>% summarize(numCars= n())
ggplot(taxi_drivers_medallion,  aes(numCars)) + geom_density() + xlab("num medallion") + xlim(0,10)


###Time####
## number of drivers per hour 
taxi_drivers<- taxi_clean %>% group_by(hack_license, day_of_the_week, pickup_hour) %>% summarize(numTrips = n()) %>% ungroup() %>% group_by(day_of_the_week,pickup_hour) %>% summarize(numdriverss=n())
ggplot(taxi_drivers , aes(pickup_hour, numdriverss)) + geom_line() + xlab("Pickup Hour") + ylab("num drivers") + facet_grid(~ day_of_the_week)







