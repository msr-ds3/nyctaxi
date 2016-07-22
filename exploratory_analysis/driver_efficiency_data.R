library(ggplot2)
library(dplyr)
library(tidyr)

load("shifts.Rdata")
source('map_visualization_functions.R')

driver_efficiency_no_threshold = shifts_clean %>%  
  group_by(hack_license) %>%
  summarize( 
    num_shifts = n(),
    fare_per_shift = sum(fare)/num_shifts,
    fare_per_shift_length = sum(fare)/sum(shift_length), #note: this average is WEIGHTED, and is not the average of each shift's efficiency
    total_trips = sum(total_trips),
    trips_per_shift = total_trips/num_shifts
  ) %>% 
  filter(is.finite(fare_per_shift_length)) #remove non-finite values where shift length is 0


# determine threshold for minimum trips (no less than 2 SD's away from the mean)
min_num_of_trips = ceiling(mean(driver_efficiency_no_threshold$trips_per_shift) - 
                             2*sd(driver_efficiency_no_threshold$trips_per_shift))

#apply threshold
driver_efficiency = driver_efficiency_no_threshold %>% 
  filter(trips_per_shift >= min_num_of_trips)


# LOW/HIGH EARNERS
avg = mean(driver_efficiency$fare_per_shift_length)
sdv = sd(driver_efficiency$fare_per_shift_length)

low_earning_drivers = driver_efficiency %>% 
  filter(fare_per_shift_length <= avg - sdv)

high_earning_drivers = driver_efficiency %>% 
  filter(fare_per_shift_length >= avg + sdv)


