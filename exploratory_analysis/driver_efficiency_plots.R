library(ggplot2)
library(dplyr)

source("driver_efficiency_data.R")


# Look at driver efficiency distributions BEFORE THRESHOLD
ggplot(data = driver_efficiency_no_threshold) + 
  geom_density(aes(x=fare_per_shift)) + 
  xlim(0,500) +
  xlab("Fare per shift ($/shift)") +
  geom_vline(xintercept = mean(driver_efficiency_no_threshold$fare_per_shift)) 


ggplot(data = driver_efficiency_no_threshold) + 
  geom_density(aes(x=fare_per_shift_length)) + 
  xlim(0,50) +
  xlab("Fare per shift hour ($/hour)") +
  geom_vline(xintercept = mean(driver_efficiency_no_threshold$fare_per_shift_length))


# Look at driver efficiency WITH THRESHOLD 
ggplot(driver_efficiency) +
  geom_density(aes(x=fare_per_shift)) + 
  xlim(0,500) +
  geom_vline(xintercept = mean(driver_efficiency$fare_per_shift))

ggplot(driver_efficiency) +
  geom_density(aes(x=fare_per_shift_length)) + 
  xlim(0,50) +
  geom_vline(xintercept = mean(driver_efficiency$fare_per_shift_length))


# View random low-earning driver
random_driver = sample(low_earning_drivers$hack_license, 1)
random_driver_trips = taxi_clean_shifts %>% 
  filter(hack_license == random_driver)

visualize_trips_by_shift(random_driver_trips, random_driver)


# View random high-earning driver
random_driver = sample(high_earning_drivers$hack_license, 1)
random_driver_trips = taxi_clean_shifts %>% 
  filter(hack_license == random_driver)

visualize_trips_by_shift(random_driver_trips, random_driver)
