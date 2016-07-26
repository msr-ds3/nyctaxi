library(ggplot2)
library(dplyr)

source("driver_efficiency_data.R")
source('map_visualization_functions.R')


# Look at driver efficiency distributions BEFORE THRESHOLD
ggplot(data = driver_efficiency_no_threshold) + 
  geom_density(aes(x=fare_per_shift)) + 
  xlim(0,500) +
  xlab("Fare per shift ($/shift)") +
  geom_vline(xintercept = mean(driver_efficiency_no_threshold$fare_per_shift)) 


ggplot(data = driver_efficiency_no_threshold) + 
  geom_density(aes(x=efficiency)) + 
  xlim(0,50) +
  xlab("Fare per shift hour ($/hour)") +
  geom_vline(xintercept = mean(driver_efficiency_no_threshold$efficiency))


# Look at driver efficiency WITH THRESHOLD 
ggplot(driver_efficiency) +
  geom_density(aes(x=fare_per_shift)) + 
  xlim(0,500) +
  geom_vline(xintercept = mean(driver_efficiency$fare_per_shift))
ggsave("../figures/driver_fare_per_shift_distribution.png")

ggplot(driver_efficiency) +
  geom_density(aes(x=efficiency)) + 
  xlim(0,50) +
  geom_vline(xintercept = mean(driver_efficiency$efficiency))
ggsave("../figures/driver_efficiency_distribution.png")

# View random low-earning driver
random_driver = sample(low_earning_drivers$hack_license, 1)
visualize_trips_by_shift(taxi_clean_shifts, random_driver)
ggsave("../figures/low_earning_driver_random.png")


# View random high-earning driver
random_driver = sample(high_earning_drivers$hack_license, 1)
visualize_trips_by_shift(taxi_clean_shifts, random_driver)
ggsave("../figures/high_earning_driver_random.png")
