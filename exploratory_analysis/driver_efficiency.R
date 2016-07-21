library(ggplot2)
library(dplyr)
library(tidyr)

load("shifts.Rdata")

driver_efficiency = shifts_clean %>%  
  group_by(hack_license) %>%
  summarize( 
    num_shifts = n(),
    fare_per_shift = sum(fare)/num_shifts,
    fare_per_shift_length = sum(fare)/sum(as.numeric(shift_length)), #note: this average is WEIGHTED, and is not the average of each shift's efficiency
    total_trips = sum(total_trips),
    trips_per_shift = total_trips/num_shifts
  ) %>% 
  filter(is.finite(fare_per_shift_length)) #remove non-finite values where shift length is 0


#look at driver efficiency distributions
ggplot(data = driver_efficiency) + 
  geom_density(aes(x=fare_per_shift)) + 
  xlim(0,500) +
  xlab("Fare per shift ($/shift)") +
  geom_vline(xintercept = mean(driver_efficiency$fare_per_shift)) 


ggplot(data = driver_efficiency) + 
  geom_density(aes(x=fare_per_shift_length)) + 
  xlim(0,50) +
  xlab("Fare per shift hour ($/hour)") +
  geom_vline(xintercept = mean(driver_efficiency$fare_per_shift_length))

# Set a threshold - minimum trips no less than 2 SD's away from the mean  
min_num_of_trips = ceiling(mean(driver_efficiency$trips_per_shift) - 
                             2*sd(driver_efficiency$trips_per_shift))

#apply threshold
drivers_filtered = driver_efficiency %>% filter(trips_per_shift >= min_num_of_trips)

#look at plots after applying threshold - graph more normally distributed
ggplot(drivers_filtered) +
  geom_density(aes(x=fare_per_shift)) + 
  xlim(0,500) +
  geom_vline(xintercept = mean(drivers_filtered$fare_per_shift))

ggplot(drivers_filtered) +
  geom_density(aes(x=fare_per_shift_length)) + 
  xlim(0,50) +
  geom_vline(xintercept = mean(drivers_filtered$fare_per_shift_length))

# HIGH VS. LOW EARNERS ANALYSIS 
avg = mean(drivers_filtered$fare_per_shift_length)
sdv = sd(drivers_filtered$fare_per_shift_length)

source('map_visualization_functions.R')

# LOW EARNERS
low_earners = drivers_filtered %>% filter(fare_per_shift_length <= avg - sdv)

random_driver = sample(low_earners$hack_license, 1)

random_driver_trips = taxi_clean_shifts %>% 
  filter(hack_license == random_driver)

visualize_trips_by_shift(random_driver_trips, random_driver)
#based on faceted plot, we can add argument shift = shift_num to view particular shift


# HIGH EARNERS
high_earners = drivers_filtered %>% filter(fare_per_shift_length >= avg + sdv)

random_driver = sample(high_earners$hack_license, 1)

random_driver_trips = taxi_clean_shifts %>% 
  filter(hack_license == random_driver)

visualize_trips_by_shift(random_driver_trips, random_driver)
#based on faceted plot, we can add argument shift = shift_num to view particular shift


#view random driver's stats
driver_efficiency %>% filter(hack_license == random_driver) %>% View()


#GUESSING GAME
#get completely random driver - and guess whether he is high or low or avg earner
random_driver = sample(drivers_filtered$hack_license, 1)
visualize_trips_by_shift(taxi_clean_shifts, random_driver)
low_earners %>% filter(hack_license == random_driver) %>% nrow() #low
high_earners %>% filter(hack_license == random_driver) %>% nrow() #high
driver_efficiency %>% filter(hack_license == random_driver) %>% View()
shifts_clean %>% filter(hack_license == random_driver) %>% 
  mutate(efficiency = fare/shift_length) %>% View()
