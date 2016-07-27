library(ggplot2)
library(dplyr)
library(tidyr)

load("../Rdata/shifts.Rdata")

driver_efficiency_no_threshold = shifts_clean %>%  
  group_by(hack_license) %>%
  summarize( 
    num_shifts = n(),
    fare_per_shift = sum(fare)/num_shifts,
    efficiency = sum(fare)/sum(shift_length), #note: this average is WEIGHTED, and is not the average of each shift's efficiency
    total_trips = sum(total_trips),
    trips_per_shift = total_trips/num_shifts
  ) %>% 
  filter(is.finite(efficiency)) #remove non-finite values where shift length is 0


# set thresholds
min_num_of_trips = ceiling(mean(driver_efficiency_no_threshold$trips_per_shift) - 
                             2*sd(driver_efficiency_no_threshold$trips_per_shift))
max_efficiency = 150

# apply threshold
driver_efficiency = driver_efficiency_no_threshold %>% 
  filter(trips_per_shift >= min_num_of_trips &
           efficiency <= max_efficiency) 
  


# LOW/HIGH EARNERS
avg = mean(driver_efficiency$efficiency)
sdv = sd(driver_efficiency$efficiency)
med = median(driver_efficiency$efficiency)

driver_efficiency = driver_efficiency %>%
  mutate(efficiency_category = 
         ifelse(efficiency <= avg - sdv, "LOW", 
                ifelse(efficiency >= avg + sdv, "HIGH", 
                       "MEDIUM")
                )
        )
driver_efficiency$efficiency_category = 
  as.factor(driver_efficiency$efficiency_category)
  

low_earning_drivers = driver_efficiency %>% 
  filter(efficiency_category == "LOW")

high_earning_drivers = driver_efficiency %>% 
  filter(efficiency_category == "HIGH")

driver_efficiency = driver_efficiency %>% 
  mutate(efficiency_binary = ifelse(efficiency <=med, 0, 1))


