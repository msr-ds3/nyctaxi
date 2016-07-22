library(ggplot2)
library(dplyr)

source("driver_efficiency_data.R")

#show the driver's trips to user and let him/her guess
random_driver = sample(driver_efficiency$hack_license, 1)
visualize_trips_by_shift(taxi_clean_shifts, random_driver)

# ANSWER - run this to display answer #
low = low_earning_drivers %>% filter(hack_license == random_driver) %>% nrow() #low
high = high_earning_drivers %>% filter(hack_license == random_driver) %>% nrow() #high
if(low == 1) {
  print("Driver is a low-earner")
} else if(high ==1) {
  print("Driver is a high-earner")
} else {
  print("Driver is close to the average earner")
}
# END OF ANSWER #    

# Answer "explanation" - view driver's stats 
driver_efficiency %>% filter(hack_license == random_driver) %>% View()

#view stats for each shift
shifts_clean %>% filter(hack_license == random_driver) %>% 
  mutate(efficiency = fare/shift_length) %>% View()
