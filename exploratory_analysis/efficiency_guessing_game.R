library(ggplot2)
library(dplyr)

source("driver_efficiency_data.R")
source("shift_efficiency_data.R")

#################################
########### PART 1 ##############
#################################

#show a random driver's shifts to user and let him/her guess efficiency
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



#################################
########### PART 2 ##############
#################################

# View random shift, and guess whether it is efficient or not
random_row = sample(1:nrow(shift_efficiency), 1)
random_driver = shift_efficiency[random_row, ]$hack_license
random_driver_shift = shift_efficiency[random_row, ]$shift_num

visualize_trips_by_shift(taxi_clean_shifts,random_driver, random_driver_shift)

# ANSWER - run this to display answer #
low = shift_efficiency %>% 
  filter(hack_license == random_driver & efficiency_category == "LOW") %>%
  nrow() 
high =  shift_efficiency %>% 
  filter(hack_license == random_driver & efficiency_category == "HIGH") %>%
  nrow() 

if(low == 1) {
  print("Driver is a low-earner")
} else if(high ==1) {
  print("Driver is a high-earner")
} else {
  print("Driver is close to the average earner")
}
# END OF ANSWER #    

# answer explanation - view shift's statistics
shift_efficiency[random_row, ] %>% View()
