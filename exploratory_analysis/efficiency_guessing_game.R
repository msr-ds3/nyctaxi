###########################################
### INSTRUCTIONS: Run question, answer, and
### then answer explanation for each part 

library(ggplot2)
library(dplyr)

source("driver_efficiency_data.R")
source("shift_efficiency_data.R")
source("map_visualization_functions.R")

######################################
## PART 1 - Guess Driver efficiency ##
######################################

# QUESTION #
random_row = sample(1:nrow(driver_efficiency),1)
random_driver = driver_efficiency[random_row, ]$hack_license[1]
visualize_trips_by_shift(taxi_clean_shifts, random_driver)
# END OF QUESTION #

# ANSWER  #
answer = as.character(driver_efficiency[random_row, ]$efficiency_category)
print(paste("Driver is a ", answer, " driver"))
# END OF ANSWER #    

# ANSWER EXPLANATION (view driver's stats) # 
driver_efficiency %>% 
  filter(hack_license == random_driver) %>% 
  View()
# END OF ANSWER EXPLANATION #

# DETAILED EXPLANATION (view stats for all of driver's shifts) #
shifts_clean %>% 
  filter(hack_license == random_driver) %>% 
  mutate(efficiency = fare/shift_length) %>% 
  View()
# END OF DETAILED EXPLANATION #



######################################
## PART 2 - Guess Shifts efficiency ##
######################################

# QUESTION #
random_row = sample(1:nrow(shift_efficiency), 1)
random_driver = shift_efficiency[random_row, ]$hack_license
random_driver_shift = shift_efficiency[random_row, ]$shift_num
visualize_trips_by_shift(taxi_clean_shifts,random_driver, random_driver_shift)
# END OF QUESTION #


# ANSWER #
answer = as.character(shift_efficiency[random_row, ]$efficiency_category)
print(paste("Driver is a ", answer, " driver"))
# END OF ANSWER #    

# ANSWER EXPLANATION #
shift_efficiency[random_row, ] %>% 
  View()
# END OF ANSWER EXPLANATION #
