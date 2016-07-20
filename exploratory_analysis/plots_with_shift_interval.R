load(file = 'shifts.Rdata')
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

###############################################
#Plotting the active hours over the shift length
###############################################
shifts_clean_test <- shifts_clean %>% filter(shift_length != 0)
ggplot(shifts_clean_test, aes(x = shift_length, y = active_hours)) + 
  geom_point() + xlim(0,24)

#################################################
# Number of shifts over active hours/shift length
#################################################
ggplot(shifts_clean_test,
       aes(x = active_hours/as.numeric(shift_length))) + 
  geom_density() + xlim(0,2)

#################################################################
#Plotting the number of shifts over the trip duration/shift length
#################################################################
ggplot(shifts_clean_test, 
       aes(x = (total_trip_duration/3600) / as.numeric(shift_length))) + 
  geom_density() + scale_x_continuous(label = percent)

####################################################
#Plotting the average shift duration for each driver
####################################################
shifts_clean_by_driver <- shifts_clean_test %>% group_by(hack_license) %>%
  summarize(percent_time_occupied = 
              sum(total_trip_duration/3600)/sum(as.numeric(shift_length)))

ggplot(shifts_clean_by_driver, aes(x = percent_time_occupied)) + geom_density()




