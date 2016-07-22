load(file = 'shifts.Rdata')
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
theme_set(theme_bw())
###############################################
#Plotting the active hours over the shift length
###############################################
shifts_clean_test <- shifts_clean %>% filter(shift_length != 0)
shift_length_vs_active_hours <-
  ggplot(shifts_clean_test, aes(x = shift_length, y = active_hours)) + 
  geom_point() + xlim(0,24) + 
  xlab("shift length in hours") +
  ylab("active hours")

ggsave("../figures/shift_length_vs_active_hours.png", 
       plot  = shift_length_vs_active_hours)

#################################################
# Number of shifts over active hours/shift length
#################################################
distribution_of_the_active_hours_over_the_shift_length <- 
  ggplot(shifts_clean_test,
       aes(x = active_hours/shift_length)) + 
  geom_density() + xlim(0,2) + 
xlab("active hours over shift length (in hours)")

ggsave('../figures/distribution_of_the _active_hours_over_the_shift_length.png', 
       plot = distribution_of_the_active_hours_over_the_shift_length )
  
##################################################################
#Plotting the number of shifts over the trip duration/shift length
##################################################################
distribution_of_the_total_trip_duration_over_the_shift_length <- 
  ggplot(shifts_clean_test,
         aes(x = (total_duration_in_seconds/3600) /shift_length)) + 
  geom_density() + scale_x_continuous(label = percent) + xlim(0,1.25) +  
  xlab("total trip duration over the shift length") 
      

ggsave('../figures/distribution_of_the_total_trip_duration_over_the_shift_length.png',
       plot = distribution_of_the_total_trip_duration_over_the_shift_length)

####################################################
#Plotting the average shift duration for each driver
####################################################
shifts_clean_by_driver <- shifts_clean_test %>% group_by(hack_license) %>%
  summarize(percent_time_occupied = 
              sum(total_duration_in_seconds/3600)/sum(shift_length))

distribution_of_the_percent_time_occupied <- ggplot(shifts_clean_by_driver, 
       aes(x = percent_time_occupied)) + geom_density() + 
  xlab("percent time occupied")

ggsave('../figures/distribution_of_the_percent_time_occupied.png', 
       plot =distribution_of_the_percent_time_occupied )

##################################################
#Plotting the distribution of the shift efficiency
##################################################
 shifts_clean_test <- shifts_clean_test %>% 
   mutate(shift_efficiency = fare / shift_length)

 distribution_of_shift_efficiency <- 
   ggplot(shifts_clean_test, aes(x = shift_efficiency)) + geom_density() +
   xlab("shift efficiency") + xlim(0,100)
 
 ggsave('../figures/distribution_of_shift_efficiency.png',
        plot = distribution_of_shift_efficiency)

