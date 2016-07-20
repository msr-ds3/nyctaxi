load("one_week_taxi.Rdata")
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

theme_set(theme_minimal())

###########################################
#examine shifts, active hours, and downtime
###########################################
shifts <- taxi_clean %>% group_by(hack_license) %>% 
  arrange(pickup_datetime) %>% 
  mutate(downtime = (lead(pickup_datetime) - dropoff_datetime)/3600) 

###################################
#Creating the is_end_shift function
##################################


shifts_no_NA <- shifts %>% filter(!is.na(downtime))
is_end_shift_function = function(df, num)
{
  if ( num >= 6)
  {
    1
  }
  else
  {
    0
  }
}

is_end_shift_function = Vectorize(is_end_shift_function)
shifts_no_NA$is_end_shift = is_end_shift_function(shifts_no_NA, 
                                                  shifts_no_NA$downtime)

############################################
#Creating another col for the is_start_shift
############################################

shifts_no_NA <- shifts_no_NA %>% 
  mutate(is_start_shift = lag(is_end_shift, default = 1)) 

shifts_no_NA <- shifts_no_NA %>% mutate(index = cumsum(is_start_shift))

###############################
#Added revenue and shift length
###############################

shifts_clean <- shifts_no_NA %>% group_by(hack_license, index) %>% 
  summarize(start_shift = first(pickup_datetime), 
            end_shift = last(dropoff_datetime),
            revenue= sum(fare_amount),
            total_trip_duration = sum(trip_time_in_secs),
            active_hours =length(unique(c(pickup_hour, dropoff_hour))) ,
            total_trips = n(),
            total_distance = sum(trip_distance)
            ) %>%
  mutate(shift_length =  difftime(end_shift,start_shift, units = "hours"))



save(Shift_Interval, file = 'Shift_Interval.Rdata')



 


 