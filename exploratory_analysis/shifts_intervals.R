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
  mutate(downtime = difftime(lead(pickup_datetime), 
                             dropoff_datetime, 
                             units = "secs")/3600) 

###################################
#Creating the is_end_shift function
##################################


taxi_clean_shifts <- shifts %>% filter(!is.na(downtime))
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
taxi_clean_shifts$is_end_shift = is_end_shift_function(taxi_clean_shifts, 
                                                  taxi_clean_shifts$downtime)

############################################
#Creating another col for the is_start_shift
############################################

taxi_clean_shifts <- taxi_clean_shifts %>% 
  mutate(is_start_shift = lag(is_end_shift, default = 1)) 

taxi_clean_shifts <- taxi_clean_shifts %>% 
  mutate(index = cumsum(is_start_shift))

###############################
#Added fare and shift length
###############################

shifts_clean <- taxi_clean_shifts %>% group_by(hack_license, index) %>% 
  summarize(start_shift = first(pickup_datetime), 
            end_shift = last(dropoff_datetime),
            fare= sum(fare_amount),
            active_hours =length(unique(c(pickup_hour, dropoff_hour))) ,
            total_trips = n(),
            total_distance = sum(trip_distance), 
            total_duration_in_seconds = sum(trip_time_in_secs)
            ) %>%
  mutate(shift_length =  as.numeric(difftime(end_shift,start_shift, units = "hours")))

save(shifts_clean, taxi_clean_shifts, file = 'shifts.Rdata')






 