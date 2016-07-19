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
  mutate(downtime = (lead(pickup_datetime) - dropoff_datetime)/3600) %>% 
  select(pickup_datetime, dropoff_datetime, day_of_the_week, downtime)

###################################
#Creating the is_end_shift function
##################################


shifts_no_NA <- shifts %>% filter(!is.na(downtime))
is_end_shift_function = function(df, num)
{
  if ( num >= 6)
  {
    TRUE
  }
  else
  {
    FALSE
  }
}

is_end_shift_function = Vectorize(is_end_shift_function)
shifts_no_NA$is_end_shift = is_end_shift_function(shifts_no_NA, 
                                                  shifts_no_NA$downtime)

############################################
#Creating another col for the is_start_shift
############################################

shifts_no_NA <- shifts_no_NA %>% 
  mutate(is_start_shift = lag(is_end_shift),
         is_start_shift = ifelse(is.na(is_start_shift), TRUE, is_start_shift))

shifts_no_NA <- shifts_no_NA %>% 
  select (hack_license, pickup_datetime, dropoff_datetime,day_of_the_week,
          downtime, is_start_shift, is_end_shift)

###############################################
#Combining start and end shifts for each driver
##############################################

shift_interval <- shifts_no_NA %>% 
  filter(is_start_shift == TRUE | is_end_shift == TRUE)

shift_interval <- shift_interval %>% 
  ungroup() %>% 
  mutate(index = 1:nrow(.)) %>%
  mutate(group_on = index + (index %% 2)) %>% 
  group_by(hack_license,group_on) %>% 
  summarize(start_shift = first(pickup_datetime), 
            end_shift = last(dropoff_datetime))

shift_interval <- shift_interval %>% select(-group_on)

save(shift_interval, file = 'shift_interval.Rdata')