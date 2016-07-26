load("../Rdata/one_month_taxi.Rdata")

library(tidyr)
library(dplyr)


###########################################
#examine shifts, active hours, and downtime
###########################################
taxi_clean_shifts <- taxi_clean %>% group_by(hack_license) %>% 
  arrange(pickup_datetime) %>% 
  mutate(downtime = as.numeric(difftime(lead(pickup_datetime), 
                             dropoff_datetime, 
                             units = "hours")))

###################################
#Creating the is_end_shift function
##################################


taxi_clean_shifts <- taxi_clean_shifts %>% filter(!is.na(downtime))
is_end_shift_function = function(num)
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
taxi_clean_shifts <- taxi_clean_shifts %>% 
  mutate(is_end_shift = is_end_shift_function(downtime))

############################################
#Creating another col for the is_start_shift
############################################

taxi_clean_shifts <- taxi_clean_shifts %>% 
  mutate(is_start_shift = lag(is_end_shift, default = 1)) 

taxi_clean_shifts <- taxi_clean_shifts %>% 
  mutate(shift_num = cumsum(is_start_shift))

############################################
#replace NA's in borough, neighborhood info
############################################

out_of_bounds = "Outside NYC"
out_of_bounds_code = 0

# Cleaning pickup borough
taxi_clean_shifts$pickup_borough = 
  as.character(taxi_clean_shifts$pickup_borough)
taxi_clean_shifts$pickup_borough = 
  replace(taxi_clean_shifts$pickup_borough, 
          is.na(taxi_clean_shifts$pickup_borough), out_of_bounds)
taxi_clean_shifts$pickup_borough =
  as.factor(taxi_clean_shifts$pickup_borough)

# Cleaning pickup borough codes
taxi_clean_shifts$pickup_boroughCode = 
  as.numeric(taxi_clean_shifts$pickup_boroughCode)
taxi_clean_shifts$pickup_boroughCode = 
  replace(taxi_clean_shifts$pickup_boroughCode, 
          is.na(taxi_clean_shifts$pickup_boroughCode), out_of_bounds_code)
taxi_clean_shifts$pickup_boroughCode =
  as.factor(taxi_clean_shifts$pickup_boroughCode)

# Cleaning dropoff borough
taxi_clean_shifts$dropoff_borough = 
  as.character(taxi_clean_shifts$dropoff_borough)
taxi_clean_shifts$dropoff_borough = 
  replace(taxi_clean_shifts$dropoff_borough, 
          is.na(taxi_clean_shifts$dropoff_borough), out_of_bounds)
taxi_clean_shifts$dropoff_borough =
  as.factor(taxi_clean_shifts$dropoff_borough)


# Cleaning dropoff borough codes
taxi_clean_shifts$dropoff_boroughCode = 
  as.numeric(taxi_clean_shifts$dropoff_boroughCode)
taxi_clean_shifts$dropoff_boroughCode = 
  replace(taxi_clean_shifts$dropoff_boroughCode, 
          is.na(taxi_clean_shifts$dropoff_boroughCode), out_of_bounds_code)
taxi_clean_shifts$dropoff_boroughCode = as.factor(taxi_clean_shifts$dropoff_boroughCode)

# Cleaning up dropoff and pickup neighborhoods 
taxi_clean_shifts$pickup_neighborhood = 
  as.character(taxi_clean_shifts$pickup_neighborhood)
taxi_clean_shifts$pickup_neighborhood = 
  replace(taxi_clean_shifts$pickup_neighborhood, 
          is.na(taxi_clean_shifts$pickup_neighborhood), out_of_bounds)
taxi_clean_shifts$pickup_neighborhood =
  as.factor(taxi_clean_shifts$pickup_neighborhood)

taxi_clean_shifts$dropoff_neighborhood = 
  as.character(taxi_clean_shifts$dropoff_neighborhood)
taxi_clean_shifts$dropoff_neighborhood = 
  replace(taxi_clean_shifts$dropoff_neighborhood, 
          is.na(taxi_clean_shifts$dropoff_neighborhood), out_of_bounds)
taxi_clean_shifts$dropoff_neighborhood =
  as.factor(taxi_clean_shifts$dropoff_neighborhood)



###############################
#Added fare and shift length
###############################

shifts_clean <- taxi_clean_shifts %>% group_by(hack_license, shift_num) %>% 
  summarize(start_shift = first(pickup_datetime), 
            end_shift = last(dropoff_datetime),
            fare= sum(fare_amount),
            active_hours =length(unique(c(pickup_hour, dropoff_hour))) ,
            total_trips = n(),
            total_distance = sum(trip_distance), 
            total_duration_in_seconds = sum(trip_time_in_secs)
            ) %>%
  mutate(shift_length = as.numeric(difftime(end_shift, 
                                            start_shift, 
                                            units = "hours")))

save(shifts_clean, taxi_clean_shifts, file = "../Rdata/shifts.Rdata")






 