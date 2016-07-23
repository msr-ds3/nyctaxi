load("shifts.Rdata")
#creating model for shift efficiency
library(lubridate)
library(dplyr)

is_rate_code <- function(rate_code, code){
  if (rate_code == code){
    1
  } else {
    0
  }
}
is_rate_code = Vectorize(is_rate_code)

shifts_design_matrix = taxi_clean_shifts %>% 
  group_by(hack_license, shift_num) %>%
  #arrange(pickup_datetime) %>%
  summarize(
    shift_start = min(pickup_datetime),
    shift_end = max(dropoff_datetime),
    
    total_fare = sum(fare_amount),
    num_trips = n(),
    total_trip_distance = sum(trip_distance),
    avg_trip_distance = mean(trip_distance),
    sd_trip_distance = sd(trip_distance),
    total_trip_time = sum(trip_time_in_secs)/3600,
    avg_trip_time = mean(trip_time_in_secs)/3600,
    sd_trip_time = sd(trip_time_in_secs)/3600,
    rate_code_1_pct = sum(is_rate_code(rate_code, 1))/num_trips,
    rate_code_2_pct = sum(is_rate_code(rate_code, 2))/num_trips,
    rate_code_3_pct = sum(is_rate_code(rate_code, 3))/num_trips,
    rate_code_4_pct = sum(is_rate_code(rate_code, 4))/num_trips,
    rate_code_5_pct = sum(is_rate_code(rate_code, 5))/num_trips,
    rate_code_6_pct = sum(is_rate_code(rate_code, 6))/num_trips,
    avg_speed = total_trip_distance/total_trip_time,
    shift_start_day = wday(shift_start, label = T)
    
  ) %>%
  mutate(shift_length = as.numeric(difftime(shift_end, shift_start, units = "hours")),
         occupancy_pct = total_trip_time/shift_length,
         efficiency = total_fare/shift_length) %>%
  filter(total_trips >= threshold &
                  shift_length <= 24 &
                  shift_efficiency <= 75 &
                  start_shift >= as.POSIXct("2013-07-07 02:00:00") & 
                  end_shift <= as.POSIXct("2013-07-13 14:00:00")) #using UTC offset to limit dataframe to full shifts  
