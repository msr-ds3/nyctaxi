load("../exploratory_analysis/shifts.Rdata")
#creating model for shift efficiency
library(lubridate)
library(dplyr)

threshold <- round(mean(shifts_clean$total_trips) - sd(shifts_clean$total_trips))

is_equal_to <- function(this, that){
  ifelse(this == that, 1, 0)
}

is_equal_to = Vectorize(is_equal_to)

shifts_design_matrix = taxi_clean_shifts %>% 
  group_by(hack_license, shift_num) %>%
  #arrange(pickup_datetime) %>%
  summarize(
    start = as.POSIXct(min(pickup_datetime), tz="EDT"),
    end = as.POSIXct(max(dropoff_datetime), tz= "EDT"),
    length = (end - start)/3600,
    total_fare = sum(fare_amount),
    num_trips = n(),
    total_trip_distance = sum(trip_distance),
    avg_trip_distance = mean(trip_distance),
    sd_trip_distance = sd(trip_distance),
    total_trip_time = sum(trip_time_in_secs)/3600,
    avg_trip_time = mean(trip_time_in_secs)/3600,
    sd_trip_time = sd(trip_time_in_secs)/3600,
    rate_code_1_pct = sum(is_equal_to(rate_code, 1))/num_trips,
    rate_code_2_pct = sum(is_equal_to(rate_code, 2))/num_trips,
    rate_code_3_pct = sum(is_equal_to(rate_code, 3))/num_trips,
    rate_code_4_pct = sum(is_equal_to(rate_code, 4))/num_trips,
    rate_code_5_pct = sum(is_equal_to(rate_code, 5))/num_trips,
    rate_code_6_pct = sum(is_equal_to(rate_code, 6))/num_trips,
    avg_speed = total_trip_distance/total_trip_time,
    start_day = wday(start, label = T),
    occupancy_pct = total_trip_time/length,
    pickups_in_man_pct = sum(is_equal_to(pickup_boroughCode, 1))/num_trips,
    pickups_in_bronx_pct = sum(is_equal_to(pickup_boroughCode, 2))/num_trips,
    pickups_in_bklyn_pct = sum(is_equal_to(pickup_boroughCode, 3))/num_trips,
    pickups_in_queens_pct = sum(is_equal_to(pickup_boroughCode, 4))/num_trips,
    pickups_in_si_pct = sum(is_equal_to(pickup_boroughCode, 5))/num_trips,
    efficiency = total_fare/length
  ) %>%
  filter(num_trips >= threshold & 
           length <= 24 &
           efficiency <= 75 &
           start >= as.POSIXct("2013-07-07 06:00:00", tz = "EDT") & 
           end <= as.POSIXct("2013-07-13 18:00:00", tz = "EDT")) 