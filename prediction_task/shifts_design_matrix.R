load("../exploratory_analysis/shifts.Rdata")
#creating model for shift efficiency
library(lubridate)
library(dplyr)

# set threshold for minimum num of rides per shift
threshold <- round(mean(shifts_clean$total_trips) - sd(shifts_clean$total_trips))


# find top n pickup neighborhoods
n <- 20
popular_pickup_neighborhoods <- taxi_clean_shifts %>%
  group_by(pickup_neighborhood) %>% 
  summarize(numtrips = n()) %>% 
  top_n(n, numtrips)

# findtop n  dropoff neighborhoods
popular_dropoff_neighborhoods <- taxi_clean_shifts %>%
  group_by(dropoff_neighborhood) %>% 
  summarize(numtrips = n()) %>% 
  top_n(n, numtrips)

# returns 1 if this == that, otherwise 0
is_equal_to <- function(this, that)
{
  ifelse(this == that, 1, 0)
}

# returns 1 if neighborhood is in top n neighborhoods, otherwise 0
is_in_popular_pickup_neighborhoods <- function(neighborhood)
{
  ifelse(neighborhood %in% popular_pickup_neighborhoods$pickup_neighborhood, 
         1, 0)
}

is_in_popular_dropoff_neighborhoods <- function(neighborhood)
{
  ifelse(neighborhood %in% popular_dropoff_neighborhoods$dropoff_neighborhood, 
         1, 0)
}

is_to_airport <- function(dropoff_neighborhood, rate_code)
{
  airports <- c("John F. Kennedy International Airport", "LaGuardia Airport")
  airport_codes <- c(2, 3)
  if (dropoff_neighborhood %in% airports || rate_code %in% airport_codes)
    1
  else
    0
}

# vectorize above functions
is_equal_to = Vectorize(is_equal_to)
is_in_popular_pickup_neighborhoods = Vectorize(is_in_popular_pickup_neighborhoods)
is_in_popular_dropoff_neighborhoods = Vectorize(is_in_popular_dropoff_neighborhoods)
is_to_airport = Vectorize(is_to_airport)

shifts_design_matrix = taxi_clean_shifts %>% 
  group_by(hack_license, shift_num) %>%
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
    start_day = wday(as.POSIXct(start,
                                tz = "EDT", 
                                origin = origin),
                     label = T),
    occupancy_pct = total_trip_time/length,
    pickups_in_man_pct = sum(is_equal_to(pickup_boroughCode, 1))/num_trips,
    pickups_in_bronx_pct = sum(is_equal_to(pickup_boroughCode, 2))/num_trips,
    pickups_in_bklyn_pct = sum(is_equal_to(pickup_boroughCode, 3))/num_trips,
    pickups_in_queens_pct = sum(is_equal_to(pickup_boroughCode, 4))/num_trips,
    pickups_in_si_pct = sum(is_equal_to(pickup_boroughCode, 5))/num_trips,
    dropoffs_in_man_pct = sum(is_equal_to(dropoff_boroughCode, 1))/num_trips,
    dropoffs_in_bronx_pct = sum(is_equal_to(dropoff_boroughCode, 2))/num_trips,
    dropoffs_in_bklyn_pct = sum(is_equal_to(dropoff_boroughCode, 3))/num_trips,
    dropoffs_in_queens_pct = sum(is_equal_to(dropoff_boroughCode, 4))/num_trips,
    dropoffs_in_si_pct = sum(is_equal_to(dropoff_boroughCode, 5))/num_trips,
    popular_pickup_neighborhood_pct = 
      sum(is_in_popular_pickup_neighborhoods(pickup_neighborhood))/num_trips,
    popular_dropoff_neighborhood_pct = 
      sum(is_in_popular_dropoff_neighborhoods(dropoff_neighborhood))/num_trips,
    airport_pct = sum(is_to_airport(dropoff_neighborhood, rate_code))/num_trips,
    efficiency = total_fare/length
  ) %>%
  filter(num_trips >= threshold & 
           length <= 24 &
           #efficiency <= 75 &
           start >= as.POSIXct("2013-07-07 06:00:00", tz = "EDT") & 
           end <= as.POSIXct("2013-07-13 18:00:00", tz = "EDT")) 

save(shifts_design_matrix, "shifts_design_matrix.Rdata")