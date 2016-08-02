load("../Rdata/shifts.Rdata")
#creating model for shift efficiency
library(lubridate)
library(dplyr)
library(tidyr)


# set threshold for minimum length of shift
threshold <- round(mean(shifts_clean$shift_length) - sd(shifts_clean$shift_length))
thresholdMin = mean(shifts_clean$shift_length) - sd(shifts_clean$shift_length)
thresholdMax = mean(shifts_clean$shift_length) + 2*sd(shifts_clean$shift_length)


# find top n pickup neighborhoods
n_popular <- 20
n_unpopular <- 50
popular_pickup_neighborhoods <- taxi_clean_shifts %>%
  group_by(pickup_neighborhood) %>% 
  summarize(numtrips = n()) %>% 
  top_n(n_popular, numtrips)

# find top n  dropoff neighborhoods
popular_dropoff_neighborhoods <- taxi_clean_shifts %>%
  group_by(dropoff_neighborhood) %>% 
  summarize(numtrips = n()) %>% 
  top_n(n_popular, numtrips)

# find bottom n  dropoff neighborhoods
unpopular_pickup_neighborhoods <- taxi_clean_shifts %>%
  group_by(pickup_neighborhood) %>% 
  summarize(numtrips = n()) %>% 
  top_n(n= n_unpopular, wt = -numtrips)

unpopular_dropoff_neighborhoods <- taxi_clean_shifts %>%
  group_by(dropoff_neighborhood) %>% 
  summarize(numtrips = n()) %>% 
  top_n(n = n_unpopular, wt= -numtrips)


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


shift_period = function(time)
{
  night_shift = "night"
  day_shift = "day"
    
  if(hour(time) >= 5 && hour(time) < 17)
      day_shift   
  else
      night_shift 
}


# vectorize above functions
is_equal_to = Vectorize(is_equal_to)
is_in_popular_pickup_neighborhoods = Vectorize(is_in_popular_pickup_neighborhoods)
is_in_popular_dropoff_neighborhoods = Vectorize(is_in_popular_dropoff_neighborhoods)
is_to_airport = Vectorize(is_to_airport)
shift_period = Vectorize(shift_period)

# Add "counter" columns to allow us to sum up easily without grouping
taxi_clean_shifts = mutate(taxi_clean_shifts, 
                           is_popular_pickup_neighborhood =
                             ifelse(pickup_neighborhood %in% 
                                      popular_pickup_neighborhoods$pickup_neighborhood, 1, 0),
                           is_popular_dropoff_neighborhood = 
                             ifelse(dropoff_neighborhood %in% 
                                      popular_dropoff_neighborhoods$dropoff_neighborhood, 1, 0),
                           is_unpopular_pickup_neighborhood =
                             ifelse(pickup_neighborhood %in% 
                                      popular_pickup_neighborhoods$pickup_neighborhood, 1, 0),
                           is_unpopular_dropoff_neighborhood = 
                             ifelse(dropoff_neighborhood %in% 
                                      popular_dropoff_neighborhoods$dropoff_neighborhood, 1, 0),
                           is_rate_code_1 = ifelse(rate_code == 1, 1, 0),
                           is_rate_code_2 = ifelse(rate_code == 2, 1, 0),
                           is_rate_code_3 = ifelse(rate_code == 3, 1, 0),
                           is_rate_code_4 = ifelse(rate_code == 4, 1, 0),
                           is_rate_code_5 = ifelse(rate_code == 5, 1, 0),
                           is_rate_code_6 = ifelse(rate_code == 6, 1, 0),
                           is_man_pickup = ifelse(pickup_boroughCode == 1, 1, 0),
                           is_bronx_pickup = ifelse(pickup_boroughCode == 2, 1, 0),
                           is_bklyn_pickup = ifelse(pickup_boroughCode == 3, 1, 0),
                           is_queens_pickup = ifelse(pickup_boroughCode == 4, 1, 0),
                           is_si_pickup = ifelse(pickup_boroughCode == 5, 1, 0),
                           is_man_dropoff = ifelse(dropoff_boroughCode == 1, 1, 0),
                           is_bronx_dropoff = ifelse(dropoff_boroughCode == 2, 1, 0),
                           is_bklyn_dropoff = ifelse(dropoff_boroughCode == 3, 1, 0),
                           is_queens_dropoff = ifelse(dropoff_boroughCode == 4, 1, 0),
                           is_si_dropoff = ifelse(dropoff_boroughCode == 5, 1, 0),
                           is_to_airport = is_to_airport(dropoff_neighborhood, rate_code)
                          )

shifts_design_matrix = taxi_clean_shifts %>% ungroup() %>%
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
    rate_code_1_pct = sum(is_rate_code_1)/num_trips,
    rate_code_2_pct = sum(is_rate_code_2)/num_trips,
    rate_code_3_pct = sum(is_rate_code_3)/num_trips,
    rate_code_4_pct = sum(is_rate_code_4)/num_trips,
    rate_code_5_pct = sum(is_rate_code_5)/num_trips,
    rate_code_6_pct = sum(is_rate_code_6)/num_trips,
    avg_speed = total_trip_distance/total_trip_time,
    start_day = wday(as.POSIXct(start,
                                tz = "EDT", 
                                origin = origin),
                     label = T),
    occupancy_pct = total_trip_time/length,
    pickups_in_man_pct = sum(is_man_pickup)/num_trips,
    pickups_in_bronx_pct = sum(is_bronx_pickup)/num_trips,
    pickups_in_bklyn_pct = sum(is_bklyn_pickup)/num_trips,
    pickups_in_queens_pct = sum(is_queens_pickup)/num_trips,
    pickups_in_si_pct = sum(is_si_pickup)/num_trips,
    dropoffs_in_man_pct = sum(is_man_dropoff)/num_trips,
    dropoffs_in_bronx_pct = sum(is_bronx_dropoff)/num_trips,
    dropoffs_in_bklyn_pct = sum(is_bklyn_dropoff)/num_trips,
    dropoffs_in_queens_pct = sum(is_queens_dropoff)/num_trips,
    dropoffs_in_si_pct = sum(is_si_dropoff)/num_trips,
    popular_pickup_neighborhood_pct = 
      sum(is_popular_pickup_neighborhood)/num_trips,
    popular_dropoff_neighborhood_pct = 
      sum(is_popular_dropoff_neighborhood)/num_trips,
    unpopular_pickup_neighborhood_pct = 
      sum(is_unpopular_pickup_neighborhood)/num_trips,
    unpopular_dropoff_neighborhood_pct = 
      sum(is_unpopular_dropoff_neighborhood)/num_trips,
    airport_pct = sum(is_to_airport)/num_trips,
    efficiency = total_fare/length,
    shift_type = shift_period(as.POSIXct(start, tz="EDT", origin=origin))
  ) %>%
  filter(length >= thresholdMin , 
           length <= thresholdMax &
           start >= as.POSIXct("2013-07-01 06:00:00", tz = "EDT") & 
           end <= as.POSIXct("2013-07-31 18:00:00", tz = "EDT") & 
           efficiency < 100 )



#Adding a ymd column in the shifts design matrix data frame
shifts_design_matrix <- shifts_design_matrix %>% mutate(ymd = as.Date(start))

#Joining the weather data to shift design matrix
source("load_weather.R")
shifts_design_matrix<- left_join(shifts_design_matrix, weather, by ="ymd")

## add neighborhood pct as feature
pickup_neighborhood_features <- taxi_clean_shifts%>%
  ungroup() %>%
  group_by(hack_license, shift_num, pickup_neighborhood) %>%
  summarize(count = n()) %>%
  group_by(hack_license, shift_num) %>%
  mutate(total = sum(count), pct_in_pickup_nbhd = count/total) %>%
  spread(key = pickup_neighborhood,
         value = pct_in_pickup_nbhd,
         fill = 0,
         sep = "_") %>%
  select(-total, -count)

dropoff_neighborhood_features <- taxi_clean_shifts%>%
  ungroup() %>%
  group_by(hack_license, shift_num, dropoff_neighborhood) %>%
  summarize(count = n()) %>%
  group_by(hack_license, shift_num) %>%
  mutate(total = sum(count), pct_in_dropoff_nbhd = count/total) %>%
  spread(key = dropoff_neighborhood,
         value = pct_in_dropoff_nbhd,
         fill = 0,
         sep = "_") %>%
  select(-total, -count)
neighborhood_features <- inner_join(pickup_neighborhood_features, 
                                    dropoff_neighborhood_features,
                                    by=c("hack_license", "shift_num"))
names(neighborhood_features) <- sub(" ", "_", names(neighborhood_features))
shifts_design_matrix <- left_join(shifts_design_matrix,
                                  neighborhood_features,
                                  by=c("hack_license", "shift_num"))
##################################
#Added is_week_end and start_hour
#################################
is_weekend = function(vec)
{
  col = vector(mode= "numeric", length = length(vec))
  if (wday(vec) ==1 | wday(vec)==7)
  {
    TRUE
  }
  else
  {
    FALSE
  }
}
is_weekend = Vectorize(is_weekend)
shifts_design_matrix$is_week_end = is_weekend(shifts_design_matrix$ymd)

shifts_design_matrix <- shifts_design_matrix %>% mutate(start_hour = hour(start))

save(shifts_design_matrix, file= "../Rdata/shifts_design_matrix.Rdata")
