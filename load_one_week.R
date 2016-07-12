library(readr)
library(dplyr)

parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}

range_begin <- as.Date("2013-07-06")
range_end <- as.Date("2013-07-14")

trip_fare <- read_csv("trip_fare_7.csv")
trip_fare <- trip_fare %>% mutate( ymd_pickup = as.Date(parse_datetime(pickup_datetime, "%Y-%m-%d %H:%M:%S"))) %>% 
                           filter(as.Date(ymd_pickup) > range_begin & as.Date(ymd_pickup) < range_end)

write_csv(fare_data, "trip_fare_2013_07_05-11.csv")


trip_data <- read_csv("trip_data_7.csv")
trip_data <- trip_data %>% mutate( ymd_pickup = as.Date(parse_datetime(pickup_datetime, "%Y-%m-%d %H:%M:%S"))) %>% 
                           filter(as.Date(ymd_pickup) > range_begin & as.Date(ymd_pickup) < range_end)
write_csv(trip_data, "trip_data_2013_07_05-11.csv")
