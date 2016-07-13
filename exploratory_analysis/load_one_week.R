library(readr)
library(dplyr)
library(broom)
library(lubridate)

parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}

#FILE DEPENDENCIES - TRIP_FARE_7 AND TRIP_DATA_7 CSVS
range_begin <- as.Date("2013-07-07")
range_end <- as.Date("2013-07-13")

trip_fare <- read_csv("trip_fare_7.csv")
trip_fare <- trip_fare %>% mutate( ymd_pickup = as.Date(parse_datetime(pickup_datetime, "%Y-%m-%d"))) %>% 
                           filter(as.Date(ymd_pickup) >= range_begin & as.Date(ymd_pickup) <= range_end)

trip_data <- read_csv("trip_data_7.csv")
trip_data <- trip_data %>% mutate( ymd_pickup = as.Date(parse_datetime(pickup_datetime, "%Y-%m-%d"))) %>% 
                           filter(as.Date(ymd_pickup) >= range_begin & as.Date(ymd_pickup) <= range_end)

taxi <- inner_join(trip_data, trip_fare, by=c("medallion", "hack_license", "vendor_id", "pickup_datetime", "ymd_pickup"))
taxi <- taxi %>% mutate(hour = hour(pickup_datetime),  days_of_the_week = wday(ymd_pickup,label = TRUE)) 

save(taxi, file = 'one_week_taxi.Rdata')




