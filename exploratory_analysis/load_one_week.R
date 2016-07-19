library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(httr)
library(rgdal)
library(broom)

#FILE DEPENDENCIES - TRIP_FARE_7 AND TRIP_DATA_7 CSVS

# range of dates we're interested in
range_begin <- as.Date("2013-07-07")
range_end <- as.Date("2013-07-13")

# read the files, add a ymd col and get the dates in our range
trip_fare <- read_csv("trip_fare_7.csv")
trip_fare <- trip_fare %>%
  mutate( ymd_pickup = as.Date(pickup_datetime) ) %>% 
  filter(ymd_pickup >= range_begin & ymd_pickup <= range_end)


trip_data <- read_csv("trip_data_7.csv")
trip_data <- trip_data %>%
  mutate( ymd_pickup = as.Date(pickup_datetime) ) %>% 
  filter(ymd_pickup >= range_begin & ymd_pickup <= range_end)

# join the two dataframes 
taxi <- inner_join(trip_data, trip_fare, by=c("medallion", "hack_license", "vendor_id", "pickup_datetime", "ymd_pickup"))
taxi <- taxi %>% mutate(pickup_hour = hour(pickup_datetime), dropoff_hour = hour(dropoff_datetime),day_of_the_week = wday(ymd_pickup,label = TRUE), 
                        pickup_minute = minute(pickup_datetime), minute = parse_minute(dropoff_datetime)) 

get_nyc_neighborhoods <- function(){
  r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
  return(readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F))
}

# get nyc boundaries
nyc_neighborhoods <- get_nyc_neighborhoods()

nyc_df <- tidy(nyc_neighborhoods)
min_lat <- min(nyc_df$lat)
max_lat <- max(nyc_df$lat)
min_lng <- min(nyc_df$long)
max_lng <- max(nyc_df$long)

# filter out pickups and dropoffs outside of nyc boundaries
taxi_clean <- filter(taxi,  pickup_longitude >= min_lng & pickup_longitude <= max_lat & pickup_latitude >= min_lat & pickup_latitude <= max_lat &
                       dropoff_longitude >= min_lng & dropoff_longitude <= max_lat & dropoff_latitude >= min_lat & dropoff_latitude <= max_lat)

# join the neighborhood info for pickup
spdf <- as.data.frame(taxi_clean)
coordinates(spdf) <- ~pickup_longitude +pickup_latitude
proj4string(spdf) <- proj4string(nyc_neighborhoods)
matches <- over(spdf, nyc_neighborhoods)
taxi_clean <- cbind(taxi_clean, matches)


# rename fields created for pickup neighborhoods
taxi_clean <- rename(taxi_clean, pickup_neighborhood = neighborhood,pickup_borough=borough, pickup_boroughCode = boroughCode, pickup_X.id = X.id)

#  join the neighborhood info for dropoff
spdf <- as.data.frame(taxi_clean)
coordinates(spdf) <- ~dropoff_longitude +dropoff_latitude
proj4string(spdf) <- proj4string(nyc_neighborhoods)
matches <- over(spdf, nyc_neighborhoods)
taxi_clean <- cbind(taxi_clean, matches)

# rename dropoff neighborhood field
taxi_clean <- rename(taxi_clean, dropoff_neighborhood = neighborhood,dropoff_borough=borough, dropoff_boroughCode = boroughCode, dropoff_X.id = X.id)

rm(spdf, nyc_df, min_lng, min_lat, max_lng, max_lat)

save(taxi_clean, file = 'one_week_taxi.Rdata')




