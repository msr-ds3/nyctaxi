library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(httr)
library(rgdal)
library(broom)

get_nyc_neighborhoods <- function(){
  r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
  return(readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F))
}
    

###########################################
# read in the files and create a data frame 
###########################################
load("one_week_taxi.Rdata")

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

# display stats by day of week, hour, pickup neighborhood, and dropoff neighborhood
trip_based_stats <- taxi_clean %>% group_by(day_of_the_week, hour, pickup_neighborhood, dropoff_neighborhood) %>% 
         summarize(count=n(), avg_distance = mean(trip_distance), first_quartile_dist = quantile(trip_distance,0.25), median_dist = median(trip_distance), 
                   third_quartile_dist = quantile(trip_distance, 0.75), max_dist = max(trip_distance), min_dist = min(trip_distance), sd_distance = sd(trip_distance),
                    avg_tip = mean(tip_amount), first_quartile_tip = quantile(tip_amount, 0.25), median_tip = median(tip_amount),
                    third_quartile_tip = quantile(tip_amount, 0.75), max_tip = max(tip_amount), min_tip = min(tip_amount), sd_tip = sd(tip_amount), 
                    avg_fare = mean(fare_amount), first_quartile_fare = quantile(fare_amount, 0.25), median_fare = median(fare_amount), 
                    third_quartile_fare = quantile(fare_amount, 0.75), max_fare = max(fare_amount), min_fare = min(fare_amount), sd_fare = sd(fare_amount),
                    avg_time_in_sec = mean(trip_time_in_secs), first_quartile = quantile(trip_time_in_secs, 0.25), median_time_in_sec = median(trip_time_in_secs), 
                    third_quartile = quantile(trip_time_in_secs, 0.75), max_time_in_sec = max(trip_time_in_secs), min_time_in_sec = min(trip_time_in_secs), sd_time_in_sec = sd(trip_time_in_secs))

  



