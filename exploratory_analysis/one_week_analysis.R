library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(httr)
library(rgdal)
library(broom)
    

####################################################################
# load base data frame 'taxi_clean' which contains neighborhood info
####################################################################
load("one_week_taxi.Rdata")

# display stats by day of week, pickup_hour, pickup neighborhood, and dropoff neighborhood
trip_based_stats <- taxi_clean %>% group_by(day_of_the_week, pickup_hour, pickup_neighborhood, dropoff_neighborhood) %>% 
         summarize(count=n(), avg_distance = mean(trip_distance), first_quartile_dist = quantile(trip_distance,0.25), median_dist = median(trip_distance), 
                   third_quartile_dist = quantile(trip_distance, 0.75), max_dist = max(trip_distance), min_dist = min(trip_distance), sd_distance = sd(trip_distance),
                    avg_tip = mean(tip_amount), first_quartile_tip = quantile(tip_amount, 0.25), median_tip = median(tip_amount),
                    third_quartile_tip = quantile(tip_amount, 0.75), max_tip = max(tip_amount), min_tip = min(tip_amount), sd_tip = sd(tip_amount), 
                    avg_fare = mean(fare_amount), first_quartile_fare = quantile(fare_amount, 0.25), median_fare = median(fare_amount), 
                    third_quartile_fare = quantile(fare_amount, 0.75), max_fare = max(fare_amount), min_fare = min(fare_amount), sd_fare = sd(fare_amount),
                    avg_time_in_sec = mean(trip_time_in_secs), first_quartile = quantile(trip_time_in_secs, 0.25), median_time_in_sec = median(trip_time_in_secs), 
                    third_quartile = quantile(trip_time_in_secs, 0.75), max_time_in_sec = max(trip_time_in_secs), min_time_in_sec = min(trip_time_in_secs), sd_time_in_sec = sd(trip_time_in_secs)) %>%
  ungroup()

  



