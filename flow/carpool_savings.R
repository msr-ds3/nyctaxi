load("../Rdata/one_month_taxi.Rdata")
library(lubridate)
library(dplyr)
library(gganimate)
library(ggplot2)
library(ggmap)
library(scales)
library(tidyr)
########
## look at the num pickup in the same neighborhood at the same time
#####

## white backgrounds on our plots
theme_set(theme_bw())

# Let there be maps
get_nyc_neighborhoods <- function(){
  r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
  return(readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F))
}
nyc_neighborhoods <- get_nyc_neighborhoods()

map <- get_map(c(-73.87, 40.70), zoom = 11, color="bw")

# settime  rounding factor to 5 minutes
time_rounding_factor <- 5*60

# setspace rounding factor to 2 decimal places
pickup_rounding_factor <- .002
dropoff_rounding_factor <- 2

## 
max_psgrs_per_car = 4

##
parse_datetime <- function(s, format="%Y-%m-%d") {
  as.POSIXct(as.character(s), format=format)
}

# add rounded columns to taxi_clean
taxi_clean <- taxi_clean %>% 
  mutate(rounded_pickup_datetime = 
           as.POSIXct(round(
             as.numeric(pickup_datetime)/time_rounding_factor)*time_rounding_factor, 
             origin = origin , tz= "UTC"), 
         rounded_pickup_lat = round(pickup_latitude/pickup_rounding_factor)*pickup_rounding_factor,
         rounded_pickup_lng = round(pickup_longitude/pickup_rounding_factor)*pickup_rounding_factor,
         rounded_dropoff_lat = round(dropoff_latitude, dropoff_rounding_factor),
         rounded_dropoff_lng = round(dropoff_longitude, dropoff_rounding_factor))

# compute savings cabs, fares, driving miles

overlapping_rides <- taxi_clean %>% 
  filter(pickup_neighborhood != dropoff_neighborhood) %>%
  group_by(rounded_pickup_datetime,
           rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat,
           rounded_dropoff_lng) %>%
  summarize(num_trips = n(),
            total_psgrs = sum(passenger_count),
            total_distance = sum(trip_distance),
            total_trip_time = sum(trip_time_in_secs/3600),
            total_fare = sum(fare_amount)) %>% 
  ungroup() %>%
  filter(num_trips > 1) %>% 
   mutate (avg_psgrs = total_psgrs / num_trips,
           avg_speed = total_distance/ total_trip_time,
           avg_distance = total_distance / num_trips,
           avg_fare = total_fare / num_trips,
           fare_per_psgr = total_fare / total_psgrs,
           min_cabs =  ceiling(total_psgrs / max_psgrs_per_car),
           min_fare = avg_fare * min_cabs,
           min_fare_per_psgr = min_fare / total_psgrs,
           cabs_savings = num_trips - min_cabs,
           fare_savings = total_fare - min_fare,
           fare_savings_per_psgr = fare_per_psgr - min_fare_per_psgr) %>%
  filter(min_cabs > 0)

##########
# total savings:
##########
sum(overlapping_rides$fare_savings)
sum(overlapping_rides$fare_savings_per_psgr)
sum(overlapping_rides$cabs_savings)

# grouping by dropoff lat/lng rounded to 2 decimal points
#
# without filtering rides with pickup and dropoff in same neighborhood:
#   total fare savings: 7382446/167044464 = 0.0441945
#   total cabs savings: 798502/13598831 = 0.05871843
# after filtering out rides with pickup and dropoff in same neighborhood
#   total fare savings: 5879232/167044464 = 0.03519561
#   total cabs savings: 574133/13598831 = 0.04221929

# grouping only by dropoff neighborhood
#
# without filtering rides with pickup and dropoff in same neighborhood:
#   total fare savings: 14462941/167044464 = 0.08658138
#   total cabs savings: 1535521/13598831 = 0.1129157
# after filtering out rides with pickup and dropoff in same neighborhood
#   total fare savings: 11415197/167044464 = 0.06833628
#   total cabs savings: 1070968/13598831 = 0.07875442


##################
###hotspots
#################


carpooling_hotspots_overall <- overlapping_rides %>%
  group_by(rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat,
           rounded_dropoff_lng) %>% 
  summarize(freq = n(), pct = freq/(31*24*12)) 


#######################################
carpooling_hotspots_by_hour <- overlapping_rides %>%
    group_by(ymd= parse_datetime(rounded_pickup_datetime),
             hour = hour(rounded_pickup_datetime),
             rounded_pickup_lat, 
             rounded_pickup_lng,
             rounded_dropoff_lat, 
             rounded_dropoff_lng) %>% 
    summarize(n = n(), total_num_trips = sum(num_trips)) %>%
  group_by(hour, rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat,
           rounded_dropoff_lng) %>% 
  summarize(freq = n(), pct = freq/31) 

##Plot
ggmap(map) +
  geom_point(data = carpooling_hotspots_by_hour, 
             aes(rounded_pickup_lng, rounded_pickup_lat, color= pct), size= 2) +
  scale_color_distiller(palette = "Spectral") + facet_wrap(hour)

################
## filter pct>= 80 %

hotspots <- carpooling_hotspots_by_hour %>% 
  filter(pct >= .80) %>%
  group_by(rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat,
           rounded_dropoff_lng) %>%
  summarize(num_of_hours = n())

############################
##Plots
###########################
ggmap(map) +
  geom_point(data = hotspots, 
             aes(rounded_pickup_lng, rounded_pickup_lat, color= num_of_hours, size=num_of_hours)) +
  scale_color_distiller(palette = "Spectral")

ggsave("../figures/carpooling_hotspots_by_hour_pct_greater_80.png")


plot<- ggmap(map) +
  geom_point(data = carpooling_hotspots_by_hour, 
             aes(rounded_pickup_lng, rounded_pickup_lat, color= pct, frame = hour), size= 2) +
  scale_color_distiller(palette = "Spectral", trans = "log10",
                        breaks = c(1 %o% 10^(0:6)))
gg_animate(plot, ani.width=960, ani.height=960 , filename = "../figures/carpooling_hotspots_by_hour.gif")






