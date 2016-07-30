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

map <- get_map(c(-73.87, 40.70), zoom = 11, color="bw",maptype = "satellite")

# settime  rounding factor to 5 minutes
time_rounding_factor <- 5*60

# setspace rounding factor to 2 decimal places
pickup_rounding_factor <- .002
dropoff_rounding_factor <- 2

## 
max_passengers_per_car = 4

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

rounded_pickup_dropoff_time_df <- taxi_clean %>% 
  filter(pickup_neighborhood != dropoff_neighborhood) %>%
  group_by(rounded_pickup_datetime,
           rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat, 
           rounded_dropoff_lng) 
rounded_pickup_dropoff_time_df <- rounded_pickup_dropoff_time_df %>%
  summarize(
            num_trips = n(),
            total_num_passenger = sum(passenger_count),
            total_distance = sum(trip_distance),
            total_trip_time = sum(trip_time_in_secs/3600),
            total_fare = sum(fare_amount)) %>% 
  ungroup() %>%
  filter(num_trips > 1) %>% 
   mutate (
           avg_num_passenger = total_num_passenger / num_trips,
           avg_speed = total_distance/ total_trip_time,
           avg_distance = total_distance / num_trips,
           avg_fare = total_fare / num_trips,
           avg_fare_per_passenger = total_fare / total_num_passenger,
           min_cabs =  ceiling(total_num_passenger / max_passengers_per_car),
           min_fare = avg_fare * min_cabs,
           min_fare_per_passenger = min_fare / total_num_passenger,
           saving_cabs = num_trips - min_cabs,
           saving_fare = total_fare - min_fare,
           saving_fare_per_passenger= avg_fare_per_passenger - min_fare_per_passenger) %>%
  filter(min_cabs > 0)


save(df, file = "savings.Rdata")

##################
###hotspots
#################


carpooling_hotspots_overall <- rounded_pickup_dropoff_time_df %>%
  group_by(rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat,
           rounded_dropoff_lng) %>% 
  summarize(freq = n(), pct = freq/(31*24*12)) 


#######################################
carpooling_hotspots_by_hour <- rounded_pickup_dropoff_time_df %>%
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






