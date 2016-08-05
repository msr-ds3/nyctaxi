load("../Rdata/one_month_taxi.Rdata")
library(lubridate)
library(dplyr)
library(gganimate)
library(ggplot2)
library(ggmap)
library(scales)
library(tidyr)
library(leaflet)
library(RColorBrewer)
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
time_rounding_factor <- 6*60

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
rounded_pickup_lat = ifelse(pickup_neighborhood == "John F. Kennedy International Airport" |
rate_code == 2 , 40.641, ifelse(pickup_neighborhood =="LaGuardia Airport", 40.773,
 round(pickup_latitude/pickup_rounding_factor)*pickup_rounding_factor)),


rounded_pickup_lng = ifelse(pickup_neighborhood == "John F. Kennedy International Airport" |
rate_code == 2 ,-73.777, ifelse(pickup_neighborhood =="LaGuardia Airport",-73.872,
round(pickup_longitude/pickup_rounding_factor)*pickup_rounding_factor)),

rounded_dropoff_lat = ifelse(dropoff_neighborhood == "John F. Kennedy International Airport" |
          rate_code == 2 , 40.64, ifelse(dropoff_neighborhood =="LaGuardia Airport",40.77,
          round(dropoff_latitude, dropoff_rounding_factor))),
rounded_dropoff_lng = ifelse(dropoff_neighborhood == "John F. Kennedy International Airport" |
  rate_code == 2 ,-73.78, ifelse(dropoff_neighborhood =="LaGuardia Airport", -73.87,
                                 round(dropoff_longitude, dropoff_rounding_factor))))


# compute savings cabs, fares, driving miles

overlapping_rides <- taxi_clean %>% 
 # filter(pickup_neighborhood != dropoff_neighborhood) %>%
  group_by(rounded_pickup_datetime,
           rounded_pickup_lat, 
           rounded_pickup_lng,
          dropoff_neighborhood) %>%
           # rounded_dropoff_lat,
           #rounded_dropoff_lng) %>%
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

# extending to 6 minutes

# grouping only by dropoff neighborhood
#
# without filtering rides with pickup and dropoff in same neighborhood:
#   total fare savings: 22486619/167044464 = 0.1346146
#   total cabs savings: 1876187/13598831 = 0.1379668
# after filtering out rides with pickup and dropoff in same neighborhood
#   total fare savings: 14259875/167044464 = 0.08536574
#   total cabs savings: 827434/13598831 = 0.06084597
##################
### 20 top hotspots
#################
# returns a continous color palette within a range to use for use in leaflet maps
set_pal <- function(range, na_color="#808080", pal = brewer.pal(11, "Spectral"))
{
  colorNumeric(palette = pal, domain = range, na.color = na_color)
}

carpooling_hotspots_overall <- overlapping_rides %>%
  group_by(rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat,
           rounded_dropoff_lng) %>% 
  summarize(freq = n(), pct = freq/(31*24*12)) 

top_20_hotspots <- carpooling_hotspots_overall %>%
  group_by(rounded_pickup_lat,
           rounded_pickup_lng)   %>% 
  summarize(top_pct = max(pct)) %>%  ungroup() %>% 
  top_n(n= 25, wt= top_pct) %>% 
  arrange(-top_pct) %>%
  mutate(index = row_number())

pal = set_pal(1:20) 
leaflet(data = top_20_hotspots) %>%
  setView(-74.00, 40.71, zoom = 12) %>%
  addCircleMarkers(~rounded_pickup_lng ,
             ~rounded_pickup_lat, 
             popup = ~as.character(index),
             weight = 1,
             fillColor = ~pal(index), fillOpacity = .8) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") 

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
  aes(rounded_pickup_lng, rounded_pickup_lat, color= pct, alpha = pct, size= pct)) +
  scale_color_distiller(palette = "Spectral" , trans= "log10") + facet_wrap(~hour)
ggsave("../figures/carpooling_hotspots_by_hour.png")

################
## filter pct>= 80 %

hotspots <- carpooling_hotspots_by_hour %>% 
  filter(pct >= .80) %>%
  group_by(rounded_pickup_lat, 
           rounded_pickup_lng,
           rounded_dropoff_lat,
           rounded_dropoff_lng) %>%
  summarize(num_of_hours = n())


hotspots_by_hour <- carpooling_hotspots_by_hour %>% 
  group_by(rounded_pickup_lat, 
           rounded_pickup_lng) %>%
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






