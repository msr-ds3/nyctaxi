load("../Rdata/shifts_design_matrix.Rdata")
source("load_weather.R")

library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(plyr)
# White backgrounds on our plots
theme_set(theme_minimal())

##########################################
## Add ymd col
##########################################
shifts_design_matrix <- shifts_design_matrix %>% mutate(ymd= as.Date(start))

########################################
## Join the two data frames:
########################################
shifts_design_matrix_weather <- left_join(shifts_design_matrix, weather, by="ymd")

########################################
# function to define high and low earning
########################################

mean_efficiency <- mean(shifts_design_matrix_weather$efficiency)
sd_efficiency <- sd(shifts_design_matrix_weather$efficiency)
high_earning <- mean_efficiency + sd_efficiency
low_earning <- mean_efficiency - sd_efficiency



high_low_efficiency <- function(efficiency){
   if (efficiency >= high_earning)
     "high"
  else if (efficiency <= low_earning)
    "low"
  else
    "med"
}

high_low_efficiency <- Vectorize(high_low_efficiency)


shifts_design_matrix_weather <- shifts_design_matrix_weather %>%  
mutate(efficiency_category= high_low_efficiency(efficiency))


########################################
# plot shifts_design_matrix_weather data
########################################

df_classification <- shifts_design_matrix_weather %>% 
           filter(efficiency_category == "high" | efficiency_category == "low")
##########################################
########Occupancy pct:
##########################################
## Classification:
ggplot(df_classification, aes(occupancy_pct, color= efficiency_category)) +
                geom_density() + xlab("time with passenger/shift length")
ggsave("../figures/occupancy_pct_density.png")
## Regression:
ggplot(shifts_design_matrix_weather, aes(occupancy_pct, efficiency)) +
  geom_point(alpha=0.1) + xlab("time with passenger/shift length") + 
 ylim(0,100) + xlim(0,1) + 
  geom_smooth()
ggsave("../figures/occupancy_pct_vs_efficiency.png")


###########################################
######## shift lenght
##########################################
## Classification:
ggplot(df_classification, aes(length, color= efficiency_category)) +
  geom_density() + xlab("shift length")
ggsave("../figures/shift_length_density.png")
## Regression:
ggplot(shifts_design_matrix_weather, aes(length, efficiency)) +
  geom_point(alpha=0.3) + xlab("shift length") + 
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/shift_length_vs_efficiency.png")

###########################################
######## Num trips
##########################################
## Classification:
ggplot(df_classification, aes(num_trips, color= efficiency_category) ) +
  geom_density() + xlab("number of trips")
ggsave("../figures/num_trips_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(num_trips, efficiency)) +
  geom_point(alpha=0.3) + xlab("number of trips") + 
 ylim(0,100) + 
  geom_smooth()
ggsave("../figures/num_trips_vs_efficiency.png")

###########################################
######## Total fare
##########################################
## Classification:
ggplot(df_classification, aes(total_fare, color= efficiency_category) ) +
  geom_density() + xlab("total fare")
ggsave("../figures/total_fare_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(total_fare, efficiency)) +
  geom_point(alpha=0.3) + xlab("total fare") + 
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/total_fare_vs_efficiency.png")

###########################################
######## Total trip distance
##########################################
## Classification:
ggplot(df_classification, aes(total_trip_distance, color= efficiency_category) ) +
  geom_density() + xlab("total trip distance")
ggsave("../figures/total_trip_distance_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(total_trip_distance, efficiency)) +
  geom_point(alpha=0.3) + xlab("total trip distance") + 
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/total_trip_distance_vs_efficiency.png")


###########################################
######## Avg trip distance
##########################################
## Classification:
ggplot(df_classification, aes(avg_trip_distance, color= efficiency_category) ) +
  geom_density() + xlab("Average trip distance")
ggsave("../figures/avg_trip_distance_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(avg_trip_distance, efficiency)) +
  geom_point(alpha=0.1) + xlab("Average trip distance") + 
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/avg_trip_distance_vs_efficiency.png")

###########################################
######## Standard deviation trip distance
##########################################
## Classification:
ggplot(df_classification, aes(sd_trip_distance, color= efficiency_category) ) +
  geom_density() + xlab("Standard deviation trip distance")
ggsave("../figures/sd_trip_distance_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(sd_trip_distance, efficiency)) +
  geom_point(alpha=0.1) + xlab("Standard deviation trip distance") + 
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/sd_trip_distance_vs_efficiency.png")

###########################################
######## Total trip time
##########################################
## Classification:
ggplot(df_classification, aes(total_trip_time, color= efficiency_category) ) +
  geom_density() + xlab("Total trip time")
ggsave("../figures/total_trip_time_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(total_trip_time, efficiency)) +
  geom_point(alpha=0.1) + xlab("Total trip time") + 
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/total_trip_time_vs_efficiency.png")

###########################################
######## Average trip time
##########################################
## Classification:
ggplot(df_classification, aes(avg_trip_time, color= efficiency_category) ) +
  geom_density() + xlab("Average trip time")
ggsave("../figures/avg_trip_time_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(avg_trip_time, efficiency)) +
  geom_point(alpha=0.1) + xlab("Average trip time") + 
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/avg_trip_time_vs_efficiency.png")

###########################################
######## Standard deviation trip time
##########################################
## Classification:
ggplot(df_classification, aes(sd_trip_time, color= efficiency_category) ) +
  geom_density() + xlab("Standard deviation trip time")
ggsave("../figures/sd_trip_time_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(sd_trip_time, efficiency)) +
  geom_point(alpha=0.1) + xlab("Standard deviation trip time") + 
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/sd_trip_time_vs_efficiency.png")


###########################################
######## Speed
##########################################
## Classification:
ggplot(df_classification, aes(avg_speed, color= efficiency_category) ) +
  geom_density() + xlab("Total distance/Total trip duration")
ggsave("../figures/speed_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(avg_speed, efficiency)) +
  geom_point(alpha=0.1) + xlab("Total distance/Total trip duration") +
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/speed_vs_efficiency.png")


###########################################
######## Precipitation
##########################################
## Classification:
ggplot(df_classification, aes(prcp, color= efficiency_category) ) +
  geom_density() + xlab("Precipitation")
ggsave("../figures/prcp_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(prcp, efficiency)) +
  geom_point(alpha=0.1) + xlab("Precipitation") +
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/prcp_vs_efficiency.png")


###########################################
######## minimum temperature
##########################################
## Classification:
ggplot(df_classification, aes(tmin, color= efficiency_category) ) +
  geom_density() + xlab("minimum temperature")
ggsave("../figures/tmin_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(tmin, efficiency)) +
  geom_point(alpha=0.1) + xlab("minimum temperature") +
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/tmin_vs_efficiency.png")

###########################################
######## maximum temperature
##########################################
## Classification:
ggplot(df_classification, aes(tmax, color= efficiency_category) ) +
  geom_density() + xlab("maximum temperature")
ggsave("../figures/tmax_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(tmax, efficiency)) +
  geom_point(alpha=0.1) + xlab("maximum temperature") +
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/tmax_vs_efficiency.png")

###########################################
######## Average temperature
##########################################

###Add average temperature column:
df_classification <- df_classification %>% mutate(avg_temp = (tmax + tmin) /2)
shifts_design_matrix_weather <- shifts_design_matrix_weather %>%
                                           mutate(avg_temp = (tmax + tmin) /2)
## Classification:
ggplot(df_classification, aes(avg_temp , color= efficiency_category) ) +
  geom_density() + xlab("average temperature")
ggsave("../figures/avg_temp_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(avg_temp, efficiency)) +
  geom_point(alpha=0.1) + xlab("average temperature") +
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/avg_temp_vs_efficiency.png")


###########################################
######## Airport percentage
##########################################
## Classification:
ggplot(df_classification, aes(airport_pct , color= efficiency_category) ) +
  geom_density() + xlab("Airport percentage") +
  xlim(0,0.50)
  
ggsave("../figures/airport_pct_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(airport_pct, efficiency)) +
  geom_point(alpha=0.1) + xlab("Airport percentage") +
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/airport_pct_vs_efficiency.png")

################################################
######## Popular pickup neighborhood percentage
################################################
## Classification:
ggplot(df_classification, aes(popular_pickup_neighborhood_pct , color= efficiency_category) ) +
  geom_density() + xlab("Popular pickup neighborhood percentage") 
ggsave("../figures/popular_pickup_neighborhood_pct_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(popular_pickup_neighborhood_pct, efficiency)) +
  geom_point(alpha=0.1) + xlab("Popular pickup neighborhood percentage") +
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/popular_pickup_neighborhood_pct_vs_efficiency.png")

################################################
######## Popular dropoff neighborhood percentage
################################################
## Classification:
ggplot(df_classification, aes(popular_dropoff_neighborhood_pct , color= efficiency_category) ) +
  geom_density() + xlab("Popular dropoff neighborhood percentage") 
ggsave("../figures/popular_dropoff_neighborhood_pct_density.png")

## Regression:
ggplot(shifts_design_matrix_weather, aes(popular_dropoff_neighborhood_pct, efficiency)) +
  geom_point(alpha=0.1) + xlab("Popular dropoff neighborhood percentage") +
  ylim(0,100) + 
  geom_smooth()
ggsave("../figures/popular_dropoff_neighborhood_pct_vs_efficiency.png")

#######################################################
#######Added a graph for efficiency over the shift type
#######################################################
efficiency_vs_shift_type <- ggplot(shifts_design_matrix, 
                                   aes(x = efficiency, 
                                       color = as.factor(shift_type))) + 
  geom_density() + xlim(0,100)

ggsave('../figures/efficiency_vs_shift_type.png', 
       plot = efficiency_vs_shift_type)

