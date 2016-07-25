#Features for NYC Taxi Model
#####Description: Features to predict efficiency (Total Fare/Shift Length) of any given shift 

###Features (to test)

Feature | Description | Computation
:--- | --- | ---
`occupancy_pct` | time with passenger/shift length | `sum(trip_time)/difftime(endshift, startshift)`
`num_trips` | total trips in a shift | `n()`
`length` | start to end duration of shift | `difftime(endshift, startshift)`
`avg_trip_distance` |  average of all trip distances within shift | `mean(trip_distance)`
`avg_trip_duration` |  average of all trip durations within shift | `mean(trip_time_in_secs)/3600`
`sd_trip_distance` | stdv of all trip distances within shift | `sd(trip_distance)`
`sd_trip_duration` | stdv of all trip durations within shift | `sd(trip_time_in_secs)/3600`
`shift_type` | dayshift = ? OR nightshift = ? | n()
`pickup_in_{borough_name}_pct` | number of pickups at borough/total trips | `length( pickup_neighborhood[pickup_nieghborhood == "{borough_name}"])/num_trips`
`dropoff_in_{borough_name}_pct` | number of dropoffs at borough/total trips | `length( dropoff_neighborhood[dropoff_nieghborhood == "{borough_name}"])/num_trips`
`avg_speed` |Total distance/Total trip duration | `sum(trip_distance)/(sum(trip_time_in_secs)/3600)`
`rate_code_{#}_pct` |  percent of trips in a shift for each rate code (1-6) |  `length( rate_code[rate_code == "{#}"])/num_trips`
`airport_pct` | percent of trips in a shift that are to ?Newark?/JFK/Laguardia | `???`
`day_of_week` | day of the week the shift starts in | `wday(min(pickup_datetime))`
`popular_pickup_nieghborhood_pct` | percentage of pickups in top (10?) neighborhoods | `???`
`popular_dropoff_nieghborhood_pct` | percentage of dropoffs in top (10?) neighborhoods | `???`
`unpopular_pickup_neighborhood_pct` | percentage of dropoffs in least popular neighborhoods | `???`
`unpopular_dropoff_neighborhood_pct` | percentage of dropoffs in least popular neighborhoods | `???`
`tmin` | minimum temperature of the day shift started on | `tmin`
`tmax` | maximum temperature of the day shift started on | `tmax`
`prcp` | total precipitation on the day shift started on | `prcp`
`tavg` | average temperature over the day shift started on | `tmin+tmax/2`






