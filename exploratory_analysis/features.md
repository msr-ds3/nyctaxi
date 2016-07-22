#Features for NYC Taxi Model
#####Description: Features to predict efficiency (Total Fare/Shift Length) of any given shift 

###Features (to test)

  * OCCUPANCY_PCT: time with passenger/shift length
    * sum(trip_time)/difftime(endshift, startshift)
  * NUM_TRIPS: total trips in a shift
    * n()
  * SHIFT_LENGTH: start to end duration of shift 
    * difftime(endshift, startshift)
  * AVG_TRIP_DISTANCE: average of all trip distances within shift
  * AVG_TRIP_DURATION: average of all trip durations within shift
  * SD_TRIP_DISTANCE: stdv of all trip distances within shift
  * SD_TRIP_DURATION: stdv of all trip durations within shift
  * SHIFT_TYPE: dayshift = ?  OR  nightshift =  ?
    * 
  * BOROUGH(?):
      * PICKUP_IN_`{borough_name}`_PCT: number of pickups at borough/total trips
      * DROPOFF_IN_`{borough_name}`_PCT: number of dropoffs at borough/total trips
  * AVG_SPEED: Total distance/Total time
  * RATE_CODE_PCT: percent of trips in a shift for each rate code (1-6)
  * AIRPORT_PCT: percent of trips in a shift that are to ?Newark?/JFK/Laguardia
  * DAY_OF_WEEK: day of the week the shift is in
  * POPULAR_NEIGHBOURHOOD_PCT: percentage of trips in top (10?) neighborhoods