#Data Decisions 
1. We decided to use the 2013 Taxi data because this data set had `hack_licenses` which allowed us to associate trips with drivers and perform analyses on driver patterns. 
2. Using fare amount rather than total amount. Total amount includes tolls, taxes, 
and most significantly tips (which are not recorded for cash payments!)
3. We initially ran our scripts on one week of data, and then one month because of computation time. 
4. We determined a "shift" of a driver by plotting the downtime (the time between each trip and the next) and noticed that 6 hours seems to be a reasonable cutoff downtime for the end of a shift. Since this cutoff is not perfect, some drivers had shifts that spanned for much longer than a "typical" shift (>12 hours), and some less than a reasonable shift. 
5. To analyze the efficiency of shifts, we had to apply a filter on the shift length. Short shifts (< ~4 hours) and long shifts (> ~15 hours) were filtered out of our data frame, before we analyzed the efficiencies of drivers with "typical" shift lengths.   

#Weirdness
1. Coordinates off the globe, some thousands of miles from New York (e.g. Texas,  Antarctica). These were filtered out as they were of no practical use to our research. 
2. Unrealistic fare to trip time ratios. Although some of these trips were marked as "negotiated fares", they don't tell us what went really went on. Some trips, for example, yielded hundreds of dollars but had a trip time of less than a minute. 
3. One cab driver used 175 different taxis in one week. Unsurprisingly, this cab driver breaks all sorts of records in terms of the amount of hours worked, the amount of money earned. This cab driver had records for metered trips that took place simultaneously, suggesting that there was more than one person using the hack license. 
4. Unclear meter reportings - we had a number of trips that started and ended in the same exact location but had a significant fare charged. These might be explained by the cab driver failing to turn his meter on and off at the appropriate time.  


#Main Data Frames
1. `shifts_design_matrix` - shifts for month of July w/ shift_type, popularity of rate codes and pickup/dropoff boroughs, and weather info (shifts_design_matrix.Rdata)
2. `taxi_cleans_shifts` - trips for month of July with corresponding shift numbers (one_month_taxi.Rdata)

#Other Data Frames
1. `shifts_clean` - `taxi_clean_shifts` grouped and summarized into shifts


