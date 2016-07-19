# nyctaxi
* Download the 2013 taxi data using [this](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/download_original_taxidata_2013.sh) shell script.
    * To download the 2015 taxi data (includes both yellow and green taxi data but lacks medallion and hack license info), use [this](https://github.com/msr-ds3/nyctaxi/blob/master/download_taxidata.sh) one. To load in R, use [this](https://github.com/msr-ds3/nyctaxi/blob/master/load_taxi_data.R) script.
* Load the csvs for one week in july as a dataframe in R using [this R script] (https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/load_one_week.R). (NOTE: AS OF 7/18/2016 4:15 PM YOU SHOULD RERUN THIS SCRIPT TO REFLECT NECESSARY CHANGES)

## Descriptives

* Cool figures, plots, and maps (output of some of the scripts below) are in [this](https://github.com/msr-ds3/nyctaxi/tree/master/figures) dir
* [This](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/visualize_trips.R) script plots a day's worth of rides made by random taxicab driver.

### Trip-based
* Stats for one week of taxi rides by day of week, hour of day, pickup location, and dropoff location are computed by [this R script](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/one_week_analysis.R).
* Trip based descriptive plotting (distributions of distance, time, fare, etc) can be found [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/Trips_based_Descriptives.R)
* Neighborhood popularity plots (in R) are [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/one_week_neighborhood_popularity.R)
* Interactive popularity heatmaps by neighborhood can be created using [this](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/popularity_heatmaps.R) script 
* Ggmap (not-interactive) popularity heatmaps can be created using the functions in [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/ggheatmaps.R)

### Driver-based
* Driver based descriptive plotting (distributions of distance, time, fare, etc, by number of drivers) are [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/Drivers_Descriptives.R)
* Visualize shifts by plotting pickup and dropoff times for 100 random drivers over the course of a week and by seeing active hours, first and last time seen, and more, using [this R script](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/identifying_shifts.R).

## Other work
###De-anonymization
* [Java code](https://github.com/msr-ds3/nyctaxi/tree/master/deanonymization) that can de-anonymize medallions and hack licenses.
