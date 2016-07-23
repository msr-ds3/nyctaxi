---
output: html_document
---
# nyctaxi
## Prerequisites
* Download the 2013 taxi data using [this](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/download_original_taxidata_2013.sh) shell script.
    * To download the 2015 taxi data (includes both yellow and green taxi data but lacks medallion and hack license info), use [this](https://github.com/msr-ds3/nyctaxi/blob/master/download_taxidata.sh) one. To load in R, use [this](https://github.com/msr-ds3/nyctaxi/blob/master/load_taxi_data.R) script.
* [This R script] (https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/load_one_week.R) loads the csvs, adds necessary and convenient columns (e.g. neighborhood names) and saves them as `taxi_clean` in `one_week_taxi.Rdata`. To use the dataframe, simply call `load('one_week_taxi.Rdata')`. (NOTE: AS OF 7/18/2016 4:15 PM YOU SHOULD RERUN THIS SCRIPT TO REFLECT NECESSARY CHANGES)
* [This R script](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/shifts_intervals.R) uses `taxi_clean` to create a dataframe calles `shifts_clean` of drivers (`hack_license`s) and their shifts (as measured by the cutoff analysis [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/downtime_cutoff_analysis.R)), and  a dataframe called `taxi_clean_shifts` with a shift number for each ride, and stores it in an Rdata file called `shifts_clean.Rdata`. {NOTE: AS OF 7/22 9:20 AM, RERUN THIS SCRIPT TO REFLECT NAME CHANGES}

##Descriptives
* Cool figures, plots, and maps (output of some of the scripts below) are in [this](https://github.com/msr-ds3/nyctaxi/tree/master/figures) dir
* [This](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/map_visualization_functions.R) script creates a function (``visualize_trips``) that can plot the route of a random taxicab driver over a day or a week.
    * Usage: ``visualize_trips(df, hacklicense, day = NULL)``. `df` is the dataframe (usually `taxi_clean` but sometimes a subset of that. `hacklicense` is the `hack_license` of the driver (usually randomly chosen from `df`). `day` is optional - it takes a day in the format of `"Sun"`, `"Mon"`, etc; when ommitted, results will be shown for the entire week, faceted by ``day_of_the_week``

### Trip-based
* Stats for one week of taxi rides by day of week, hour of day, pickup location, and dropoff location are computed by [this R script](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/one_week_analysis.R).
* Trip based descriptive plotting (distributions of distance, time, fare, etc) can be found [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/trips_based_Descriptives.R)
* Neighborhood popularity plots (in R) are [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/one_week_neighborhood_popularity.R)
* Interactive popularity heatmaps by neighborhood can be created using [this](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/popularity_heatmaps.R) script 
* Ggmap (not-interactive) popularity heatmaps can be created using the functions in [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/ggheatmaps.R)

### Driver-based
* Driver based descriptive plotting (distributions of distance, time, fare, etc, by number of drivers) are [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/driver_Descriptives.R)
* Visualize shifts, and rides within them, for n random drivers by calling the `visualize_rides_and_shifts()` function created by [this R script](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/rides_and_shifts_visualization.R).

### Shift-based
* Some plots using shift intervals [here] (https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/plots_with_shift_interval.R)

## Other work
###De-anonymization
* [Java code](https://github.com/msr-ds3/nyctaxi/tree/master/deanonymization) that can de-anonymize medallions and hack licenses.
 

