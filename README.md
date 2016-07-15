# nyctaxi
* Download the 2013 taxi data using [this](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/download_original_taxidata_2013.sh) shell script.
    * To download the 2015 taxi data (includes both yellow and green taxi data but lacks medallion and hack license info), use [this](https://github.com/msr-ds3/nyctaxi/blob/master/download_taxidata.sh) one. To load in R, use [this](https://github.com/msr-ds3/nyctaxi/blob/master/load_taxi_data.R) script.
* Load the csvs for one week in july as a dataframe in R using [this R script](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/load_one_week.R).

## Descriptives
* Stats for one week of taxi rides by day of week, hour of day, pickup location, and dropoff location are computed by [this R script](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/one_week_analysis.R).
* Trip based descriptive plotting (distributions of distance, time, fare, etc) can be found [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/Trips_based_Descriptives.R)
* Driver based descriptive plotting (distributions of distance, time, fare, etc, by number of drivers) are [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/Drivers_Descriptives.R)
* Neighborhood popularity plots (in R) are [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/one_week_neighborhood_popularity.R)
* Interactive popularity heatmaps by neighborhood can be created using [this](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/popularity_heatmaps.R) script 
* Ggmap (not-interactive) popularity heatmaps can be created using the functions in [here](https://github.com/msr-ds3/nyctaxi/blob/master/exploratory_analysis/ggheatmaps.R)
* Cool figures, plots, and maps (output of some of the above) are in [this](https://github.com/msr-ds3/nyctaxi/tree/master/figures) dir
