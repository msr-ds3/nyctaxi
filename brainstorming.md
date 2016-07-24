Notes from today's brainstorming meeting, in no particular order.

## Potential research questions
  * Where do different neighborhoods travel to and from at different times of day?
  * What the best and worst times to travel between two parts of the city?
  * What factors contribute to exceedingly long or short trip times?
  * When should you take a subway vs. a taxi, considering variance in driving times?
  * What are the potential benefits of carpooling?
  * How does behavior vary across drivers? Are there patterns to the more successful or efficient drivers?
    * New vs. experienced drivers
    * Day vs. night shifts
  * Strategies around when to quit (cf. past academic work)?
  * Did the speed limit change to 25mph impact taxi fares and/or trip times?
  * Predominant cost and location of trips
  * Peak season for taxi performance
  * Rush hour traffic
  * How efficient is the taxi system overall? How often do cabs have a fare, how many "wasted" trips are taken without passengers? Has this changed over the past few years?
  * How does weather affect taxi ridership?
  * How do special events, holidays, etc., impact traffic flow in the city?
  * What explains variance in tipping?
  * Could taxi hotspots in the outer boroughs be used to plan new public transportation routes or improve existing ones?
  * Could the city subsidize taxi trips to underserved areas (similar to balancing citibike distribution)?
  * Yellow vs. green cab performance


## Related work
  * [Original data request](http://chriswhong.com/open-data/foil_nyc_taxi/)
  * A series of [interesting analyses, with GitHub code](http://toddwschneider.com/posts/analyzing-1-1-billion-nyc-taxi-and-uber-trips-with-a-vengeance/) 
  * Some tips on [working with the data in R](http://hafen.github.io/taxi)
  * ["Hunting or waiting? Discovering passenger-finding strategies from a large-scale real-world taxi dataset"]("Hunting or waiting? Discovering passenger-finding strategies from a large-scale real-world taxi dataset.")
  * [Problems w/ anonymization](https://tech.vijayp.ca/of-taxis-and-rainbows-f6bc289679a1)
  * [When to quit / driver elasticities](http://www.decisionsciencenews.com/2014/11/19/nyc-cab-drivers-quit-early-rains/)
  	* [Link to paper] (https://github.com/msr-ds3/nyctaxi/blob/master/NBER_WORKING_PAPER_SERIES.pdf)  
  * [Optimal strategies?](https://github.com/samuelklee/taxi-strategy)
  * [Analysis from Berkeley student](https://www.ocf.berkeley.edu/~dlevitt/2015/12/13/final-project-nyc-taxi-and-uber-data/)
    * Where most trips happen (by borough)
	* "Average taxi" (or [randomly sampled one](http://nyctaxi.herokuapp.com))
  * Todd Schneider's analysis
  * [Visualizing taxi trips](http://minimaxir.com/2015/11/nyc-ggplot2-howto/)
  * The trip data can also be accessed through [Google BigQuery](https://cloud.google.com/bigquery/public-data/nyc-tlc-trips) ([some](https://www.reddit.com/r/bigquery/comments/28ialf/173_million_2013_nyc_taxi_rides_shared_on_bigquery/) [examples](https://github.com/hack-c/taxidata/blob/master/taxidata.sql))
  * Efficiency of urban taxi systems
    * Can reduce 20% to 90% of vacant taxi trips
  * Special behavior of taxis around holidays
  * Why can't you find a taxi in the rain
  * [Best and worst times of day](http://iquantny.tumblr.com/post/93845043909/quantifying-the-best-and-worst-times-of-day-to-hit)
    * 5am great, lunch bad
  * [538 posts on Uber vs TLC](http://fivethirtyeight.com/tag/uber/)
  * [Uber data](https://github.com/fivethirtyeight/uber-tlc-foil-response)
  * [Outlier Detection in Large-Scale Taxi Trip Records](https://www.cs.uic.edu/~urbcomp2012/papers/UrbComp2012_Paper18_J.T.Zhang.pdf)
  * [Detecting Traffic Anomalies Using Taxi GPS Data](http://www.hindawi.com/journals/mpe/2015/809582/)
  * [Effect of Taxi Information System on Efficiency and Quality of Taxi Services](http://trrjournalonline.trb.org/doi/abs/10.3141/1903-11?journalCode=trr) (under paywall)
  * [Measuring the Efficiency  of the Urban Taxi Service System:](http://www2.cs.uic.edu/~urbcomp2013/urbcomp2014/papers/Zhang_Efficiency_taxi.pdf)
  * [Predicting Taxi Pickups in NYC](http://www.vivekchoksi.com/papers/taxi_pickups.pdf)
  * [YC Taxi Data Prediction](http://sdaulton.github.io/TaxiPrediction/)
  * [Predicting Taxi Tip-Rates in NYC](http://cseweb.ucsd.edu/~jmcauley/cse190/reports/sp15/050.pdf)

## Data questions
  * Do we have an id for the medallion or for individual drivers?
