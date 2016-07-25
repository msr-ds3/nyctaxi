library(dplyr)
library(lubridate)

parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}

download.file(url = "https://raw.githubusercontent.com/sdaulton/TaxiPrediction/master/data/nyc-weather-data.csv",
              destfile = "nyc_weather_data.csv")

weather <- read.csv("nyc_weather_data.csv")

weather <- select(weather, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather) <- tolower(names(weather))
weather <- mutate(weather,
                  tmin = tmin / 10,
                  tmax = tmax / 10,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather <- tbl_df(weather)
