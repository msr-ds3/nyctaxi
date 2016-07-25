download.file(url = "https://raw.githubusercontent.com/sdaulton/TaxiPrediction/master/data/nyc-weather-data.csv",
              destfile = "nyc_weather_data.csv")

weather <- read.csv("nyc_weather_data.csv")
