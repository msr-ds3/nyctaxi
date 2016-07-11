library(readr)

green_csvs <- Sys.glob('green_tripdata_2015-*.csv')
green_taxi <- data.frame()
for (csv in green_csvs)
{
  tmp <- read_csv(csv)
  green_taxi <- rbind(tmp, green_taxi)
}

yellow_csvs <- Sys.glob('yellow_tripdata_2015-*.csv')
yellow_taxi <- data.frame()
for (csv in yellow_csvs)
{
  tmp <- read_csv(csv)
  yellow_taxi <- rbind(tmp, yellow_taxi)
}

save(yellow_taxi, green_taxi, "taxidata.Rdata")