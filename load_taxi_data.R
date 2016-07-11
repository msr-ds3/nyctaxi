library(readr)
library(ggplot)

csvs <- Sys.glob('*_tripdata_2015-*.csv')
taxi <- data.frame()
for (csv in csvs)
{
  tmp <- read_csv(csv)
  taxi <- rbind(tmp, taxi)
}

save(taxi, "taxidata.Rdata")