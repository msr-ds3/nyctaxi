load("one_week_taxi.Rdata")
library(tidyr)

shifts <- taxi_clean %>% group_by(medallion, hack_license, day_of_the_week) %>% 
  summarize(first = min(pickup_datetime), last = max(pickup_datetime), active_hours = n_distinct(hour), num_of_trips = n(), revenue = sum(total_amount))
ggplot(shifts, aes(revenue)) + geom_histogram(binwidth = 1) 
random_drivers <- sample(taxi_clean$hack_license, 100)

driver_data <- taxi_clean %>% filter(hack_license %in% random_drivers) %>% gather("variable", "datetime", 6:7)

ggplot(driver_data, aes(datetime, as.factor(hack_license), color=variable)) + geom_point(size = 2) + 
  scale_x_datetime(date_breaks = "4 hour",labels=date_format("%H")) + scale_y_discrete(labels= NULL)
