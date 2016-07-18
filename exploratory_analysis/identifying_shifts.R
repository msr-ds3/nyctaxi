# load("one_week_taxi.Rdata")
source("one_week_analysis.R")
library(ggplot2)
library(scales)
library(tidyr)

theme_set(theme_minimal())
# examine shifts and active hours
shifts <- taxi_clean %>% group_by(medallion, hack_license, day_of_the_week) %>% 
  summarize(first = min(pickup_datetime), last = max(pickup_datetime), active_hours = n_distinct(hour), num_of_trips = n(), revenue = sum(total_amount))

ggplot(shifts, aes(revenue)) + geom_histogram(binwidth = 1) 

## see pickups and dropoffs for hundred random taixs throught the week
random_drivers <- sample(taxi_clean$hack_license, 100)

filtered_drivers <- taxi_clean %>% filter(hack_license %in% random_drivers)

driver_levels <- filtered_drivers %>% group_by(hack_license) %>%
  summarize(level = min(pickup_datetime)) %>% arrange(level)


driver_data <- filtered_drivers %>%  gather("variable", "datetime", 6:7)

driver_data$hack_license <- factor(driver_data$hack_license, levels = rev(driver_levels$hack_license))

ggplot(driver_data, aes(datetime, as.factor(hack_license), color=variable)) + geom_point(size = 1.5, alpha = .75) + 
  scale_x_datetime(date_breaks = "4 hour",labels=date_format("%H")) + scale_y_discrete(labels= NULL) + xlab("hour of day") + ylab("100 random drivers")
