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
random_drivers <- sample(taxi_clean$hack_license, 500)

filtered_drivers <- taxi_clean %>% filter(hack_license %in% random_drivers)

driver_levels <- filtered_drivers %>% group_by(hack_license) %>%
  summarize(level = min(pickup_datetime)) %>% arrange(level)


driver_data <- filtered_drivers %>%  gather("variable", "datetime", 6:7)

driver_data$hack_license <- factor(driver_data$hack_license, levels = rev(driver_levels$hack_license))

july_07_am_start_shift <- as.numeric(as.POSIXct("2013-07-07 04:00:00", tz = "EDT"))
july_08_am_start_shift <- as.numeric(as.POSIXct("2013-07-08 04:00:00", tz = "EDT"))
july_09_am_start_shift <- as.numeric(as.POSIXct("2013-07-09 04:00:00", tz = "EDT"))
july_10_am_start_shift <- as.numeric(as.POSIXct("2013-07-10 04:00:00", tz = "EDT"))
july_11_am_start_shift <- as.numeric(as.POSIXct("2013-07-11 04:00:00", tz = "EDT"))
july_12_am_start_shift <- as.numeric(as.POSIXct("2013-07-12 04:00:00", tz = "EDT"))
july_13_am_start_shift <- as.numeric(as.POSIXct("2013-07-13 04:00:00", tz = "EDT"))

july_07_am_end_shift <- as.numeric(as.POSIXct("2013-07-07 10:00:00", tz = "EDT"))
july_08_am_end_shift <- as.numeric(as.POSIXct("2013-07-08 10:00:00", tz = "EDT"))
july_09_am_end_shift <- as.numeric(as.POSIXct("2013-07-09 10:00:00", tz = "EDT"))
july_10_am_end_shift <- as.numeric(as.POSIXct("2013-07-10 10:00:00", tz = "EDT"))
july_11_am_end_shift <- as.numeric(as.POSIXct("2013-07-11 10:00:00", tz = "EDT"))
july_12_am_end_shift <- as.numeric(as.POSIXct("2013-07-12 10:00:00", tz = "EDT"))
july_13_am_end_shift <- as.numeric(as.POSIXct("2013-07-13 10:00:00", tz = "EDT"))

july_07_pm_start_shift <- as.numeric(as.POSIXct("2013-07-07 14:00:00", tz = "EDT"))
july_08_pm_start_shift <- as.numeric(as.POSIXct("2013-07-08 14:00:00", tz = "EDT"))
july_09_pm_start_shift <- as.numeric(as.POSIXct("2013-07-09 14:00:00", tz = "EDT"))
july_10_pm_start_shift <- as.numeric(as.POSIXct("2013-07-10 14:00:00", tz = "EDT"))
july_11_pm_start_shift <- as.numeric(as.POSIXct("2013-07-11 14:00:00", tz = "EDT"))
july_12_pm_start_shift <- as.numeric(as.POSIXct("2013-07-12 14:00:00", tz = "EDT"))
july_13_pm_start_shift <- as.numeric(as.POSIXct("2013-07-13 14:00:00", tz = "EDT"))

july_07_pm_end_shift <- as.numeric(as.POSIXct("2013-07-07 20:00:00", tz = "EDT"))
july_08_pm_end_shift <- as.numeric(as.POSIXct("2013-07-08 20:00:00", tz = "EDT"))
july_09_pm_end_shift <- as.numeric(as.POSIXct("2013-07-09 20:00:00", tz = "EDT"))
july_10_pm_end_shift <- as.numeric(as.POSIXct("2013-07-10 20:00:00", tz = "EDT"))
july_11_pm_end_shift <- as.numeric(as.POSIXct("2013-07-11 20:00:00", tz = "EDT"))
july_12_pm_end_shift <- as.numeric(as.POSIXct("2013-07-12 20:00:00", tz = "EDT"))
july_13_pm_end_shift <- as.numeric(as.POSIXct("2013-07-13 20:00:00", tz = "EDT"))


ggplot(driver_data, aes(datetime, as.factor(hack_license), color=variable)) + geom_point(size = 1.5, alpha = .75) + 
  scale_x_datetime(date_breaks = "2 hour",labels=date_format("%H")) + scale_y_discrete(labels= NULL) + xlab("hour of day") + ylab("100 random drivers") +
  geom_vline(xintercept = july_07_am_start_shift, color="blue") +
  geom_vline(xintercept = july_08_am_start_shift, color="blue") + 
  geom_vline(xintercept = july_09_am_start_shift ,color="blue") +
  geom_vline(xintercept = july_10_am_start_shift, color="blue") +
  geom_vline(xintercept = july_11_am_start_shift, color="blue") +
  geom_vline(xintercept = july_12_am_start_shift, color="blue") +
  geom_vline(xintercept = july_13_am_start_shift, color="blue")  +
  
  geom_vline(xintercept = july_07_am_end_shift, color="blue") +
  geom_vline(xintercept = july_08_am_end_shift, color="blue") + 
  geom_vline(xintercept = july_09_am_end_shift ,color="blue") +
  geom_vline(xintercept = july_10_am_end_shift, color="blue") +
  geom_vline(xintercept = july_11_am_end_shift, color="blue") +
  geom_vline(xintercept = july_12_am_end_shift, color="blue") +
  geom_vline(xintercept = july_13_am_end_shift, color="blue") +
  
  geom_vline(xintercept = july_07_pm_start_shift, color="black") +
  geom_vline(xintercept = july_08_pm_start_shift, color="black") + 
  geom_vline(xintercept = july_09_pm_start_shift ,color="black") +
  geom_vline(xintercept = july_10_pm_start_shift, color="black") +
  geom_vline(xintercept = july_11_pm_start_shift, color="black") +
  geom_vline(xintercept = july_12_pm_start_shift, color="black") +
  geom_vline(xintercept = july_13_pm_start_shift, color="black")  +
  
  geom_vline(xintercept = july_07_pm_end_shift, color="black") +
  geom_vline(xintercept = july_08_pm_end_shift, color="black") + 
  geom_vline(xintercept = july_09_pm_end_shift, color="black") +
  geom_vline(xintercept = july_10_pm_end_shift, color="black") +
  geom_vline(xintercept = july_11_pm_end_shift, color="black") +
  geom_vline(xintercept = july_12_pm_end_shift, color="black") +
  geom_vline(xintercept = july_13_pm_end_shift, color="black") 