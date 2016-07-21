load("one_week_taxi.Rdata")
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

theme_set(theme_minimal())

###########################################
#examine shifts, active hours, and downtime
###########################################
shifts <- taxi_clean %>% group_by(hack_license) %>% 
  arrange(pickup_datetime) %>% 
  mutate(downtime = (lead(pickup_datetime) - dropoff_datetime)/3600) %>% 
  select(pickup_datetime, dropoff_datetime, day_of_the_week, downtime)

##########################################################
#plotting the number of intervals aver the downtime period
##########################################################
ggplot(shifts) + geom_density(aes(as.numeric(downtime) ), na.rm = T) + 
  scale_x_log10(label = comma, breaks = seq(1, 10, 2)) +
  xlab("downtime in hours (log scale)") +
  geom_vline(xintercept = 6)
#The cutoff time is 6 hours
ggsave(filename = "downtime_distribution.png")







