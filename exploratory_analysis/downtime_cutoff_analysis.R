load("../Rdata/one_month_taxi.Rdata")
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

theme_set(theme_bw())

###########################################
#examine shifts, active hours, and downtime
###########################################
shifts <- taxi_clean %>% group_by(hack_license) %>% 
  arrange(pickup_datetime) %>% 
  mutate(downtime = as.numeric((lead(pickup_datetime) - dropoff_datetime)/3600)) %>% 
  select(pickup_datetime, dropoff_datetime, day_of_the_week, downtime)

##########################################################
#plotting the number of intervals aver the downtime period
##########################################################
downtime_distribution <- 
  ggplot(shifts) + 
  geom_histogram(aes(downtime), binwidth=.25, na.rm = T, color= "black", alpha=.5) + 
  scale_x_log10(label = c("1 minute", "10 minutes", "1 hour", "6 hours"), 
                breaks = c(.01666666666, .1666666666, 1, 6),limits=c(.0016666,100)) +
  xlab("downtime (log scale)") +
  geom_vline(xintercept = 6)
#The cutoff time is 6 hours
ggsave(filename = "../figures/downtime_distribution.png", 
       plot = downtime_distribution)







