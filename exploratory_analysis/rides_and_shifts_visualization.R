load("one_week_taxi.Rdata")
load("shifts.Rdata")
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

theme_set(theme_minimal())
######################################################################
## see pickups and dropoffs for hundred random taixs throughout the week
#####################################################################

#get n random drivers
n_random_drivers <- function(x = taxi_clean_shifts, n = 100)
{
  sample(unique(x$hack_license), n)
}

#filter dataframe by random drivers
filter_df_by_drivers <- function(x, random_drivers)
{
  x %>% filter(hack_license %in% random_drivers)
}

# get driver levels arranged by starttime
get_driver_levels <- function(filtered_df)
{
  filtered_df %>% 
    group_by(hack_license) %>%
    summarize(level = min(pickup_datetime)) %>% 
    arrange(level)
}

# set levels to use as factor
set_driver_levels <- function(x, levels)
{
  factor(x$hack_license, levels = rev(levels$hack_license))
}

# plot shifts and rides
plot_ <- function(rides, shifts, n = 100)
{
  ggplot() +
  geom_segment(data =rides, 
               aes(x=pickup_datetime, xend=dropoff_datetime, 
                   y =as.factor(hack_license), yend=as.factor(hack_license)),
               size = 2) + 
  geom_segment(data =shifts,
               aes(x=start_shift, xend=end_shift, 
                   y=as.factor(hack_license), yend=as.factor(hack_license)),
               color="red", alpha=0.3, size = 2) +
  scale_shape_manual(values = c(4, 1)) +
  scale_x_datetime(date_breaks = "2 hour",labels=date_format("%H")) + 
  scale_y_discrete(labels= 1:n) + 
  xlab("hour of day") + 
  ylab(paste(n, "random drivers"))
}

###############
# main function
##############
visualize_rides_and_shifts <- function(x = taxi_clean_shifts, n=100)
{
  random_drivers <- n_random_drivers(x = x, n = n)
  rides <- filter_df_by_drivers(x = x, 
                                random_drivers = random_drivers)
  shifts <- filter_df_by_drivers(x = shifts_clean, 
                                 random_drivers = random_drivers)
  levels <- get_driver_levels(rides)
  rides$hack_license <- set_driver_levels(rides, levels)
  shifs$hack_license <- set_driver_levels(shifts, levels)
  plot_(rides, shifts, n)
}