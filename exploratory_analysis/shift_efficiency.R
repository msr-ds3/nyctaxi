#SHIFT EFFICIENCY - defined as ratio of total fare amount to shift length

library(ggplot2)
library(dplyr)
library(tidyr)

load("shifts.Rdata")

#SHIFT EFFICIENCY
shift_efficiency = shifts_clean %>% filter(shift_length != 0) %>%
  mutate(shift_efficiency = fare/shift_length)

#plot of shift efficiency
ggplot(shift_efficiency) + geom_density(aes(x=shift_efficiency)) +
  xlim(0,100) +
  geom_vline(xintercept = mean(shift_efficiency$shift_efficiency))

#apply threshold to remove outliers
min_num_of_trips = ceiling(mean(shift_efficiency$total_trips) - 
                             1.5*sd(shift_efficiency$total_trips))

shift_filtered = shift_efficiency %>% 
  filter(total_trips >= min_num_of_trips)

#look at plots after applying threshold - graph more normally distributed
ggplot(shift_filtered) +
  geom_density(aes(x=shift_efficiency)) + 
  xlim(0,100) +
  geom_vline(xintercept = mean(shift_filtered$shift_efficiency))

# HIGH VS. LOW EARNERS ANALYSIS 
avg = mean(shift_filtered$shift_efficiency)
sdv = sd(shift_filtered$shift_efficiency)

source('map_visualization_functions.R')

# LOW EARNERS
low_earners = shift_filtered %>% filter(shift_efficiency <= avg - sdv)

#View random low earner
random_row = sample(1:nrow(low_earners), 1)

visualize_trips_by_shift(taxi_clean_shifts, 
                         low_earners[random_row, ]$hack_license,
                         low_earners[random_row, ]$index)

# HIGH EARNERS
high_earners = shift_filtered %>% filter(shift_efficiency >= avg + sdv)

#View random high earner
random_row = sample(1:nrow(high_earners), 1)

visualize_trips_by_shift(taxi_clean_shifts, 
                         high_earners[random_row, ]$hack_license,
                         high_earners[random_row, ]$index)


