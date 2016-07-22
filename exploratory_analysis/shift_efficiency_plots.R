#SHIFT EFFICIENCY - defined as ratio of total fare amount to shift length

library(ggplot2)
library(dplyr)
library(tidyr)

source("shift_efficiency_data.R")
#SHIFT EFFICIENCY

#plot of shift efficiency
ggplot(shift_efficiency_no_threshold) + 
  geom_density(aes(x=shift_efficiency)) +
  xlim(0,100) +
  geom_vline(xintercept = mean(shift_efficiency_no_threshold$shift_efficiency))


#look at plots after applying threshold - graph more normally distributed
ggplot(shift_efficiency) +
  geom_density(aes(x=shift_efficiency)) + 
  xlim(0,100) +
  geom_vline(xintercept = mean(shift_efficiency$shift_efficiency))

source('map_visualization_functions.R')

#View random low earner
random_row = sample(1:nrow(low_earning_shifts), 1)

visualize_trips_by_shift(taxi_clean_shifts, 
                         low_earning_shifts[random_row, ]$hack_license,
                         low_earning_shifts[random_row, ]$shift_num)


#View random high earner
random_row = sample(1:nrow(high_earning_shifts), 1)

visualize_trips_by_shift(taxi_clean_shifts, 
                         high_earning_shifts[random_row, ]$hack_license,
                         high_earning_shifts[random_row, ]$shift_num)

#####################################################
#Plotting the shift efficiency over the shift length
#####################################################
shift_efficiency_vs_shift_length <- ggplot(shift_efficiency, 
                                           aes(x = shift_length, y = shift_efficiency)) + geom_point() + ylim(0,100) + 
  geom_hline(yintercept = mean(shift_efficiency$shift_efficiency)) + 
  geom_vline( xintercept = mean(shift_efficiency$shift_length)) + xlim(0,24) + 
  xlab("shift length") + ylab("shift efficiecncy")

ggsave('../figures/shift_efficiency_vs_shift_length.png', 
       plot = shift_efficiency_vs_shift_length)

