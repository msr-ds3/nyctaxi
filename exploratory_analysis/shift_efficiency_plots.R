#SHIFT EFFICIENCY - defined as ratio of total fare amount to shift length

library(ggplot2)
library(dplyr)
library(tidyr)

source("shift_efficiency_data.R")
source('map_visualization_functions.R')


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
ggsave("../figures/shift_efficiency_distribution.png")


#View random low earner
low_earning_shifts <- shift_efficiency %>% filter(efficiency_category == "LOW")
random_row = sample(1:nrow(low_earning_shifts), 1)

visualize_trips_by_shift(taxi_clean_shifts, 
                         low_earning_shifts[random_row, ]$hack_license,
                         low_earning_shifts[random_row, ]$shift_num)


#View random high earner

low_earning_shifts <- shift_efficiency %>% filter(efficiency_category == "HIGH")
random_row = sample(1:nrow(high_earning_shifts), 1)

visualize_trips_by_shift(taxi_clean_shifts, 
                         high_earning_shifts[random_row, ]$hack_license,
                         high_earning_shifts[random_row, ]$shift_num)

#####################################################
#Plotting the shift efficiency over the shift length
#####################################################
shift_efficiency_vs_shift_length <- 
  ggplot(shift_efficiency, 
         aes(x = shift_length, y = shift_efficiency, color = efficiency_category)) + 
  geom_point() + xlab("shift length") + ylab("shift efficiecncy")

ggsave('../figures/shift_efficiency_vs_shift_length.png', 
       plot = shift_efficiency_vs_shift_length)

###################################################
#Plotting the shift length for high and low earners
###################################################
low_and_high_earners_shift_efficiency <- 
  ggplot(shift_efficiency, aes(x = shift_length, color = efficiency_category))+
  geom_density() 

ggsave('../figures/low_and_high_earners_shift_efficiency.png', 
       plot = low_and_high_earners_shift_efficiency)

#############################################
#Plotting the distribution of the total trips
#############################################
distribution_pf_the_total_trips <-ggplot(shift_efficiency, aes(x = total_trips, color = efficiency_category)) + 
  geom_density()

ggsave('../figures/distribution_pf_the_total_trips.png', 
       plot = distribution_pf_the_total_trips)

