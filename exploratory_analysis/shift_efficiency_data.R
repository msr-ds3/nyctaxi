library(dplyr)

load("shifts.Rdata")

#SHIFT EFFICIENCY
# create shift_efficiency column
shift_efficiency_no_threshold <- shifts_clean %>% filter(shift_length != 0) %>%
  mutate(shift_efficiency = fare/shift_length) 

# set threshold
threshold <- ceiling(mean(shift_efficiency_no_threshold$total_trips) - 
                       1.5*sd(shift_efficiency_no_threshold$total_trips))

# create thresholded dataframe
shift_efficiency <- shift_efficiency_no_threshold %>% 
  filter(total_trips >= threshold)

# find efficiency stats
avg <- mean(shift_efficiency$shift_efficiency)
sdv <- sd(shift_efficiency$shift_efficiency)

# high and low earning shifts dataframes
low_earning_shifts <- shift_efficiency %>% filter(shift_efficiency <= avg - sdv)
high_earning_shifts <- shift_efficiency %>% filter(shift_efficiency >= avg + sdv)
