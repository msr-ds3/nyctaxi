library(dplyr)

load("../Rdata/shifts.Rdata")

# Add the high threshold for shift efficiency

#SHIFT EFFICIENCY
# create shift_efficiency column
shift_efficiency_no_threshold <- shifts_clean %>% filter(shift_length != 0) %>%
  mutate(shift_efficiency = fare/shift_length)  

# set thresholds
min_num_of_trips <- ceiling(mean(shift_efficiency_no_threshold$total_trips) - 
                       1.5*sd(shift_efficiency_no_threshold$total_trips))
max_shift_length = 24 
max_shift_efficiency = 75

#using UTC offset to limit dataframe to full shifts
min_shift_start_time = as.POSIXct("2013-07-07 02:00:00")
max_shift_end_time = as.POSIXct("2013-07-13 14:00:00")

# apply threshold to data frame
shift_efficiency <- shift_efficiency_no_threshold %>% 
  filter(total_trips >= min_num_of_trips &
           shift_length <= max_shift_length &
           shift_efficiency <= max_shift_efficiency &
           start_shift >= min_shift_start_time & 
           end_shift <= max_shift_end_time
        )

# find efficiency stats
avg <- mean(shift_efficiency$shift_efficiency)
sdv <- sd(shift_efficiency$shift_efficiency)

shift_efficiency <- shift_efficiency %>% 
  mutate(efficiency_category = ifelse(shift_efficiency <= avg - sdv, "LOW", 
                                  ifelse(shift_efficiency >= avg + sdv, "HIGH", 
                                         "MEDIUM")))

