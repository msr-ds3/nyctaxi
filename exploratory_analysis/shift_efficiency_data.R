library(dplyr)

load("shifts.Rdata")

# Add the high threshold for shift efficiency

#SHIFT EFFICIENCY
# create shift_efficiency column
shift_efficiency_no_threshold <- shifts_clean %>% filter(shift_length != 0) %>%
  mutate(shift_efficiency = fare/shift_length)  

# set threshold
threshold <- ceiling(mean(shift_efficiency_no_threshold$total_trips) - 
                       1.5*sd(shift_efficiency_no_threshold$total_trips))

# create thresholded dataframe
shift_efficiency <- shift_efficiency_no_threshold %>% 
  filter(total_trips >= threshold &
           shift_length <= 24 &
           shift_efficiency <= 75 &
           start_shift >= as.POSIXct("2013-07-07 02:00:00") & 
           end_shift <= as.POSIXct("2013-07-13 14:00:00")) #using UTC offset to limit dataframe to full shifts  

# find efficiency stats
avg <- mean(shift_efficiency$shift_efficiency)
sdv <- sd(shift_efficiency$shift_efficiency)

shift_efficiency <- shift_efficiency %>% 
  mutate(efficiency_category = ifelse(shift_efficiency <= avg - sdv, "LOW", 
                                  ifelse(shift_efficiency >= avg + sdv, "HIGH", 
                                         "MEDIUM")))

