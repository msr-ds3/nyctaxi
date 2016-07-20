load("shifts.Rdata")
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

theme_set(theme_minimal())

#shifts_clean_shif_less_24 <- shifts_clean %>% filter(shift_length < 24)

#########################################
## num shift per week:
########################################
shifts_clean_num_shift <- shifts_clean %>% group_by(hack_license) %>% 
                                          summarize(num_shift = n())

ggplot(shifts_clean_num_shift, aes(num_shift)) + geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:10)

## Shift length:
ggplot(shifts_clean, aes(shift_length)) + geom_density() + xlim(0,24)

#######################################
### Discriptive shift stats
#######################################
### Total trips:

shifts_clean_num_trips <- shifts_clean %>% group_by(total_trips) %>% 
   summarize(num_shifts = n())

ggplot(shifts_clean_num_trips, aes(x= total_trips, y = num_shifts)) + 
                                                    geom_point() +
                                           geom_vline(xintercept = 20)

### Total fare amount
shifts_clean_fare_amount <- shifts_clean %>% group_by(fare) %>% 
  summarize(num_shifts= n())

ggplot(shifts_clean_fare_amount, aes(x = revenue, y = num_shifts)) + 
                                           geom_point() + xlim(0,1000)

### Total distance:
shifts_clean_total_distance<- shifts_clean %>% group_by(total_distance) %>% 
  summarize(num_shifts= n())

ggplot(shifts_clean_total_distance, aes(x = total_distance, y = num_shifts)) + 
                                                  geom_point() + xlim(0,250)



