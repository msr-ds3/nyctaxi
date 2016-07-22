load("shifts.Rdata")
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

theme_set(theme_minimal())

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
### Descriptive shift stats
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

ggplot(shifts_clean_fare_amount, aes(x = fare, y = num_shifts)) + 
                                           geom_point() + xlim(0,1000)
ggplot(shifts_clean) + geom_histogram(aes(fare)) + xlim(0,500)

### Total distance:
shifts_clean_total_distance<- shifts_clean %>% mutate(bin= round(total_distance /5)*5) %>% group_by(bin) %>% 
  summarize(num_shifts= n())

ggplot(shifts_clean_total_distance, aes(x = bin, y = num_shifts)) + 
                                                  geom_point() + xlim(0,250)



