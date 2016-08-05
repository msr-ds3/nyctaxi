### QUESTIONS: #################################
### Can we classify drivers as low/high earners?
### OR is efficiency of a driver due to chance? 
################################################

library(dplyr)
library(ggplot2)
library(lubridate)

load("../Rdata/shifts_design_matrix.Rdata")

med = median(shifts_design_matrix$efficiency)

# 0 - LOW EFFICIENCY  |   1 - HIGH EFFICIENCY
shifts_design_matrix = shifts_design_matrix %>% 
  mutate(efficiency_category = ifelse(efficiency < med, 0, 1))


#####################################################
## VIEW DISTRIBUTION OF NUMSHIFTS TO DETERMINE CUTOFF
#####################################################
num_shifts_distribution = 
  shifts_design_matrix %>% 
  group_by(hack_license) %>%
  summarize(total_shifts = n())
ggplot(num_shifts_distribution) + geom_histogram(aes(x=total_shifts))

min_num_of_shifts = 15


#############################################  
# VIEW REAL DISTRIBUTION OF DRIVER EFFICIENCY
#############################################
driver_efficiency_real =
  shifts_design_matrix %>% 
  group_by(hack_license) %>%
  summarize(
    eff_percent = mean(efficiency_category),
    total_shifts = n()) %>% 
  filter(total_shifts >= min_num_of_shifts)

ggplot(driver_efficiency_real) + 
  geom_histogram(aes(x = eff_percent), bins=101) + 
  ylim(0, 3500)

ggplot(driver_efficiency_real) + 
  geom_density(aes(x=eff_percent)) + 
  
ggsave("../figures/distribution_efficiency_drivers_real.png")


######################################################### 
# SHUFFLE DRIVERS & EFFICIENCIES AND COMPARE DISTRIBUTION
##########################################################
random_hack_licenses = sample(shifts_design_matrix$hack_license, 
                      nrow(shifts_design_matrix))
driver_efficiency_randomized = shifts_design_matrix
driver_efficiency_randomized$hack_license = random_hack_licenses
driver_efficiency_randomized = 
  driver_efficiency_randomized %>% 
  group_by(hack_license) %>%
  summarize(
    eff_percent = mean(efficiency_category),
    total_shifts = n()) %>%
  filter(total_shifts >= min_num_of_shifts)

ggplot(driver_efficiency_randomized) + 
  geom_histogram(aes(x = eff_percent),bins = 101) + 
  ylim(0, 3500)
ggplot(driver_efficiency_randomized, aes(x=eff_percent)) + 
  geom_density()
ggsave("../figures/distribution_efficiency_drivers_random.png")


# Plot with efficiencies for both randomized and real data
ggplot() +
geom_density(data = driver_efficiency_randomized, mapping = aes(x=eff_percent, 
                                                                fill = "red",
                                                                alpha = 1.0
                                                                )) + 
  geom_density(data = driver_efficiency_real, mapping = aes(x=eff_percent,
                                                            fill = "green",
                                                            alpha = 1.0))
ggsave("../figures/distribution_efficiency_drivers_random_and_real.png")

###########################
# INVESTIGATING 0's and 1's
###########################
extreme_drivers = driver_efficiency_real %>% 
  filter(eff_percent == 1 | eff_percent ==0)

# join to see all shifts where eff = 0 | 1
extreme_drivers_full_stats = left_join(extreme_drivers,shifts_design_matrix) 

#see how many drivers belong to each category
ggplot(extreme_drivers, aes(x=eff_percent)) + 
  geom_histogram() +
  xlab("Efficiency Category")

#see how many drivers belong to each category
ggplot(extreme_drivers_full_stats, aes(x=eff_percent)) + 
  geom_histogram() +
  xlab("Efficiency Category")

# View how many shifts each driver had
ggplot(extreme_drivers, aes(x=total_shifts)) + 
  geom_density() +
  facet_wrap(~eff_percent)

# See the number of shifts throughout the month
ggplot(extreme_drivers_full_stats, aes(x=ymd)) + geom_histogram() + 
  facet_wrap(~ efficiency_category)

# ERROR: need to replace night/day with 0/1 in ggplot
ggplot(extreme_drivers_full_stats, aes(x=shift_type)) +
  geom_histogram() +
  facet_wrap(~ efficiency_category)

# View average trip time for drivers shifts
ggplot(extreme_drivers_full_stats, aes(x=avg_trip_time)) +
  geom_histogram() +
  facet_wrap(~ efficiency_category)

# View the number of trips in drivers shifts
ggplot(extreme_drivers_full_stats, aes(x=num_trips)) + 
  geom_histogram() +
  facet_wrap(~ efficiency_category)

# View the occupancy percentage of drivers shifts
ggplot(extreme_drivers_full_stats, aes(x=occupancy_pct)) +
  geom_histogram() +
  facet_wrap(~ efficiency_category) +
  xlim(0,1) #max efficiency is 1

# View the length of driver shifts
ggplot(extreme_drivers_full_stats, aes(x=length)) +
  geom_histogram(binwidth = 1, bins = 16) + 
  facet_wrap(~ efficiency_category) +
  xlab('Shift Length')

# View the total fare of driver shifts
ggplot(extreme_drivers_full_stats, aes(x=total_fare)) +
  geom_histogram() + 
  facet_wrap(~ efficiency_category)

# View the start hour of drivers shifts
ggplot(extreme_drivers_full_stats, aes(x = start_hour)) +
  geom_histogram(bins = 24) +  # one bin per hour
  facet_wrap(~ efficiency_category) 

# low efficient drivers airport %
ggplot(extreme_drivers_full_stats %>%
         filter(efficiency_category == 0), aes(x = airport_pct)) +
  geom_histogram(bins = 10) + 
  xlab("Airport % for INEFFICIENT driver shifts")

# high efficient drivers airport %
ggplot(extreme_drivers_full_stats %>%
         filter(efficiency_category == 1), aes(x = airport_pct)) +
  geom_histogram(bins = 10) + 
  xlab("Airport % for EFFICIENT driver shifts")

# See % of various rate codes for driver shifts
ggplot(extreme_drivers_full_stats, aes(x=rate_code_1_pct)) + 
  geom_histogram() + 
  facet_wrap(~efficiency_category)

# See pickups % in popular nieghborhoods for driver shifts
ggplot(extreme_drivers_full_stats, aes(x=popular_pickup_neighborhood_pct)) + 
  geom_histogram() +
  facet_wrap(~ efficiency_category)


# See dropoffs % in popular nieghborhoods for driver shifts
ggplot(extreme_drivers_full_stats, aes(x=popular_dropoff_neighborhood_pct)) + 
  geom_histogram() +
  facet_wrap(~ efficiency_category)




################  WEATHER  ###################
# look at precipitation for full month
ggplot(extreme_drivers_full_stats, aes(x=ymd, y=prcp)) + 
  geom_point()

# Question: for days with high rain, 
#  how many efficient shifts were there in comparison with the total shifts
ggplot(extreme_drivers_full_stats %>%
         mutate(day = as.factor(day(ymd))) %>%
         group_by(day) %>%
         summarize(
           eff_pct= mean(efficiency_category)
                   ), 
       aes(x=day, y=eff_pct)) + 
  geom_point() 
