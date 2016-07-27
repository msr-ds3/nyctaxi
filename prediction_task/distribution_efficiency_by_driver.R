### QUESTIONS: #################################
### Can we classify drivers as low/high earners?
### OR is efficiency of a driver due to chance? 
################################################

library(dplyr)
library(ggplot2)

load("../Rdata/shifts_design_matrix.Rdata")

med = median(shifts_design_matrix$efficiency)

# 0 - LOW EFFICIENCY  |   1 - HIGH EFFICIENCY
shifts_design_matrix = shifts_design_matrix %>% 
  mutate(efficiency_category = ifelse(efficiency < med, 0, 1))


#####################################################
## VIEW DISTRIBUTION OF NUMSHIFTS TO DETERMINE CUTOFF

num_shifts_distribution = 
  shifts_design_matrix %>% 
  group_by(hack_license) %>%
  summarize(total_shifts = n())
ggplot(num_shifts_distribution) + geom_histogram(aes(x=total_shifts))

min_num_of_shifts = 15


#############################################  
# VIEW REAL DISTRIBUTION OF DRIVER EFFICIENCY

driver_efficiency_real =
  shifts_design_matrix %>% 
  group_by(hack_license) %>%
  summarize(
    eff_percent = mean(efficiency_category),
    total_shifts = max(shift_num)) %>% 
  filter(total_shifts >= min_num_of_shifts)

ggplot(driver_efficiency_real) + 
  geom_histogram(aes(x = eff_percent), bins=101) + 
  ylim(0, 3500)
ggsave("../figures/distribution_efficiency_drivers_real.png")


######################################################### 
# SHUFFLE DRIVERS & EFFICIENCIES AND COMPARE DISTRIBUTION

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
ggsave("../figures/distribution_efficiency_drivers_random.png")


###################################
# INVESTIGATING 0's and 1's

extreme_drivers = driver_efficiency_real %>% filter(eff_percent == 1 | eff_percent ==0)
# join to see all shifts where eff = 0 | 1
extreme_drivers_full_stats = left_join(extreme_drivers,shifts_design_matrix) 
ggplot(extreme_drivers_full_stats, aes(x=ymd)) + geom_histogram() + 
  facet_wrap(~ efficiency_category)

ggplot(extreme_drivers_full_stats, aes(x=shift_type)) +
  geom_histogram() +
  facet_wrap(~ efficiency_category)

