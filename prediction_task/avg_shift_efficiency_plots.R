library(ggplot2)
library(dplyr)
library(lubridate)

theme_set(theme_bw())
# load shifts summary df
load("../Rdata/shifts_design_matrix.Rdata")

# group by day and hour and compute avg efficiency 
shifts_design_matrix %>% 
  mutate(ymd_h = ymd_h(paste(date(start), hour(start), sep=" "))) %>% 
  group_by(ymd_h) %>% 
  summarize(avg_eff = mean(efficiency)) %>%
  ggplot() + 
  geom_point(aes(ymd_h, avg_eff)) + 
  geom_smooth(aes(ymd_h, avg_eff)) +
  xlab("date and hour of day") +
  ylab("average efficiency")
ggsave("../figures/avg_shift_eff_by_day_and_hour.png")

shifts_design_matrix %>% 
  group_by(ymd) %>% 
  summarize(avg_eff = mean(efficiency)) %>%
  ggplot() + 
  geom_point(aes(ymd, avg_eff)) + 
  geom_smooth(aes(ymd, avg_eff)) +
  xlab("date") +
  ylab("average efficiency")
ggsave("../figures/avg_shift_eff_by_day.png")

shifts_design_matrix %>% 
  group_by(hour = hour(start)) %>% 
  summarize(avg_eff = mean(efficiency)) %>%
  ggplot() + 
  geom_point(aes(hour, avg_eff)) + 
  geom_smooth(aes(hour, avg_eff)) +
  xlab("hour of day") +
  ylab("average efficiency") +
  geom_hline(yintercept = mean(shifts_design_matrix$efficiency))
ggsave("../figures/avg_shift_eff_by_day.png")
