source("carpool_analysis.R")
library(ggplot2)
library(ggmap)

theme_set(theme_bw())
rounded_pickup_dropoff_time_df <- rounded_pickup_dropoff_time_df %>% 
    mutate(hour = hour(rounded_pickup_datetime))

ggplot(rounded_pickup_dropoff_time_df, aes(rounded_pickup_datetime)) + 
  geom_histogram(color="black", alpha=0.1) +
  xlab("hour of day") + ylab("number of overlapping rides") +
  scale_x_datetime(date_breaks = "2 days")
ggsave("../figures/overlapping_rides_by_ymd.png")

tmp <- rounded_time_time_df %>% filter()
ggplot(rounded_pickup_dropoff_time_df, aes(hour)) + 
   geom_histogram(binwidth = 1,color="black", alpha=0.1) +
  xlab("hour of day") + ylab("number of overlapping rides")
ggsave("../figures/overlapping_rides_by_hour_of_day.png")

map_df <- rounded_pickup_dropoff_time_df %>%
  group_by(rounded_pickup_lat, rounded_pickup_lng) %>%
  summarize(count = sum(count)) %>% filter(count >= 1000)
map <- get_map(c(-73.87, 40.70), zoom = 11)

ggmap(map) + 
  geom_point(data=map_df, aes(rounded_pickup_lng, rounded_pickup_lat, size=log(count)), alpha=0.2)
