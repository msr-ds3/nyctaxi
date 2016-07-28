source("carpool_analysis.R")
library(ggplot2)
library(ggmap)
library(scales)
library(gganimate)

parse_datetime <- function(s, format="%Y-%m-%d") {
  as.POSIXct(as.character(s), format=format)
}

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

map <- get_map(c(-73.87, 40.70), zoom = 11, color="bw")

map_df <- rounded_pickup_dropoff_time_df %>%
  group_by(rounded_pickup_lat, rounded_pickup_lng) %>%
  summarize(count = sum(count)) #%>% filter(count >= 10)

ggmap(map) + 
  geom_point(data=map_df, aes(rounded_pickup_lng, rounded_pickup_lat, color=count)) +
  scale_color_distiller(palette = "Spectral", trans = "log10",
                        breaks = c(1 %o% 10^(0:6)))
ggsave("../figures/carpool_hotspots.png")

##day
map_df_by_ymd <- rounded_pickup_dropoff_time_df %>%
  group_by(rounded_pickup_lat, rounded_pickup_lng, day = parse_datetime(rounded_pickup_datetime)) %>%
  summarize(count = sum(count)) #%>% filter(count >= 10)

plot <- ggmap(map) + 
  geom_point(data=map_df_by_ymd, aes(rounded_pickup_lng, rounded_pickup_lat, color=count, frame=day)) +
  scale_color_distiller(palette = "Spectral", trans = "log10",
                        breaks = c(1 %o% 10^(0:6)))
gg_animate(plot, ani.width=960, ani.height=960, filename = "../figures/heatmap_over_month.gif")


##hour
map_df_by_hour <- rounded_pickup_dropoff_time_df %>%
  group_by(rounded_pickup_lat, rounded_pickup_lng, hour = hour(rounded_pickup_datetime)) %>%
  summarize(count = sum(count)) #%>% filter(count >= 10)

plot <- ggmap(map) + 
  geom_point(data=map_df_by_hour, aes(rounded_pickup_lng, rounded_pickup_lat, color=count, frame=hour)) +
  scale_color_distiller(palette = "Spectral", trans = "log10",
                        breaks = c(1 %o% 10^(0:6)))
gg_animate(plot, ani.width=960, ani.height=960, filename = "../figures/heatmap_by_hour.gif")


##day of week
map_df_by_wday <- rounded_pickup_dropoff_time_df %>%
  group_by(rounded_pickup_lat, rounded_pickup_lng, day = wday(rounded_pickup_datetime)) %>%
  summarize(count = sum(count)) #%>% filter(count >= 10)

plot <- ggmap(map) + 
  geom_point(data=map_df_by_wday, aes(rounded_pickup_lng, rounded_pickup_lat, color=count, frame=day)) +
  scale_color_distiller(palette = "Spectral", trans = "log10",
                        breaks = c(1 %o% 10^(0:6)))
gg_animate(plot, ani.width=960, ani.height=960, filename = "../figures/heatmap_by_wday.gif")




