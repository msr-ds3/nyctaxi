source("one_week_analysis.R")
source("heatmap_functions.R")

#########################
# pickup heatmap
#########################
# summarize rides by pickup neighborhood
taxi_by_src_nbhd <- taxi_clean %>% group_by(pickup_neighborhood) %>% summarize(logcount = log(n())) 

# overlay rides on map
map_data <- geo_join(nyc_neighborhoods, taxi_by_src_nbhd, "neighborhood", "pickup_neighborhood")

# get color palette
pal <- set_pal(range = range(map_data@data$logcount, na.rm = T))

# get map (transform function translates log vals into human radable vals)
pickup_heatmap <- get_map(data = map_data, 
        color_by_data = map_data@data$logcount, popup_data = map_data@data$neighborhood, 
        pal=pal, legend_title = "pickup locations", transform = log_transform)


#########################
# dropoff heatmap
#########################
# summarize trips by dropoff neighborhoods
taxi_by_dst_nbhd <- taxi_clean %>% group_by(dropoff_neighborhood) %>% summarize(logcount = log(n()))

# overlay rides on map
map_data <- geo_join(nyc_neighborhoods, taxi_by_dst_nbhd, "neighborhood", "dropoff_neighborhood")

# overlay rides on map
pal <- set_pal(range= range(map_data@data$logcount, na.rm=T))

dropoff_heatmap <- get_map(data = map_data, color_by_data = map_data@data$logcount,
                           popup_data = map_data@data$neighborhood, pal=pal, 
                           legend_title = "dropoff locations",transform = log_transform)

##########################################################
# given a neighborhood X, what are destinations?
#########################################################
neighborhood <- "Williamsburg"
src_by_dst <- taxi_clean %>% filter(pickup_neighborhood == neighborhood) %>% group_by(dropoff_neighborhood) %>% summarize(logcount = log(n()))

map_data <- geo_join(nyc_neighborhoods, src_by_dst, "neighborhood", "dropoff_neighborhood")
pal <- set_pal(range = range(map_data@data$logcount, na.rm = T))

from_X_heatmap <- get_map(data = map_data, 
                          color_by_data = map_data@data$logcount, 
                          popup_data = map_data@data$neighborhood, 
                          pal=pal,
                          legend_title = paste("Rides From", neighborhood),transform = log_transform)

##########################################################
# given a neighborhood X, what are the sources?
#########################################################
neighborhood <- "Chelsea"
dst_by_src <- taxi_clean %>% filter(dropoff_neighborhood == neighborhood) %>% group_by(pickup_neighborhood) %>% summarize(logcount = log(n()))

map_data <- geo_join(nyc_neighborhoods, dst_by_src, "neighborhood", "pickup_neighborhood")
pal <- set_pal(range = range(map_data@data$logcount, na.rm = T))

to_X_heatmap <- get_map(data = map_data, 
                          color_by_data = map_data@data$logcount, 
                          popup_data = map_data@data$neighborhood, 
                          pal=pal,
                          legend_title = paste("Rides To", neighborhood),transform = log_transform)

get_map_by_neighborhood <- function(data = taxi_clean, neighborhood ,starttime = "0:00", endtime = "23:59", is_source = TRUE, is_log =  TRUE){
  exp <- lazyeval::interp(quote(x == y), x = as.name(ifelse(is_source,"pickup_neighborhood", "dropoff_neighborhood")), y=neighborhood)
  
  
  starttime <- get_hr_or_min(starttime, value = 1) * 60 + get_hr_or_min(starttime, value = 2)
  endtime <- get_hr_or_min(endtime, value = 1) * 60 + get_hr_or_min(endtime, value = 2)
  data <- data %>% mutate(starttime_in_min = hour * 60 + pickup_minute, endtime_in_min = dropoff_hour * 60 + dropoff_minute) %>% 
                  filter(starttime_in_min > starttime & endtime_in_min < endtime) %>%
    filter_(exp) %>%
    group_by_(ifelse(is_source, "dropoff_neighborhood", "pickup_neighborhood")) %>% summarize(summary = ifelse(is_log, log(n()), n()))
    map_data <- geo_join(nyc_neighborhoods, data, "neighborhood", ifelse(is_source, "dropoff_neighborhood", "pickup_neighborhood"))
    pal <- set_pal(range = range(map_data@data$summary, na.rm = T)) 
    direction <- ifelse(is_source, "From","To")
    get_map(data = map_data, 
          color_by_data = map_data@data$summary, 
          popup_data = map_data@data$neighborhood, 
          pal=pal,
          legend_title = paste("Rides",  direction, neighborhood),transform = transform_type(is_log))
}
 