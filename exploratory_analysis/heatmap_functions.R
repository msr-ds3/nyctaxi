library(leaflet)
library(tigris)
library(RColorBrewer)

# returns a continous color palette within a range to use for use in leaflet maps
set_pal <- function(range, na_color="#808080", pal = rev(brewer.pal(11, "Spectral")))
{
  colorNumeric(palette = pal, domain = range, na.color = na_color)
}

# returns a map based on parameters. ignore legend_title if no legend needed.
get_map <- function(data, color_by_data, popup_data, pal, boundary_color = "darkblue", boundary_weight=1, opacity=0.4,
                    lng=-73.96, lat=40.75, zoom_level=11, legend_title=NULL, transform=identity, legend_position = "bottomright")
{
  map <- leaflet(data) %>%
    addPolygons(fillColor = ~pal(color_by_data), 
                popup = ~popup_data, 
                weight = boundary_weight, 
                fillOpacity = opacity,
                color=boundary_color) %>% 
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng, lat, zoom = zoom_level)
  if (!is.null(legend_title))
  {
    map <- add_legend(map = map, pal = pal, legend_title = legend_title, values = color_by_data, 
                      opacity = opacity, transform = transform, legend_position = legend_position)
  }
  return(map)
}

add_legend <- function(map, pal, legend_title, values, opacity = 0.4,
                       transform=identity, legend_position = "bottomright")
{
  map %>% addLegend(legend_position, pal = pal, values=values, title = legend_title,
                    opacity = opacity, labFormat = labelFormat(transform =transform))
}

log_transform <- function(x) { round(exp(x)) }

# return transform function based on bool
transform_type <- function(bool){
  if (bool) {
    return(log_transform)
  }
  else { 
    return(identity) 
  }
}

# converts string in form of "#:##" in ints: value = 1 retrieves hours, value = 2 retrieves minutes
get_hr_or_min <- function(s, value){
  as.integer(strsplit(s, ":" )[[1]][value])
}

# converts timestmaps in form of "#:##" into num of minutes since midnight
get_timestamp_in_minutes_from_midnight <- function(s)
{
  get_hr_or_min(s, value = 1) * 60 + get_hr_or_min(s, value = 2)
}

#given neigborhood X, create heatmap by popular destination or origins (based on is_source). Toggle log scale by using is_log
get_map_by_neighborhood <- function(data = taxi_clean, neighborhood  = NULL ,begin = "0:00", end = "23:59", is_source = TRUE, is_log =  TRUE){
  begin <- get_timestamp_in_minutes_from_midnight(begin)
  end <- get_timestamp_in_minutes_from_midnight(end)
  direction <- ifelse(is_source, "From","To")
  group_by_neighborhood <- ifelse(is_source, "dropoff_neighborhood", "pickup_neighborhood")
  
  data <- data %>% mutate(starttime_in_min = hour * 60 + pickup_minute) %>% 
    filter(starttime_in_min > begin & starttime_in_min < end)
  if (!is.null(neighborhood))
  {
    if(is_source){
      data <- data %>% filter(pickup_neighborhood == neighborhood)
   } else {
     data <- data %>% filter(dropoff_neighborhood == neighborhood)
   } 
  }
  data <- data %>% group_by_(group_by_neighborhood) %>% summarize(summary = ifelse(is_log, log(n()/7), n()/7))
  map_data <- geo_join(nyc_neighborhoods, data, "neighborhood", group_by_neighborhood)
  pal <- set_pal(range = range(map_data@data$summary, na.rm = T)) 
  
  get_map(data = map_data, color_by_data = map_data@data$summary, popup_data = map_data@data$neighborhood, 
          pal=pal,  legend_title = paste("Rides", direction, neighborhood), transform = transform_type(is_log))
}