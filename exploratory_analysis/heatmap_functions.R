library(leaflet)
library(tigris)
library(RColorBrewer)

# returns a continous color palette within a range to use for use in leaflet maps
set_pal <- function(range, na_color="#808080", pal = rev(brewer.pal(11, "Spectral")))
{
  colorNumeric(palette = pal, domain = range, na.color = na_color)
}

set_quanpal <- function(range, na_color="#808080", pal = rev(brewer.pal(11, "Spectral")))
{
  colorQuantile(palette = pal, domain = range, na.color = na_color, n=11)
}

set_binpal <- function(range, na_color="#808080", pal = rev(brewer.pal(11, "Spectral")))
{
  colorBin(palette = pal, domain = range, na.color = na_color, bins=11)
}

# returns a map based on parameters. ignore legend_title if no legend needed.
get_leaflet_map <- function(data, color_by_data, popup_data, pal, boundary_color = "darkblue", boundary_weight=1, opacity=0.4,
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

log_transform <- function(x) { round(10^(x)) }

# return transform function based on bool
transform_type <- function(bool){
  if (bool) {
    return(log_transform)
  }
  else { 
    return(identity) 
  }
}

#given neigborhood X (or the Null Neighborhood), create heatmap by popular destination or origins (based on is_source). 
#Toggle log scale by using is_log
get_map_by_neighborhood <- function(data = taxi_clean, neighborhood  = NULL ,begin = 0, end = 24, is_source = TRUE, is_log =  TRUE){
  
  direction <- ifelse(is_source, "From","To")
  neighborhoods_of_interest <- ifelse(is_source, "dropoff_neighborhood", "pickup_neighborhood")
  
  data <- filter_by_range(data, begin, end)
  
  if (!is.null(neighborhood)) { data <- filter_by_neighborhood(data, neighborhood, is_source) }
  else { neighborhood <- "Everywhere"}
  
  data <- data %>% group_by_(neighborhoods_of_interest) %>% summarize(summary = ifelse(is_log, log10(n()/(7*(end-begin))), n()/(7*(end-begin))))
  
  map_data <- geo_join(nyc_neighborhoods, data, "neighborhood", neighborhoods_of_interest)
 
  pal <- set_binpal(range =map_data@data$summary)
  
  get_leaflet_map(data = map_data, color_by_data = map_data@data$summary, popup_data = map_data@data$neighborhood, 
          pal=pal,  legend_title = paste("Rides", direction, neighborhood), transform = transform_type(is_log))
}

filter_by_neighborhood <- function(data, neighborhood, is_source) {
  if (is_source) {
    data %>% filter(pickup_neighborhood == neighborhood)
  } else {
    data %>% filter(dropoff_neighborhood == neighborhood)
  }
}

filter_by_range <- function(data, begin, end) {
  data %>% filter(hour >= begin & hour < end)
}