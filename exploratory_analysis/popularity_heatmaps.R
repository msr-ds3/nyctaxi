library(leaflet)
library(tigris)
library(colorspace)

source("one_week_analysis.R")

##################################
# get some functions out of the way
##################################
# returns a continous color palette within a range
set_pal <- function(range, na_color="#808080")
{
  colorNumeric(palette = diverge_hsv(max(range)),
               domain = range, na.color = na_color)
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
  map %>% addLegend(legend_position, pal = pal, values=values,
                           title = legend_title,
                           opacity = opacity,
                           labFormat = labelFormat(transform =transform))
}

#########################
# pickup heatmap
#########################
# summarize rides by pickup neighborhood
taxi_by_src_nbhd <- taxi_clean %>% group_by(pickup_neighborhood) %>% summarize(logcount = log(n())) 

# overlay rides on map
map_data <- geo_join(nyc_neighborhoods, taxi_by_src_nbhd, "neighborhood", "pickup_neighborhood")

# get color palette
pal <- set_pal(range = range(map_data@data$logcount, na.rm = T), na_color = "green")

# get map (transform function translates log vals into human radable vals)
pickup_heatmap <- get_map(data = map_data, 
        color_by_data = map_data@data$logcount, 
        popup_data = map_data@data$neighborhood, 
        pal=pal,
        legend_title = "pickup",
        transform = function(x) round(exp(x)))


#########################
# dropoff heatmap
#########################
# summarize trips by dropoff neighborhoods
taxi_by_dst_nbhd <- taxi_clean %>% group_by(dropoff_neighborhood) %>% summarize(logcount = log(n()))

# overlay rides on map
map_data <- geo_join(nyc_neighborhoods, taxi_by_dst_nbhd, "neighborhood", "dropoff_neighborhood")

# overlay rides on map
pal <- set_pal(range= range(map_data@data$logcount, na.rm=T), na_color = "green")

dropoff_heatmap <- get_map(data = map_data, color_by_data = map_data@data$logcount,
                           popup_data = map_data@data$neighborhood, pal=pal, 
                           legend_title = "dropoff",transform = function(x) round(exp(x)))

##########################################################
# given a neighborhood X, what are destinations?
#########################################################
neighborhood <- "Williamsburg"
src_by_dst <- taxi_clean %>% filter(pickup_neighborhood == neighborhood) %>% group_by(dropoff_neighborhood) %>% summarize(logcount = log(n()))

map_data <- geo_join(nyc_neighborhoods, src_by_dst, "neighborhood", "dropoff_neighborhood")
pal <- set_pal(range = range(map_data@data$logcount, na.rm = T), na_color = "green")

from_X_heatmap <- get_map(data = map_data, 
                          color_by_data = map_data@data$logcount, 
                          popup_data = map_data@data$neighborhood, 
                          pal=pal,
                          legend_title = paste("from", neighborhood),transform = function(x) round(exp(x)))

##########################################################
# given a neighborhood X, what are the sources?
#########################################################
neighborhood <- "Chelsea"
dst_by_src <- taxi_clean %>% filter(dropoff_neighborhood == neighborhood) %>% group_by(pickup_neighborhood) %>% summarize(logcount = log(n()))

map_data <- geo_join(nyc_neighborhoods, dst_by_src, "neighborhood", "pickup_neighborhood")
pal <- set_pal(range = range(map_data@data$logcount, na.rm = T), na_color = "green")

to_X_heatmap <- get_map(data = map_data, 
                          color_by_data = map_data@data$logcount, 
                          popup_data = map_data@data$neighborhood, 
                          pal=pal,
                          legend_title = paste("to", neighborhood),transform = function(x) round(exp(x)))
