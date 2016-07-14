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