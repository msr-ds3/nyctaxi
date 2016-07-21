load("shifts.R")
library(ggmap)
library(httr)
library(rgdal)
library(scales)
library(maptools)
library(gganimate)
library(broom)

get_nyc_neighborhoods <- function(){
  r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
  return(readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F))
}
nyc_neighborhoods <- get_nyc_neighborhoods()
nyc_map <- get_map(location = c(lon = -73.95, lat = 40.71), maptype = "terrain", zoom = 11, color="bw")

  # takes a map, a dataframe, a spdf, and a vector of the form c("spdf_id"="df_id") 
# assumes dataframe has column named "summary". assumes spdf has "long" and "lat" 
make_ggmap <- function(df, map, spdf, join_by, boundary_color="black", boundary_thickness=0.25, fill_alpha=0.5,
                       pal = "Spectral", transform = "log10", breaks =c(1 %o% 10^(0:5)), na_color = "#808080", title=waiver())
{
  plot_data <- get_plot_df(spdf, df, join_by)
  ggmap(map) + 
    geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=summary), 
                 color=boundary_color, size = boundary_thickness, alpha=fill_alpha) + 
    scale_fill_distiller(palette = pal, trans = transform, breaks = breaks, na.value = na_color, labels = comma) + 
    theme_nothing(legend = T)
}

# tajes a spdf and a df and joins them
get_plot_df <- function(spdf, df, join_by){
  tidy(spdf, region="neighborhood") %>% 
   left_join(., df, by=join_by)
}

get_map_by_range <- function(df, begin=0, end=24, group_by, filter_by ="pickup_hour"){
  data <- df %>% filter_(paste(filter_by, ">=", begin, "&", filter_by, "<", end)) %>%
    group_by_(group_by) %>%
    summarize(summary = n()/(7*(end-begin)))
  make_ggmap(data, nyc_map, nyc_neighborhoods, join_by = c("id"=group_by), title=begin)
}

get_map_by_driver <- function(df, group_by, driver, shift = NULL){
  data <- df %>% filter(hack_license == driver)
  if (!is.null(shift))
  {
    data <- data %>% filter(index == shift) 
  }
    data <- data %>% 
      group_by_(group_by) %>%
    summarize(summary = n())
  make_ggmap(data, nyc_map, nyc_neighborhoods, join_by = c("id"=group_by), 
             transform = "identity", 
             breaks = seq(1, max(data$summary), max(data$summary)/10))
}

