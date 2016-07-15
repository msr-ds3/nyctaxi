source("one_week_analysis.R")
library(ggmap)
library(scales)
library(maptools)

nyc_map <- get_map(location = c(lon = -73.95, lat = 40.71), maptype = "terrain", zoom = 10, color="bw")

# takes a map, a dataframe, a spdf, and a vector of the form c("spdf_id"="df_id") 
# assumes dataframe has column named "summary". assumes spdf has "long" and "lat" 
make_ggmap <- function(df, map, spdf, join_by, boundary_color="black", boundary_thickness=0.25, fill_alpha=0.5,
                       pal = "Spectral", transform = "log10", breaks =c(1 %o% 10^(0:5)), na_color = "#808080")
{
  plot_data <- get_plot_df(spdf, df, join_by)
  
  ggmap(map) + 
    geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=summary), color=boundary_color, size = boundary_thickness, alpha=fill_alpha) + 
    scale_fill_distiller(palette = pal, trans = transform, breaks = breaks, na.value = na_color, labels = comma) +
    theme_nothing(legend = T)
}

# tajes a spdf and a df and joins them
get_plot_df <- function(spdf, df, join_by){
  tidy(spdf, region="neighborhood") %>% 
   left_join(., df, by=join_by)
}

get_map_by_range <- function(df, begin=0, end=24, group_by, filter_by ="hour"){
  data <- df %>% filter_(paste(filter_by, ">=", begin, "&", filter_by, "<", end)) %>%
    group_by_(group_by) %>%
    summarize(summary = n()/(7*(end-begin)))
  make_ggmap(data, nyc_map, nyc_neighborhoods, join_by = c("id"=group_by))
}


get_map_by_range(taxi_clean, begin = 3, end = 4, group_by = "dropoff_neighborhood")

