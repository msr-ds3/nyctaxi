library(shiny)
library(ggplot2)
load("../exploratory_analysis/one_week_taxi.Rdata")

shinyServer(function(input, output) {
   
  output$map <- renderLeaflet({
    
    log_transform <- function(x) { round(10^(x)) }
    
    range <- round(as.numeric(difftime(max(taxi_clean$pickup_datetime),
                        min(taxi_clean$pickup_datetime), 
                        unit="days")))
    
    data <- taxi_clean
    if(input$neighborhood != "Entire NYC")
    {
      if (input$type == "pickup"){
        data <- data %>% filter(dropoff_neighborhood == input$neighborhood)
      } else {
        data <- data %>% filter(pickup_neighborhood == input$neighborhood)
      }
    }
    if (input$type == "pickup")
    {
      data <- data %>% 
        mutate(neighborhood = pickup_neighborhood,
               hour = pickup_hour)
    } else {
      data <- data %>% mutate(neighborhood = dropoff_neighborhood,
                                    hour = dropoff_hour)
    }   
        
        data <- data %>%
          filter(hour == input$hour) %>%
        group_by(neighborhood) %>% 
        summarize(num_trips = log10(n()/range))
      
      map_data <- geo_join(nyc_neighborhoods, data, 
                           "neighborhood", "neighborhood")
    
    
    pal <- colorBin(palette = rev(brewer.pal(11, "Spectral")),
                    domain = -1:4, na.color = "#808080")
    
    
    leaflet(map_data) %>%
      addPolygons(fillColor = ~pal(num_trips), 
                  popup = ~neighborhood,
                  weight = 1, 
                  fillOpacity = 0.4,
                  color="darkblue") %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.85, 40.71, zoom = 10) %>% 
      add_legend(pal = pal, 
                   legend_title = "number of trips", 
                   values = map_data$num_trips,
                   opacity = 0.4, 
                   transform = log_transform, 
                   legend_position = "bottomright")
  })
  
})
