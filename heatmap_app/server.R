library(shiny)
library(dplyr)
library(httr)
library(rgdal)
library(leaflet)
library(tigris)
library(RColorBrewer)
load("../exploratory_analysis/one_week_taxi.Rdata")

shinyServer(function(input, output) {
   
  output$map <- renderLeaflet({
    
    log_transform <- function(x) { round(10^(x)) }
    
    range <- round(as.numeric(difftime(max(taxi_clean$pickup_datetime),
                        min(taxi_clean$pickup_datetime), 
                        unit="days")))
    
    get_nyc_neighborhoods <- function(){
      r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
      return(readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F))
    }
    
    nyc_neighborhoods <- get_nyc_neighborhoods()
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
          filter(hour >= input$hour[1] & hour <= input$hour[2]) %>%
        group_by(neighborhood) %>% 
        summarize(num_trips = log10(n()/range))
      
      map_data <- geo_join(nyc_neighborhoods, data, 
                           "neighborhood", "neighborhood")
    
    
    pal <- colorBin(palette = rev(brewer.pal(11, "Spectral")),
                    domain = -1:5, na.color = "#808080")
    
    
    leaflet(map_data) %>%
      addPolygons(fillColor = ~pal(num_trips), 
                  popup = ~neighborhood,
                  weight = 1, 
                  fillOpacity = 0.4,
                  color="darkblue") %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.85, 40.71, zoom = 10) %>% 
      addLegend(pal = pal, 
                   title = "number of trips", 
                   values = map_data$num_trips,
                   opacity = 0.4, 
                   labFormat = labelFormat(transform = log_transform), 
                   position = "bottomright")
  })
  
})
