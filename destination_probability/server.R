library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
load("probability.Rdata")

set_pal <- function(range, na_color="#808080", pal = brewer.pal(11, "Spectral"))
{
  colorNumeric(palette = rev(pal), domain = range, na.color = na_color)
}


shinyServer(function(input, output) {
  
  # render basic map
    output$map <- renderLeaflet({
        leaflet() %>%
        #addTiles() %>%
        #addProviderTiles("CartoDB.DarkMatter") %>%
        addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
        setView(-73.85, 40.71, zoom = 11) 
    })
    
    # reactive to filter data on demand by hour and pickup neighborhood
    filter_data <- reactive({
      data <- probability %>%
        filter(pickup_hour == input$hour, 
               pickup_neighborhood == input$neighborhood,
               is_weekend == (input$is_weekend == "Weekend"))
        return(data)
    })
    
    # query reactive to get changed options
    observe({
      col_name <- switch(input$type,
             "Popular destinations" = "probability",
             "Unusual destinations (method a)" = "a",
             "Unusual destinations (method b)" = "b",
             "Unusual destinations (method c)" = "c",
             ... = "probability")
      
      data <- filter_data()  %>%
        rename_("x" = col_name) %>%
        top_n(input$n, wt=x)
      pal <- set_pal(range = range(data$x))
      
      leafletProxy("map", data = data) %>%
        clearMarkers() %>% 
        clearShapes() %>%
        clearControls() %>%
        addCircles(lng = ~dropoff_lng,
                   lat=~dropoff_lat,
                   radius = 425,
                   color = "white",
                   weight = 2,
                   opacity =8,
                   popup = ~sprintf("%g", x*100),
                   fillOpacity = .6,
                   fillColor = ~pal(x)) 
    })
})