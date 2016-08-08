library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(data.table)
library(sp)
library(rgdal)
library(maptools)
library(KernSmooth)
load("probability.Rdata")


shinyServer(function(input, output) {
  
  # render basic map
  output$map <- renderLeaflet({
    leaflet() %>%
      #addTiles() %>%
      #addProviderTiles("CartoDB.DarkMatter") %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.85, 40.71, zoom = 11) 
  })
  
  # reactive to filter data on demand by hour and pickup neighborhood
  filter_data <- reactive({
    set.seed(42)
    data <- probability %>%
      filter(pickup_hour == input$hour, 
             pickup_neighborhood == input$neighborhood,
             is_weekend == (input$is_weekend == "Weekend")) %>%
      filter(!is.na(dropoff_lat) & !is.na(dropoff_lng))
    
    df <- sample_n(data, sum(data$n), replace=T, weight=n)
    
    kde <- bkde2D(as.data.table(df)[ , list(dropoff_lng, dropoff_lat)],
                  bandwidth=c(.0045, .0068), gridsize = c(100,100))
    CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
    return(CL) 
    })
    
    get_levels <- function(CL){
      levs <- as.factor(sapply(CL, `[[`, "level"))
      return(levs)
    }
    ## EXTRACT CONTOUR LINE LEVELS
   
    get_spgons <- function(CL){
      
    ## CONVERT CONTOUR LINES TO POLYGONS
    pgons <- lapply(1:length(CL), function(i)
      Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
    spgons = SpatialPolygons(pgons)
    return(spgons)
  }
  
  # query reactive to get changed options
  observe({
    CL <- filter_data()
    data <- get_spgons(CL)
    levs <- get_levels(CL)
    n_levels <- length(levels(levs))
    leafletProxy("map", data = data) %>%
      clearMarkers() %>% 
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor  = rev(heat.colors(n_levels, NULL)[levs]), 
                  color = "black", weight = .1)
  })
})