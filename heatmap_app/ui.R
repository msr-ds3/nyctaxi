library(shiny)
library(leaflet)
load("../Rdata/one_month_taxi.Rdata")
# get list of neighborhoods to display in ui
neighborhoods <- levels(taxi_clean$pickup_neighborhood)

# append Entire NYC as an option
neighborhoods <- append("Entire NYC", neighborhoods)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("NYC Taxi Flow"),
  
  
  helpText(HTML("<p>Every day, thousands of taxicabs travel throughout NYC. Ever wondered
           where all these taxis come from? Or where they are headed?
           Wonder no more. Choose the stop type and hour of day (or range of hours)
           to visualize
           the flow of taxis in NYC as a heatmap.</p>
           <p>You can view dropoffs (or pickups) coming from (or going to) the 
           entire city, or you can choose to view only the dropoffs (or pickups)
           that are coming from (or going to) a specific neighborhood.</p>
           <p>The number of trips are averaged over the month of July 2013.</p>")),
  # Sidebar with a dropdown for neighborhoods, a dropdown for stop types, and a
  # slider input for hour of day 
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood",
                  "Choose a neighborhood:",
                  as.list(neighborhoods),
                  selected ="Entire NYC" 
                  ),
      selectInput("type",
                  "Type of stop:",
                  list("dropoff", "pickup")
                  ),
       sliderInput("hour",
                   "Hour of day",
                   min = 0,
                   max = 23,
                   value = range(9:12), 
                   step = 1,
                   animate = T)
    ),
    
    # Show the generated map
    mainPanel(
       leafletOutput("map")
    )
  )
))
