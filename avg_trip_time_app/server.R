#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
load("../../Rdata/one_month_taxi.Rdata") 

wday_as_abbr <- function(day)
{
 switch(day,
        sunday="Sun",
        monday="Mon",
        tuesday="Tues",
        wednesday="Wed",
        thursday="Thurs",
        friday="Fri",
        saturday="Sat" ) 
}
taxi_clean <- taxi_clean %>%
  mutate(is_weekend = ifelse(day_of_the_week == "Sat" | 
                               day_of_the_week == "Sun", T, F),
         timestamp = as.POSIXct(pickup_hour*3600, origin=origin))

neighborhoods <- levels(taxi_clean$pickup_neighborhood)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$sourceSelector <- renderUI({
    selectInput("source", 
                "Choose a starting neighborhood:", 
                as.list(neighborhoods),
                selected = "Midtown") 
  })
  output$destinationSelector <- renderUI({
    selectInput("destination", 
                "Choose a destination:", 
                as.list(neighborhoods),
                selected = "John F. Kennedy International Airport") 
  })
  
  output$distPlot <- renderPlot({
    
    data <- taxi_clean %>% 
      filter(pickup_neighborhood == input$source & 
               dropoff_neighborhood == input$destination)
    if(input$weekday == "weekday")
    {
      data <- data %>% filter(!is_weekend)
    }
    else if (input$weekday == "weekend")
    {
      data <- data %>% filter(is_weekend)
    }
    else{
      data <- data %>% filter(day_of_the_week == wday_as_abbr(input$weekday))
    }
    
    data %>% 
      group_by(timestamp) %>%
      summarize(pct50 = median(trip_time_in_secs)/60,
                pct10 = quantile(trip_time_in_secs, .10)/60,
                pct25 = quantile(trip_time_in_secs, .25)/60,
                pct75 = quantile(trip_time_in_secs, .75)/60,
                pct90 = quantile(trip_time_in_secs, .90)/60) %>%  
      ggplot(aes(x = timestamp)) +
      geom_line(aes(y= pct50, alpha = "Median"))  +
      geom_ribbon(aes(ymin = pct25, ymax = pct75, alpha = " 25–75th percentile"))+
      geom_ribbon(aes(ymin = pct10, ymax = pct90, alpha = "10–90th percentile")) +
      scale_x_datetime("",
                       labels = date_format("%l %p"),
                       date_breaks = "2 hours"
                       #minor_breaks = "hour"
      ) +
      scale_y_continuous("trip duration in minutes\n") +
      
      expand_limits(y = c(0,90)) +
      theme(legend.position = "bottom") 
    
  })
  
})
