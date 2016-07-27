library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
load("../Rdata/one_month_taxi.Rdata") 

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
         timestamp = as.POSIXct(pickup_hour*3600, origin=origin, tz= "EDT"))

neighborhoods <- levels(taxi_clean$pickup_neighborhood)
# Define server logic required to draw a plot
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
    
    data <-data %>% 
      group_by(timestamp) %>%
      summarize(pct50 = median(trip_time_in_secs)/60,
                pct10 = quantile(trip_time_in_secs, .10)/60,
                pct25 = quantile(trip_time_in_secs, .25)/60,
                pct75 = quantile(trip_time_in_secs, .75)/60,
                pct90 = quantile(trip_time_in_secs, .90)/60)
    
    if(nrow(data) < 2){
      ggplot(data) +
        annotate("text", x=5, y= 5, label="Insufficient Data to produce plot")
    }
    else
    {
      ggplot(data, aes(x = timestamp)) +
        geom_line(aes(y= pct50),  alpha = 1) +
        geom_ribbon(aes(ymin = pct25, ymax = pct75), alpha = 0.4)+
        geom_ribbon(aes(ymin = pct10, ymax = pct90), alpha = 0.2) +
        ylab("trip duration in minutes\n")+ 
        xlab("time of the day") +
        ylim(0,90) + 
        scale_x_datetime(date_breaks = "2 hours" ,
                         date_minor_breaks = "1 hour",
                         date_labels = "%l%p")
    }
  })
  
})
