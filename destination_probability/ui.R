library(shiny)
library(leaflet)
library(dplyr)
load("probability.Rdata")
# we limit our app to only look at places that had >= 25 rides/hour
# we include only the neighborhoods for which we have information 
# for all 24 hours both during the week and during weekends
neighborhoods <- probability %>% 
  filter(total >= 25) %>% 
  group_by(pickup_neighborhood, pickup_hour, is_weekend) %>%
  summarize(count = n()) %>%
  group_by(pickup_neighborhood) %>%
  summarize(hours = n()) %>% 
  filter(hours == 48)

neighborhoods <- levels(factor(neighborhoods$pickup_neighborhood))
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Where Do People Go?"),
  helpText(HTML('<p>Every day, thousands of yellow taxis travel throughout NYC. Ever wondered
           where all these taxis are going?
           Wonder no more. Using <a href="http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml">publicly available data from the TLC</a>, we created a
            tool to see the distribution of popular destinations for taxi rides.</p>
<p>Choose the neigborhood and time of day to visualize
           the most popular taxi destinations in NYC as a heatmap.</p>
           <p>The data used for this visualization is from July 2013, and was 
            created by <a href="https://ds3.research.microsoft.com/">MSR DS3</a> 2016 students. 
                See our <a href="https://github.com/msr-ds3/nyctaxi">github</a> 
          repo for more details.</p>')),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood",
                  "Choose a neighborhood:",
                  choices = neighborhoods,
                  selected = "Upper East Side"),
      radioButtons("is_weekend",
                   "Choose time of the week:",
                   choices = c("Weekend", "Weekdays"),
                   selected = "Weekend",
                   inline = T),
      sliderInput("hour",
                  "Hour of day:",
                  min = 0,
                  max = 23,
                  value = 9,
                  step = 1,animate = T)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map")
    )
  )
))
