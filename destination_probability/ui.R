library(shiny)
library(leaflet)
#source("../flow/distribution_of_destinations.R")

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
  titlePanel("Where are people going?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood",
                  "Choose a source neighborhood:",
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
                   step = 1),
       sliderInput("n",
                   "Choose top n:",
                   min = 1,
                   max = 25,
                   value = 5,
                   step = 1),
      radioButtons("type",
                   "What data do you want to see?",
                   c("Popular destinations",
                     "Unusual destinations (method a)",
                     "Unusual destinations (method b)",
                     "Unusual destinations (method c)"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       leafletOutput("map")
    )
  )
))
