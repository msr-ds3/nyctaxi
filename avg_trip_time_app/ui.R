library(shiny)


# Define UI for application that draws a plot
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Average Travel Times"),
  
  # Sidebar with a inputs for choice
  sidebarLayout(
    sidebarPanel(
      uiOutput("sourceSelector"),
      uiOutput("destinationSelector"),
      selectInput("weekday",
                  "Choose an option",
                  as.list(c("weekday",
                            "weekend",
                            "sunday",
                            "monday",
                            "tuesday", 
                            "wednesday",
                            "thursday",
                            "friday",
                            "saturday")))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
