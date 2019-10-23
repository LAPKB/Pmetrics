library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Simulation"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("K12",
                  "K12:",
                  min = 0,
                  max = 2,
                  value = 0.13,
                  step=0.01)
      ,
      
      sliderInput("K21",
                  "K21:",
                  min = 0,
                  max = 0.5,
                  value = 0.02,
                  step=0.01)
      ,
      
      sliderInput("K20",
                  "K20:",
                  min = 0,
                  max = 1,
                  value = 0.33,
                  step=0.01)
      ,
      
      sliderInput("fm",
                  "fm:",
                  min = 0,
                  max = 1,
                  value = 0.32,
                  step=0.01)
      ,
      
      sliderInput("K34",
                  "K34:",
                  min = 0,
                  max = 0.5,
                  value = 0.05,
                  step=0.01)
      ,
      
      sliderInput("K43",
                  "K43:",
                  min = 0,
                  max = 1.5,
                  value = 1.04,
                  step=0.01)
      ,
      
      sliderInput("K30",
                  "K30:",
                  min = 0,
                  max = 1,
                  value = 0.13,
                  step=0.01)
    ),
    
    # Show plots
    mainPanel(
      uiOutput("tcPlots")
    )
  )
))
