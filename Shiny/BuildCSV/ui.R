library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Build CSV Data File"),
  
  sidebarPanel(
    radioButtons("evid","Event Type",choices=c("Dose" = "1", "Observation" = "0"),selected="Dose"),
    textInput("date","Date mm/dd/yyyy"),
    textInput("time","Time hh:mm"),
    uiOutput("evidControls")
    
    
  ), #end sidebarPanel
  
  mainPanel(
    
  
    
    
  )
  
) #end pageWithSideBar
        
) #end shinyUI


