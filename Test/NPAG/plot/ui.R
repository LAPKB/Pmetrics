library(shiny)

ClassFilter <- function(x) any(grepl("^PMcov|^PMfinal|^PMop|^PMcycle|^PMmatrix|^PMsim|^PMpta",class(get(x))))
choices <- Filter(ClassFilter,ls(globalenv()))
if(length(choices)==0) stop("No Pmetrics objects loaded\n")

shinyUI(fluidPage(
                  
                  titlePanel("Pmetrics Plot"),
                  
                  sidebarLayout(
                    sidebarPanel(tabsetPanel(
                      tabPanel("Data",
                               selectInput("data","Choose a Pmetrics object to plot:",choices=choices),
                               uiOutput("DataControls")
                      ),
                      tabPanel("Formatting",
                               uiOutput("FormatControls")
                      ),
                      tabPanel("Axes",
                               uiOutput("AxesControls")
                      )
                    ) #end tabsetPanel
                    
                    ), #end sidebarPanel
                    
                    mainPanel(
                      h3("Copy and paste the code below into your R script to reproduce the plot:"),
                      helpText("Note: If you accepted the default value for an argument,",
                               "it is not necessary to include that argument in the call to plot",
                               "and it has been omitted here, following standard R practice."),
                      helpText(htmlOutput("help")),
                      wellPanel(h4(textOutput("plotCode"))),
                      plotOutput("PMplot")
                      
                    ) #end mainPanel
                    
                  ) #end sidebarLayout
                  
) #end fluidpage

) #end shinyUI

