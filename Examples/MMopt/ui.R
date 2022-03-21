library(shiny)

shinyUI(fluidPage(theme="bootstrap.css",
                  
                  titlePanel(
                    title=selectInput("runtype","",choices=c("NPAG","IT2B")),
                    windowTitle="Pmetrics Run"    
                  ),
                  
                  sidebarLayout(
                    sidebarPanel(
                      tabsetPanel(
                        tabPanel("Data",
                                 #fileInput("data","Data File",accept="csv"),
                                 selectInput("data","",list.files(getwd()),selected=list.files(getwd(),pattern="\\..sv")[1]),
                                 uiOutput("DataControls")
                        ),
                        tabPanel("Model",
                                 #fileInput("model","Model File",accept="txt"),
                                 selectInput("model","",list.files(getwd()),selected=list.files(getwd(),pattern="\\.txt")[1])
                        ),
                        tabPanel("Run",
                                 uiOutput("RunControls")
                        )
                      ) #end tabsetPanel
                    ), #end sidebarPanel
                    
                    mainPanel(
                      
                      tags$style("#model",
                                 "{",
                                 "font-family:'Trebuchet MS', Arial, Helvetica, sans-serif;",
                                 "border-collapse:collapse;",
                                 "}",
                                 "#model td, #model th ",
                                 "{",
                                 "font-size:1em;",
                                 "text-align:center;",
                                 "padding-right:15px;",
                                 "padding-left:15px;",               
                                 "}",
                                 "#model tr ",
                                 "{",
                                 "vertical-align:top;",
                                 
                                 "}",
                                 "#model th.alt ",
                                 "{",
                                 "font-size:1.1em;",
                                 "text-align:center;",
                                 "background-color:#0099FF;",
                                 "color:#ffffff;",
                                 "}"),
                      h3("Copy and paste the code below into your R script to execute the run and get results back into R:"),
                      helpText("Note: If you accepted the default value for an argument,",
                               "it is not necessary to include that argument in the call to NPrun",
                               "and it has been omitted here, following standard R practice."),
                      wellPanel(h4(textOutput("RunCode")),h4(textOutput("LoadCode"))),
                      h3("Current Model"),
                      HTML("<table id='model'>"),
                      HTML("<tr>"),
                      HTML("<th class='alt'>Primary Variables</th><th class='alt'>Covariates</th><th class='alt'>Secondary Variables</th>"),
                      HTML("</tr>"),
                      HTML("<tr>"),
                      HTML("<td>"),
                      tableOutput(outputId = "primVar"),
                      HTML("</td>"),
                      HTML("<td>"),
                      tableOutput(outputId = "covar"),
                      HTML("</td>"),
                      HTML("<td>"),
                      tableOutput(outputId = "secVar"),
                      HTML("</td>"),
                      HTML("</tr>"),
                      HTML("<tr>"),
                      HTML("<th class='alt'>Bolus Inputs</th><th class='alt'>Initial Conditions</th><th class='alt'>Lag Time</th>"),
                      HTML("</tr>"),
                      HTML("<tr>"),
                      HTML("<td>"),
                      tableOutput(outputId = "bolus"),
                      HTML("</td>"),
                      HTML("<td>"),
                      tableOutput(outputId = "IC"),
                      HTML("</td>"),
                      HTML("<td>"),
                      tableOutput(outputId = "tlag"),
                      HTML("</td>"),
                      HTML("</tr>"),
                      HTML("<th class='alt'>Differential Equations</th><th class='alt'>Outputs</th><th class='alt'>Error Model</th>"),
                      HTML("</tr>"),
                      HTML("<tr>"),
                      HTML("<td>"),
                      tableOutput(outputId = "diffeq"),
                      HTML("</td>"),
                      HTML("<td>"),
                      tableOutput(outputId = "output"),
                      HTML("</td>"),
                      HTML("<td>"),
                      tableOutput(outputId = "error"),
                      HTML("</td>"),
                      HTML("</tr>")
                      
                    ) #end main page
                    
                  ) #end sidebarLayout
                  
) #end fluidPage

) #end shinyUI

