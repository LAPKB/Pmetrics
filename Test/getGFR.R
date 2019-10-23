library(shiny)
library(miniUI)


getGFR <- function(){
  allthere <- function(...){
    result <- all(!is.na(c(...)))
    return(result)
  }
  
  ui <- miniPage(
    gadgetTitleBar("Estimate GFR"),
    fillRow(
      miniContentPanel(
        selectInput("method","Choose estimation formula",
                    choices=c("Cockroft-Gault", "MDRD","MDRD-BUN","CKD-EPI","Schwartz","Mayo", "Jelliffe73"),
                    selected="Jelliffe73"),
        uiOutput("DataFields")
      ), #end miniContent Panel
      
      miniContentPanel(
        h3(textOutput("GFR"))
      ) #end miniContentPanel #2
    ) #end fillRow
  ) #end miniPage
  
  server <- function(input, output, session){
    
    
    makeDataFields <- function(){
      if(input$method == "Cockroft-Gault"){
        return(list(
          textInput("scr","Serum creatinine"),
          textInput("age","Age (years)"),
          textInput("wt","Weight (kg)"),
          radioButtons("male","Sex",choices=c("Male" = 1, "Female" = 0),inline=T)
        ))
      } 
      if(input$method == "CKD-EPI"){
        return(list(
          textInput("scr","Serum creatinine"),
          textInput("age","Age (years)"),
          radioButtons("male","Sex",choices=c("Male" = 1, "Female" = 0),inline=Te),
          radioButtons("black","Black Race",choices=c("No" = 0, "Yes" = 1),inline=Tk)
          
        ))
      } 
    }
    
    calcGFR <- function(scr=NULL,age=NULL,wt=NULL,male=NULL,black=NULL){
      if(input$method == "Cockroft-Gault" & allthere(scr,age,wt,male)){
        coef <- ifelse(male==0,0.85,1)
        GFR <- ((140-age)*wt*coef)/(72*scr)
        return(list(gfr=round(GFR,2),units="ml/min"))
      }
      
      if(input$method == "CKD-EPI" & allthere(scr,age,male,black)){
        alpha <- ifelse(male==0,-0.329,-0.411)
        kappa <- ifelse(male==0,0.7,0.9)
        coefF <- ifelse(male==0,1.018,1)
        coefB <- ifelse(black==0,1,1.159)
        maxi <-  max(1,scr/kappa)
        mini <-  min(1,scr/kappa)

        GFR <-  141 * mini**alpha * maxi**(-1.209) * 0.993**age * coefF * coefB
        return(list(gfr=round(GFR,2),units="ml/min"))
        
      }
      return(NULL)
    } #end calcGFR
    
    
    
    output$DataFields <- renderUI({makeDataFields()}) 
    output$GFR <- renderText({paste(calcGFR(scr=as.numeric(input$scr),age=as.numeric(input$age),wt=as.numeric(input$wt),
                                            male=as.numeric(input$male),black=as.numeric(input$black)),collapse=" ")})

  } #end server function
  
  runGadget(ui, server)
}

getGFR()

