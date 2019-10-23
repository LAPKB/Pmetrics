library(shiny)
require(Pmetrics)



shinyServer(function(input,output){
  
  
  output$evidControls <- renderUI({
    if(input$evid=="0"){
      list(
        textInput("out","Output/Observation"),
        textInput("outeq","Output Number"),
        textInput("asserr","C0,C1,C2,C3",value=NA)
      )
    } else {
      list(
        textInput("dur","Input Duration","0"),
        textInput("dose","Dose Amount"),
        textInput("addl","Additional Doses",value=NA),
        textInput("ii","Interdose Interval",value=NA),
        textInput("input","Input Number",value="1")
      )
    }
  })
  
 
  
})
