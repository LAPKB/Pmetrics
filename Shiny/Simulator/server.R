library(shiny)
library(deSolve)
library(Pmetrics)

# Define server logic required to draw a histogram
mdata.8 <- PMreadMatrix("test.csv")
shinyServer(function(input, output){
  
  state <- c(
    X1 = 0.056,
    X2 = 0,
    X3 = 0,
    X4 = 0
  )
  
  conc <- function(t,state,param){
    A = ifelse(t<4,0.056,0)
    with(as.list(c(state,param)),{
      dX1 = A - K12*X1 + K21*X2
      dX2 = K12*X1 - (K20 + K21 - fm)*X2 
      dX3 = fm*X2 - (K34 + K30)*X3 + K43*X4
      dX4 = K34*X3 - K43*X4
      
      list(c(dX1,dX2,dX3,dX4))
    })
  }
  
  eventdat <- data.frame(var=c("X1","X4"),
                         time = 4,
                         value = c(0.000721,0.000025),
                         method="replace")
  
  times <- seq(0,72,0.5)
  
  output$tcPlots <- renderUI({
    plot_output_list <- lapply(1:4, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 210, width = 400)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }) #end renderUI
  
    for (i in 1:4) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          param <- c(
            K12 = input$K12,
            K21 = input$K21,
            K20 = input$K20,
            fm = input$fm,
            K34 = input$K34,
            K43 = input$K43,
            K30 = input$K30
          )
          ymax <- c(0.1,0.1,0.2,0.01)
          titles <- c("DNR Medium","DNR Cell","DNR-ol Cell","DNR-ol Medium")
          out <- ode(y=state, times=times, func=conc,parms=param, events=list(data=eventdat))
          plot(out~time,mdata.8[mdata.8$evid==0 & mdata.8$outeq==my_i,],ylim=c(0,ymax[my_i]),xlim=c(0,72),
               xlab="Time (h)",ylab="Conc mcg/mL",main=titles[my_i])
          lines(y=out[,my_i+1],x=out[,1])
        }) #end renderPlot
      }) #end local
    } #end for loop

}) #end shinyServer
  
  
  
  
  