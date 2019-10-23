require(shiny)
require(Pmetrics)



shinyServer(function(input,output){
  
  #########  HELPER FUNCTIONS #################
  
  getXlim <- function(){
    if(length(input$xmin)>0 & length(input$xmax)>0) {
      if(input$xmin!="" & input$xmax!="") {xlim <- as.numeric(c(input$xmin,input$xmax))} else {xlim <- NULL}
    } else {xlim <- NULL}
    return(xlim)
  }
  
  getYlim <- function(){
    if(length(input$ymin)>0 & length(input$ymax)>0) {
      if(input$ymin!="" & input$ymax!="") {ylim <- as.numeric(c(input$ymin,input$ymax))} else {ylim <- NULL}
    } else {ylim <- NULL}
    return(ylim)
  }
  
  getXlab <- function(){
    if(length(input$xlab)==0 & inherits(get(input$data),"PMmatrix")) return("Time (h)")
    if(length(input$xlab)==0) return(NULL)
    if(input$xlab=="") return(NULL)
    return(input$xlab)
  }
  
  getYlab <- function(){
    if(length(input$ylab)==0 & inherits(get(input$data),"PMmatrix")) return("Observation")
    if(length(input$ylab)==0) return(NULL)
    if(input$ylab=="") return(NULL)
    return(input$ylab)
  }
  
  getFormula <- function(charac=F,choices){
    if (length(input$x)==0 || length(input$y)==0) return(NULL)
    if (input$x=="Select" || input$y=="Select") return(NULL)
    if (!input$x %in% choices) return(NULL)
    if (!input$y %in% choices) return(NULL)
    if(charac) {return(paste(input$y,input$x,sep="~"))} else {return(as.formula(paste(input$y,input$x,sep="~")))}
  }
  
  getFinalPlotType <- function(){
    if(length(input$ptype)==0) {return(list(ptype="uni",lwd=input$lwd1,col=input$col1))} 
    if(input$ptype=="uni") {return(list(ptype="uni",lwd=input$lwd1,col=input$col1))} 
    if(input$ptype=="bi") {return(list(ptype="bi",lwd=input$lwd2,col=input$col2))} 
  }
  
  getProbs <- function(){
    if(length(input$probs)==0) {return(c(0.05,0.25,0.5,0.75,0.95))} else {return(as.numeric(input$probs))}
  }
  
  getPred <- function(){
    if(length(input$pred)==0) return(NULL)
    if(input$pred=="" | input$pred=="None") return(NULL)
    return(get(input$pred))
  }
  
  getObs <- function(){
    if(length(input$obs)==0) return(NULL)
    if(input$obs=="" | input$obs=="None") return(NULL)
    return(get(input$obs))
  }
  
  getGroup <- function(group){
    x <- get(input$data)
    if(length(group)==0) {return(NULL)}
    colfac <- which(names(x)==group)
    if(length(colfac)>0){return(x[,colfac])}
    return(NULL)
  }
  
  ############### Make Data Controls #####################
  makeDataControls <- function(){
    if(inherits(get(input$data),"PMcov")){
      return(list(
        selectInput("y","Y-axis",choices=c("Select",names(get(input$data))),selected="Select"),
        selectInput("x","X-axis",choices=c("Select",names(get(input$data))),selected="Select"),      
        helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
        radioButtons("include","",c("Include subjects" = "yes", "Exclude subjects" = "no"),selected="yes"),
        selectInput("select","",choices=unique(get(input$data)$id),selected=unique(get(input$data)$id),multiple=T,selectize=F),
        selectInput("icen","Summary method for changing covariates:",choices=c("mean","median","mode","none"),"mean")
      )) #end list
    } #end PMcov
    
    if(inherits(get(input$data),"PMfinal")){
      return(list(
        radioButtons("ptype","Plot type:",c("Univariate" = "uni", "Bivariate" = "bi"),selected="Univariate"),
        conditionalPanel(
          condition="input.ptype=='bi'",
          selectInput("y","Y-axis",choices=c("Select",names(get(input$data)$popMean)),selected="Select"),
          selectInput("x","X-axis",choices=c("Select",names(get(input$data)$popMean)),selected="Select")
        ) #end conditional panel
      )) #end list
    } #end  PMfinal
    
    if(inherits(get(input$data),"PMop")){
      return(list(
        radioButtons("pred.type","",c("Posterior Predictions" = "post","Population Predictions" = "pop"),selected="post"),      
        checkboxInput("resid","Residual Plot",FALSE),
        selectInput("icen","Predictions based on:",choices=c("mean","median"),"median"),
        helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
        radioButtons("include","",c("Include subjects" = "yes", "Exclude subjects" = "no"),selected="yes"),
        selectInput("select","",choices=unique(get(input$data)$id),selected=unique(get(input$data)$id),multiple=T,selectize=F),
        selectInput("outeq","Output equation:",choices=1:max(get(input$data)$outeq),selected=1)
      )) #end list
    } #end PMop
    
    if(inherits(get(input$data),"PMcycle")){
      return(list(
        numericInput("omit","Proportion of burn-in cycles to omit:",0.2,min=0,max=1,step=0.1)
      )) #end list
    } #end PMcycle
    
    if(inherits(get(input$data),"PMmatrix")){
      x <- get(input$data)
      predFilter <- function(x) any(grepl("^PMpop|^PMpost",class(get(x))))
      predchoices <- Filter(predFilter,ls(globalenv()))
      if(length(predchoices)==0){predchoices <- "None"} else {predchoices <- c("None",predchoices)}
      
      ncov <- ncol(x)-which(names(x)=="c3")
      if(ncov>0) {covnames <- c("None",names(x)[(ncol(x)-ncov+1):ncol(x)])} else {covnames <- NULL}
      return(list(
        helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
        radioButtons("include","",c("Include subjects" = "yes", "Exclude subjects" = "no"),selected="yes"),
        selectInput("select","",choices=unique(x$id),selected=unique(x$id),multiple=T),
        selectInput("outeq","Output equation:",choices=1:max(x$outeq,na.rm=T),selected=1),
        numericInput("block","Block number:",1,min=1,step=1),
        selectInput("group","Grouping factor",choices=covnames),
        selectInput("pred","Prediction object:",choices=predchoices)
      )) #end list
    } #end PMmatrix
    
    if(inherits(get(input$data),"PMsim")){
      x <- get(input$data)
      OPFilter <- function(x) any(grepl("^PMop",class(get(x))))
      OPchoices <- Filter(OPFilter,ls(globalenv()))
      if(length(OPchoices)==0){OPchoices <- "None"} else {OPchoices <- c("None",OPchoices)}
      
      return(list(
        selectInput("outeq","Output equation:",choices=1:max(x$obs$outeq,na.rm=T),selected=1),
        selectInput("obs","Observed (for VPC)",choices=OPchoices)
      )) #end list
    } #end PMsim
    
  } #end makeDataControls function
  
  ############### Make Format Controls #####################
  makeFormatControls <- function(){
    if(inherits(get(input$data),"PMcov")){
      return(list(
        h4("Regression Options:"),
        checkboxGroupInput("regression","",
                           choices=c("Linear"="reg",
                                     "Lowess"="lowess",
                                     "Unity line"="ref")),
        numericInput("ci","Confidence Interval",0.95,min=0.1,max=0.99,step=0.05),
        h4("Formatting Options:"),
        checkboxInput("grid","Grid",FALSE),
        checkboxInput("ident","Identify points with subject IDs",FALSE),
        checkboxInput("square","Square",FALSE),
        checkboxInput("log","Log-log plot",FALSE),
        textInput("col","Color:","red"),
        textInput("col.stat","Color of stat text:","black"),
        numericInput("cex","Point size",1,step=0.5),
        numericInput("cex.stat","Stat text size",0.8,step=0.1),
        numericInput("x.stat","Stat horizontal pos",0.6,step=0.1),
        numericInput("y.stat","Stat vertical pos",0.1,step=0.1),
        numericInput("lwd","Line width",2,step=0.5)
      )) #end list
    } #end PMcov
    
    if(identical(inherits(get(input$data),c("PMfinal","NPAG"),which=T),as.integer(1:2))){
      return(list(
        conditionalPanel(
          condition="input.ptype=='uni'",
          checkboxInput("density","Add kernel density",FALSE),
          numericInput("lwd1","Line width:",4),
          textInput("col1","Color:","red")
        ),
        conditionalPanel(
          condition="input.ptype=='bi'",
          checkboxInput("grid","Grid",TRUE),
          numericInput("lwd2","Line width:",1),
          textInput("col2","Color:","white"),
          numericInput("cex","Point size:",1,step=0.1),
          numericInput("pch","Plotting character:",3,min=1,step=1),
          numericInput("scale","Scale factor for points:",100,min=50,step=50),
          textInput("bg","Background fill color for NPAG plot:","gray50")
        ) #end conditional panel
      )) #end list
    } #end PMfinal, NPAG
    
    if(identical(inherits(get(input$data),c("PMfinal","IT2B"),which=T),as.integer(1:2))){
      return(list(
        conditionalPanel(
          condition="input.ptype=='uni'",
          checkboxInput("standard","Standardize IT2B marginals",FALSE),
          numericInput("lwd1","Line width:",4),
          textInput("col1","Color:","red")
        ),
        conditionalPanel(
          condition="input.ptype=='bi'",
          checkboxInput("grid","Grid",TRUE),
          checkboxInput("legend","Legend",TRUE),
          numericInput("lwd2","Line width:",1),
          textInput("col2","Color:","white"),
          numericInput("cex","Point size:",1,step=0.1),
          numericInput("pch","Plotting character:",3,min=1,step=1),
          selectInput("probs","Quantiles",choices=c(0.01,0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975,0.99),selected=c(0.05,0.25,0.5,0.75,0.95),multiple=T)
        ) #end conditional panel
      )) #end list
    } #end PMfinal, IT2B
    
    if(inherits(get(input$data),"PMop")){
      return(list(
        h4("Regression Options:"),
        checkboxGroupInput("regression","",
                           choices=c("Linear"="reg",
                                     "Lowess"="lowess",
                                     "Unity line"="ref"),selected=c("reg","ref")),
        numericInput("ci","Confidence Interval",0.95,min=0.1,max=0.99,step=0.05),
        h4("Formatting Options:"),
        textInput("mult","Multiplication factor for y-axis:","1"),
        checkboxInput("log","Log-log plot",FALSE),
        checkboxInput("grid","Grid",FALSE),
        checkboxInput("ident","Identify points with subject IDs",FALSE),
        checkboxInput("square","Square",TRUE),
        textInput("col","Color:","red"),
        textInput("col.stat","Color of stat text:","black"),
        numericInput("cex","Point size",1,step=0.5),
        numericInput("cex.stat","Stat text size",0.8,step=0.1),
        numericInput("x.stat","Stat horizontal pos",0.6,step=0.1),
        numericInput("y.stat","Stat vertical pos",0.1,step=0.1),
        numericInput("lwd","Line width",2,step=0.5)
      )) #end list
    } #end PMop 
    
    if(inherits(get(input$data),"PMcycle")){
      return(list(
        numericInput("x.leg","Legend horizontal pos:",0,min=0,max=1,step=0.1),
        numericInput("y.leg","Legend vertical pos:",1,min=0,max=1,step=0.1),
        numericInput("cex.leg","Legend text size",1.2,step=0.1)
      )) #end list
    } #end PMcycle
    
    if(inherits(get(input$data),"PMmatrix")){
      return(list(
        helpText("Note that all plots are overlayed in this GUI"),
        textInput("mult","Multiplication factor for y-axis:","1"),
        checkboxInput("log","Log scale Y-axis",FALSE),
        checkboxInput("grid","Grid",FALSE),
        checkboxInput("ident","Identify points with subject IDs",FALSE),
        checkboxInput("legend","Legend for groups",FALSE),
        numericInput("pch","Plotting character",NA,min=0,step=1),
        checkboxInput("errbar","Observation error bars",FALSE),
        checkboxInput("join","Join observations",TRUE),
        numericInput("cex","Point size",1,step=0.5)
      )) #end list
    } #end PMmatrix
    
    if(inherits(get(input$data),"PMsim")){
      return(list(
        textInput("mult","Multiplication factor for y-axis","1"),
        checkboxInput("log","Log scale Y-axis",TRUE),
        selectInput("probs","Quantiles",choices=c(0.01,0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975,0.99),selected=c(0.05,0.25,0.5,0.75,0.95),multiple=T),
        textInput("binSize","Bin Size",0),
        numericInput("pch","Plotting character",NA,min=1,step=1),
        checkboxInput("join","Join observations",TRUE),
        numericInput("cex.qlab","Quantile label size",0.8,step=0.1),
        numericInput("x.qlab","Quantile label horizontal pos",0.4,step=0.1),
        selectInput("pos.qlab","Quantile label position",choices=1:4,selected=1),
        numericInput("ci","Confidence interval",0.95,min=0,max=1,step=0.05),
        checkboxInput("grid","Grid",FALSE),
        textInput("ocol","Observation Color","blue")
      )) #end list
    } #end PMfinal, IT2B
    
  } #end makeFormatControls function
  
  ############### Make Axis Controls #####################
  makeAxisControls <- function(){
    if(inherits(get(input$data),c("PMcov","PMfinal","PMop", "PMsim"))){
      return(list(
        h3("Axes"),
        textInput("xmin","X min"),
        textInput("xmax","X max"),
        textInput("ymin","Y min"),
        textInput("ymax","Y max"),
        textInput("xlab","X label"),
        textInput("ylab","Y label"),
        numericInput("cex.lab","Axis label size",1.2,step=0.1)
      )) #end list
    } #end PMcov, PMfinal, PMop
    
    if(inherits(get(input$data),"PMmatrix")){
      return(list(
        h3("Axes"),
        textInput("xmin","X min"),
        textInput("xmax","X max"),
        textInput("ymin","Y min"),
        textInput("ymax","Y max"),
        textInput("xlab","X label","Time (h)"),
        textInput("ylab","Y label","Observation"),
        numericInput("cex.lab","Axis label size",1.2,step=0.1)
      )) #end list
    } #end PMmatrix
  } #end makeAxisControls
  
  ############### Make PMplot #####################
  makePMplot <- function(){
    if(inherits(get(input$data),"PMcov")){
      if (length(input$x)==0 || length(input$y)==0) return(NULL)
      if (input$x=="Select" || input$y=="Select") return(NULL)
      x <- get(input$data)
      if(input$include=="yes"){
        include <- input$select
        exclude <- NULL
      } else {
        include <- NULL
        exclude <- input$select
      }
      
      if(length(input$regression>0)){
        reg <- "reg" %in% unlist(input$regression)
        lowess <- "lowess" %in% unlist(input$regression)
        ref <- "ref" %in% unlist(input$regression)
      } else {
        reg <- F
        lowess <- F
        ref <- F
      }
      args <- list(x=x,formula=getFormula(choices=names(get(input$data))),include=include,exclude=exclude,icen=input$icen,reg=reg,lowess=lowess,ref=ref,grid=input$grid,cex=input$cex,col=input$col,
                   log=input$log,square=input$square,ident=input$ident,ci=input$ci,x.stat=input$x.stat,y.stat=input$y.stat,
                   col.stat=input$col.stat,cex.stat=input$cex.stat,cex.lab=input$cex.lab,lwd=input$lwd,xlim=getXlim(),ylim=getYlim(),xlab=getXlab(),ylab=getYlab()) 
      args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
      do.call(plot.PMcov,args)  
    } #end PMcov
    
    if(identical(inherits(get(input$data),c("PMfinal","NPAG"),which=T),as.integer(1:2))){
      x <- get(input$data)
      args <- list(x=x,formula=getFormula(choices=names(get(input$data)$popMean)),cex.lab=input$cex.lab,col=getFinalPlotType()$col,
                   pch=input$pch,cex=input$cex,lwd=getFinalPlotType()$lwd,density=input$density,scale=input$scale,bg=input$bg,
                   grid=input$grid,xlim=getXlim(),ylim=getYlim(),xlab=getXlab(),ylab=getYlab())
      if(getFinalPlotType()$ptype=="uni"){args$formula <- NULL} #reset formula
      args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
      do.call(plot.PMfinal,args)
    } #end PMfinal, NPAG
    
    if(identical(inherits(get(input$data),c("PMfinal","IT2B"),which=T),as.integer(1:2))){
      x <- get(input$data)
      args <- list(x=x,formula=getFormula(choices=names(get(input$data)$popMean)),cex.lab=input$cex.lab,col=getFinalPlotType()$col,
                   pch=input$pch,cex=input$cex,lwd=getFinalPlotType()$lwd,probs=getProbs(),standard=input$standard,legend=input$legend,
                   grid=input$grid,xlim=getXlim(),ylim=getYlim(),xlab=getXlab(),ylab=getYlab())
      if(getFinalPlotType()$ptype=="uni"){args$formula <- NULL} #reset formula
      args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
      do.call(plot.PMfinal,args)
    } #end PMfinal, IT2B
    
    if(inherits(get(input$data),"PMop")){
      x <- get(input$data)
      if(input$include=="yes"){
        include <- input$select
        exclude <- NULL
      } else {
        include <- NULL
        exclude <- input$select
      }
      if(length(input$regression)>0){
        reg <- "reg" %in% unlist(input$regression)
        lowess <- "lowess" %in% unlist(input$regression)
        ref <- "ref" %in% unlist(input$regression)
      } else {
        reg <- T
        lowess <- F
        ref <- T
      }
      if(length(input$square)>0){
        square <- input$square
      } else {square <- T}
      args <- list(x=x,include=include,exclude=exclude,pred.type=input$pred.type,icen=input$icen,outeq=as.numeric(input$outeq),mult=as.numeric(input$mult),resid=input$resid,reg=reg,lowess=lowess,ref=ref,grid=input$grid,cex=input$cex,col=input$col,
                   log=input$log,square=square,ident=input$ident,ci=input$ci,x.stat=input$x.stat,y.stat=input$y.stat,
                   col.stat=input$col.stat,cex.stat=input$cex.stat,cex.lab=input$cex.lab,lwd=input$lwd,xlim=getXlim(),ylim=getYlim(),xlab=getXlab(),ylab=getYlab()) 
      args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
      do.call(plot.PMop,args)  
    } #end PMop
    
    if(inherits(get(input$data),"PMcycle")){
      x <- get(input$data)
      if(length(input$omit)==0) {omit <- 0.2} else {omit <- input$omit}
      args <- list(x=x,x.leg=input$x.leg,y.leg=input$y.leg,cex.leg=input$cex.leg,omit=omit)
      args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
      do.call(plot.PMcycle,args)
    } #end PMcycle
    
    if(inherits(get(input$data),"PMmatrix")){
      x <- get(input$data)
      if(input$include=="yes"){
        include <- input$select
        exclude <- NULL
      } else {
        include <- NULL
        exclude <- input$select
      }   
      if(length(input$group)==0 || input$group=="None"){group <- NULL} else {group <- input$group}
      
      args <- list(x=x,include=include,exclude=exclude,pred=getPred(),outeq=as.numeric(input$outeq),group=group,
                   block=as.numeric(input$block),mult=as.numeric(input$mult),grid=input$grid,cex=input$cex,
                   log=input$log,ident=input$ident,join=input$join,errbar=input$errbar,pch=input$pch,legend=input$legend,
                   cex.lab=input$cex.lab,xlim=getXlim(),ylim=getYlim(),
                   xlab=getXlab(),ylab=getYlab()) 
      args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
      do.call(plot.PMmatrix,args)  
    } #end PMmatrix
    
    if(inherits(get(input$data),"PMsim")){
      x <- get(input$data)
      args <- list(x=x,mult=as.numeric(input$mult),log=input$log,
                   probs=getProbs(),
                   binSize=as.numeric(input$binSize),
                   outeq=as.numeric(input$outeq),pch=input$pch,join=input$join,
                   x.qlab=input$x.qlab,cex.qlab=input$cex.qlab,
                   pos.qlab=as.numeric(input$pos.qlab),ci=input$ci,cex.lab=input$cex.lab,
                   xlab=getXlab(),ylab=getYlab(),xlim=getXlim(),ylim=getYlim(),
                   obs=getObs(),grid=input$grid,ocol=input$ocol) 
      args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
      do.call(plot.PMsim,args)  
    } #end PMop
    
    return(NULL) #plot method not defined
    
  } #end makePMplot
  
  
  ############### Make PMstatement #####################
  makePMstatement <- function(){
    FormatArg <- function(argname,argval,argdef,quote=F,showargname=T){
      if(length(argval)>0 & !identical(as.character(argval),as.character(argdef))){
        if(length(argval)>1) argval=paste("c(",paste(argval,collapse=","),")",sep="")
        if(showargname){
          argstr <- paste(argname,"=",ifelse(quote,"\"",""),argval,ifelse(quote,"\"",""),sep="")
        } else {argstr <- paste(ifelse(quote,"\"",""),argval,ifelse(quote,"\"",""),sep="")}
      } else {argstr <- ""}
      return(argstr)
    } #end FormatArg
    
    if(inherits(get(input$data),"PMcov")){
      if (length(input$x)==0 || length(input$y)==0) return("plot(...)")
      if (input$x=="Select" || input$y=="Select") return("plot(...)")
      if(length(input$include)!=0){
        if(input$include=="yes"){
          incdef <- unique(get(input$data)$id)
          excdef <- input$select
        } else {
          incdef <- input$select
          excdef <- unique(get(input$data)$id)
        }
      } else {
        incdef <- unique(get(input$data)$id)
        excdef <- input$select
      }
      arglist <- c(
        FormatArg("x",input$data,"",showargname=F),
        FormatArg("formula",getFormula(charac=T,choices=names(get(input$data))),"",showargname=F),
        FormatArg("icen",input$icen,"median",T),
        FormatArg("include",input$select,incdef),      
        FormatArg("exclude",input$select,excdef),
        FormatArg("log",input$log,F),
        FormatArg("square",input$square,F),
        FormatArg("ref","ref" %in% unlist(input$regression),F),
        FormatArg("lowess","lowess" %in% unlist(input$regression),F),
        FormatArg("grid",input$grid,F),
        FormatArg("ident",input$ident,F),
        FormatArg("reg","reg" %in% unlist(input$regression),F),
        FormatArg("ci",input$ci,0.95),
        FormatArg("cex",input$cex,1),
        FormatArg("cex.lab",input$cex.lab,1.2),
        FormatArg("x.stat",input$x.stat,0.6),
        FormatArg("y.stat",input$y.stat,0.1),
        FormatArg("col.stat",input$col.stat,"black",T),
        FormatArg("cex.stat",input$cex.stat,0.8),
        FormatArg("lwd",input$lwd,2),
        FormatArg("col",input$col,"red",T),
        FormatArg("xlim",getXlim(),NULL),
        FormatArg("ylim",getYlim(),NULL),
        FormatArg("xlab",getXlab(),NULL,T),
        FormatArg("ylab",getYlab(),NULL,T)
      ) #end arglist for PMcov
      if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
      arglist <- paste(arglist,collapse=", ")
      return(paste("plot(",arglist,")",sep=""))
    } #end PMcov
    
    if(identical(inherits(get(input$data),c("PMfinal","NPAG"),which=T),as.integer(1:2))){
      arglist <- c(
        FormatArg("x",input$data,"",showargname=F),
        ifelse(getFinalPlotType()$ptype=="uni","",FormatArg("formula",getFormula(charac=T,choices=names(get(input$data)$popMean)),"",showargname=F)),
        FormatArg("cex.lab",input$cex.lab,1.2),
        FormatArg("col",getFinalPlotType()$col,ifelse(getFinalPlotType()$ptype=="uni","red","white"),T),
        FormatArg("pch",input$pch,3),
        FormatArg("cex",input$cex,1),
        FormatArg("lwd",getFinalPlotType()$lwd,ifelse(getFinalPlotType()$ptype=="uni",4,1)),
        FormatArg("density",input$density,FALSE),
        FormatArg("scale",input$scale,100),
        FormatArg("bg",input$bg,"gray50",TRUE),
        FormatArg("grid",input$grid,TRUE),
        FormatArg("xlim",getXlim(),NULL),
        FormatArg("ylim",getYlim(),NULL),
        FormatArg("xlab",getXlab(),NULL,T),
        FormatArg("ylab",getYlab(),NULL,T)
      ) #end arglist for PMfinal
      if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
      arglist <- paste(arglist,collapse=", ")
      return(paste("plot(",arglist,")",sep=""))
    } #end PMfinal, NPAG
    
    if(identical(inherits(get(input$data),c("PMfinal","IT2B"),which=T),as.integer(1:2))){
      arglist <- c(
        FormatArg("x",input$data,"",showargname=F),
        ifelse(getFinalPlotType()$ptype=="uni","",FormatArg("formula",getFormula(charac=T,choices=names(get(input$data)$popMean)),"",showargname=F)),
        FormatArg("cex.lab",input$cex.lab,1.2),
        FormatArg("col",getFinalPlotType()$col,ifelse(getFinalPlotType()$ptype=="uni","red","white"),T),
        FormatArg("pch",input$pch,3),
        FormatArg("cex",input$cex,1),
        FormatArg("lwd",getFinalPlotType()$lwd,ifelse(getFinalPlotType()$ptype=="uni",4,1)),
        FormatArg("legend",input$legend,TRUE),
        FormatArg("probs",getProbs(),c(0.05,0.25,0.5,0.75,0.95)),
        FormatArg("standard",input$standard,FALSE),
        FormatArg("grid",input$grid,TRUE),
        FormatArg("xlim",getXlim(),NULL),
        FormatArg("ylim",getYlim(),NULL),
        FormatArg("xlab",getXlab(),NULL,T),
        FormatArg("ylab",getYlab(),NULL,T)
      ) #end arglist for PMfinal
      if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
      arglist <- paste(arglist,collapse=", ")
      return(paste("plot(",arglist,")",sep=""))
    } #end PMfinal, IT2B
    
    if(inherits(get(input$data),"PMop")){
      if(length(input$include)!=0){
        if(input$include=="yes"){
          incdef <- unique(get(input$data)$id)
          excdef <- input$select
        } else {
          incdef <- input$select
          excdef <- unique(get(input$data)$id)
        }
      } else {
        incdef <- unique(get(input$data)$id)
        excdef <- input$select
      }
      if(length(input$regression)>0){
        reg <- "reg" %in% unlist(input$regression)
        lowess <- "lowess" %in% unlist(input$regression)
        ref <- "ref" %in% unlist(input$regression)
      } else {
        reg <- T
        lowess <- F
        ref <- T
      }
      if(length(input$square)>0){
        square <- input$square
      } else {square <- T}
      arglist <- c(
        FormatArg("x",input$data,"",showargname=F),
        FormatArg("include",input$select,incdef),      
        FormatArg("exclude",input$select,excdef),
        FormatArg("pred.type",input$pred.type,"post",T),
        FormatArg("icen",input$icen,"median",T),
        FormatArg("outeq",input$outeq,1),
        FormatArg("mult",as.numeric(input$mult),1),
        FormatArg("resid",input$resid,F),
        FormatArg("log",input$log,F),
        FormatArg("square",square,T),
        FormatArg("ref",ref,T),
        FormatArg("lowess",lowess,F),
        FormatArg("grid",input$grid,F),
        FormatArg("ident",input$ident,F),
        FormatArg("reg",reg,T),
        FormatArg("ci",input$ci,0.95),
        FormatArg("cex",input$cex,1),
        FormatArg("cex.lab",input$cex.lab,1.2),
        FormatArg("x.stat",input$x.stat,0.6),
        FormatArg("y.stat",input$y.stat,0.1),
        FormatArg("col.stat",input$col.stat,"black",T),
        FormatArg("cex.stat",input$cex.stat,0.8),
        FormatArg("lwd",input$lwd,2),
        FormatArg("col",input$col,"red",T),
        FormatArg("xlim",getXlim(),NULL),
        FormatArg("ylim",getYlim(),NULL),
        FormatArg("xlab",getXlab(),NULL,T),
        FormatArg("ylab",getYlab(),NULL,T)
      ) #end arglist for PMop
      if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
      arglist <- paste(arglist,collapse=", ")
      return(paste("plot(",arglist,")",sep=""))
    } #end PMop
    
    if(inherits(get(input$data),"PMcycle")){
      arglist <- c(
        FormatArg("x",input$data,"",showargname=F),
        FormatArg("x.leg",input$x.leg,0),
        FormatArg("y.leg",input$y.leg,1),
        FormatArg("cex.leg",input$cex.leg,1.2),
        FormatArg("omit",input$omit,0.2)
      ) #end arglist for PMcycle
      if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
      arglist <- paste(arglist,collapse=", ")
      return(paste("plot(",arglist,")",sep=""))
    } #end PMcycle
    
    if(inherits(get(input$data),"PMmatrix")){
      if(length(input$include)!=0){
        if(input$include=="yes"){
          incdef <- unique(get(input$data)$id)
          excdef <- input$select
        } else {
          incdef <- input$select
          excdef <- unique(get(input$data)$id)
        }
      } else {
        incdef <- unique(get(input$data)$id)
        excdef <- input$select
      }
      if(length(input$group)==0 || input$group=="None"){group <- NULL} else {group <- input$group}
      
      arglist <- c(
        FormatArg("x",input$data,"",showargname=F),
        FormatArg("include",input$select,incdef),      
        FormatArg("exclude",input$select,excdef),
        FormatArg("pred",input$pred,"None"),
        FormatArg("outeq",as.numeric(input$outeq),1),
        FormatArg("group",group,NULL,T),
        FormatArg("block",as.numeric(input$block),1),
        FormatArg("mult",as.numeric(input$mult),1),
        FormatArg("log",input$log,F),
        FormatArg("grid",input$grid,F),
        FormatArg("join",input$join,T),
        FormatArg("pch",input$pch,NA),
        FormatArg("ident",input$ident,F),
        FormatArg("cex",input$cex,1),
        FormatArg("cex.lab",input$cex.lab,1.2),        
        FormatArg("legend",input$legend,F),
        FormatArg("xlim",getXlim(),NULL),
        FormatArg("ylim",getYlim(),NULL),
        FormatArg("xlab",getXlab(),"Time (h)",T),
        FormatArg("ylab",getYlab(),"Observation",T)
      ) #end arglist for PMmatrix
      if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
      arglist <- paste(arglist,collapse=", ")
      return(paste("plot(",arglist,")",sep=""))
    } #end PMmatrix
    
    if(inherits(get(input$data),"PMsim")){
      arglist <- c(
        FormatArg("x",input$data,"",showargname=F),
        FormatArg("mult",as.numeric(input$mult),1),
        FormatArg("log",input$log,T),
        FormatArg("probs",getProbs(),c(0.05,0.25,0.5,0.75,0.95)),
        FormatArg("binSize",as.numeric(input$binSize),0),
        FormatArg("outeq",as.numeric(input$outeq),1),
        FormatArg("pch",input$pch,NA),
        FormatArg("join",input$join,T),
        FormatArg("x.qlab",input$x.qlab,0.4),
        FormatArg("cex.qlab",input$cex.qlab,0.8),
        FormatArg("pos.qlab",as.numeric(input$pos.qlab),1),
        FormatArg("ci",input$ci,0.95),
        FormatArg("cex.lab",input$cex.lab,1.2),
        FormatArg("xlab",getXlab(),NULL,T),
        FormatArg("ylab",getYlab(),NULL,T),

        FormatArg("xlim",getXlim(),NULL),
        FormatArg("ylim",getYlim(),NULL),
        FormatArg("obs",input$obs,"None"),
        FormatArg("grid",input$grid,FALSE),        
        FormatArg("ocol",input$ocol,"blue")
      ) #end arglist for PMsim
      if(any(sapply(arglist,function(x) x==""))){arglist <- arglist[-which(arglist=="")]}
      arglist <- paste(arglist,collapse=", ")
      return(paste("plot(",arglist,")",sep=""))
    } #end PMsim
    
  } #end makePMstatement
  
  
  #################################################### 
  ############### Build the Page #####################
  #################################################### 
  
  output$help <- renderText({
    if(inherits(get(input$data),"PMcov")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMcov</span>.")}
    if(inherits(get(input$data),"PMfinal")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMfinal</span>.")}
    if(inherits(get(input$data),"PMop")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMop</span>.")}
    if(inherits(get(input$data),"PMcycle")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMcycle</span>.")}
    if(inherits(get(input$data),"PMmatrix")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMmatrix</span>.")}
    if(inherits(get(input$data),"PMsim")){return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PMsim</span>.")}
    
  })
  
  #Set up the inputs
  output$DataControls <- renderUI({makeDataControls()}) 
  output$FormatControls <- renderUI({makeFormatControls()})
  output$AxesControls <- renderUI({makeAxisControls()})
  
  #Make the plot call
  output$PMplot <- renderPlot({makePMplot()})
  
  #Build the Pmetrics plot statements
  output$plotCode <- renderText({makePMstatement()})
  
  
}) #end shinyServer