require(shiny)
require(Pmetrics)



shinyServer(function(input,output){
  
  #########  HELPER FUNCTIONS #################
  
  getDefaults <- reactive({
    if(length(input$data)==0) return(NULL)
    mdata <- PMreadMatrix(file=input$data,quiet=T)
    if("id" %in% names(mdata) & "time" %in% names(mdata) & "input" %in% names(mdata)){
      ndrug <- max(mdata$input,na.rm=T)
      numeqt <- max(mdata$outeq,na.rm=T)
      maxTime <- max(mdata$time,na.rm=T)
      if(ncol(mdata)>14) {covnames <- names(mdata)[15:ncol(mdata)]} else {covnames <- "None"}
      if(maxTime>24*48) {aucint <- ceiling(maxTime/48)} else {aucint <- 24}
      id <- unique(mdata$id)
      nsub <- length(id)
      if(input$runtype=="NPAG") {engine <- list(alg="NP",nsubtot=1,nsub=1,activesub=1,ncov=1,covnames=covnames,ndrug=ndrug,tol=0.01,
                                                salt=1,numeqt=numeqt,cycles=1,icen="median",indpts=-99,aucint=24,idelta=12,xmic=1,ode=-4,
                                                limits=NA,priorString=1,wrkFlag=F)}
      
      if(input$runtype=="IT2B"){engine <- list(alg="IT",nsubtot=1,nsub=1,activesub=1,ncov=1,covnames=covnames,ndrug=ndrug,
                                               salt=1,numeqt=numeqt,cycles=1,xsig=0.5,xtol=0.001,xdev=5,indpts=-99,
                                               ode=-4,limits=NA,wrkFlag=F)}
      
      
      trans <- makeModel(model=input$model,data=input$data,engine=engine,write=F,silent=T)
      
      olddir <- list.dirs(recursive=F)
      olddir <- olddir[grep("^\\./[[:digit:]]+",olddir)]
      olddir <- sub("^\\./","",olddir)
      if(length(olddir)>0){
        newdir <- as.character(max(as.numeric(olddir))+1)
      } else {newdir <- "1"}
      
      if(input$runtype=="NPAG"){return(list(ndrug=ndrug,maxTime=maxTime,covnames=covnames,aucint=aucint,id=id,trans=trans,newdir=newdir,error=F))}
      if(input$runtype=="IT2B"){return(list(ndrug=ndrug,covnames=covnames,id=id,trans=trans,newdir=newdir,error=F))}
      
    } else {
      if(input$runtype=="NPAG"){return(list(ndrug=1,maxTime=0,covnames="",aucint=24,id=1,trans=list(blocks=NULL),newdir=1,error=T))}
      if(input$runtype=="IT2B"){return(list(ndrug=1,covnames="",id=1,trans=list(blocks=NULL),newdir=1,error=T))}
    }
  })
  
  ############### Make Data Controls #####################
  makeDataControls <- function(){
    if(input$runtype=="NPAG"){
      return(list(
        helpText(paste("Covariate names:",paste(getDefaults()$covnames,collapse=", "))),
        helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
        radioButtons("include","",c("Include subjects" = "yes", "Exclude subjects" = "no"),selected="Include subjects"),
        selectInput("select","",choices=getDefaults()$id,selected=getDefaults()$id,multiple=T,selectize=T),
        helpText(paste("Number of drug inputs:",getDefaults()$ndrug)),
        textInput("salt","Salt fraction for each input, separated by commas:",paste(rep(1,getDefaults()$ndrug),collapse=",")),
        numericInput("aucint","AUC interval in hours:",getDefaults()$aucint)
      ))
    } #end NPAG data controls
    
    if(input$runtype=="IT2B"){
      return(list(
        helpText(paste("Covariate names:",paste(getDefaults()$covnames,collapse=", "))),
        helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
        radioButtons("include","",c("Include subjects" = "yes", "Exclude subjects" = "no"),selected="Include subjects"),
        selectInput("select","",choices=getDefaults()$id,selected=getDefaults()$id,multiple=T),
        helpText(paste("Number of drug inputs:",getDefaults()$ndrug)),
        textInput("salt","Salt fraction for each input, separated by commas:",paste(rep(1,getDefaults()$ndrug),collapse=","))
      ))
    } #end IT2B data controls
    
  } #end makeDataControls
  
  ############### Make Run Controls #####################
  makeRunControls <- function(){
    if(input$runtype=="NPAG"){
      return(list(
        numericInput("cycles","Number of Cycles to run:",100),
        selectInput("indpts","Index of starting grid point number:",choices=c(1,3,4,6,101:114),getDefaults()$trans$indpts),
        sliderInput("ode","Log Ordinary Differential Equation Solver Tolerance:",min=-8,max=0,value=-4,step=1),
        selectInput("icen","Parameter distribution summary for reporting predictions on HTML summary:",c("median","mean")),
        numericInput("idelta","Interval in minutes for predicted concentrations",12),
        textInput("prior","Prior density (Run number, NPdata.x or DENxxxx)")
      ))
    } #end NPAG run controls
    
    if(input$runtype=="IT2B"){
      return(list(
        numericInput("cycles","Number of Cycles to run:",100),
        sliderInput("ode","Log Ordinary Differential Equation Solver Tolerance:",min=-8,max=0,value=-4,step=1),
        textInput("xsig","Multiple of initial parameter range for initial parameter SDs:","0.5"),
        textInput("xtol","Convergence tolerance:","0.001"),
        textInput("xdev","Multiples of parameter SDs to send to NPAG:","5")
      ))
    } #end IT2B run controls
    
  } #end makeRunControls
  
  ############### Make PMstatement #####################
  makeRunStatement <- function(){  
    FormatArg <- function(argname,argval,argdef,quote=F){
      if(length(argval)>0 & !identical(as.character(argval),as.character(argdef))){
        if(length(argval)>1) argval=paste("c(",paste(argval,collapse=","),")",sep="")
        argstr <- paste(argname,"=",ifelse(quote,"\"",""),argval,ifelse(quote,"\"",""),sep="")
      } else {argstr <- ""}
      return(argstr)
    }
    
    if(length(input$include)!=0){
      if(input$include=="yes"){
        incdef <- getDefaults()$id
        excdef <- input$select
      } else {
        incdef <- input$select
        excdef <- getDefaults()$id
      }
    } else {
      incdef <- getDefaults()$id
      excdef <- input$select
    }
    
    if(input$runtype=="NPAG"){
      arglist <- c(
        FormatArg("model",input$model,"model.txt",T),
        FormatArg("data",input$data,"data.csv",T),
        FormatArg("include",input$select,incdef),      
        FormatArg("exclude",input$select,excdef),
        FormatArg("ode",input$ode,-4),
        FormatArg("salt",c(unlist(strsplit(as.character(input$salt),","))),as.character(rep(1,getDefaults()$ndrug))),
        FormatArg("cycles",input$cycles,100),
        FormatArg("indpts",input$indpts,getDefaults()$trans$indpts),
        FormatArg("icen",input$icen,"median",T),
        FormatArg("aucint",input$aucint,getDefaults()$aucint),
        FormatArg("idelta",input$idelta,12),
        FormatArg("prior",input$prior,"",ifelse(length(grep("DEN",input$prior))>0,T,F))
      )
    } #end NPAG
    
    if(input$runtype=="IT2B"){
      
      arglist <- c(
        FormatArg("model",input$model,"model.txt",T),
        FormatArg("data",input$data,"data.csv",T),
        FormatArg("include",input$select,incdef),      
        FormatArg("exclude",input$select,excdef),
        FormatArg("ode",input$ode,-4),
        FormatArg("salt",c(unlist(strsplit(as.character(input$salt),","))),as.character(rep(1,getDefaults()$ndrug))),
        FormatArg("cycles",input$cycles,100),
        FormatArg("xsig",input$xsig,0.5),
        FormatArg("xtol",input$xtol,0.001),
        FormatArg("xdev",input$xdev,5)      )
    }#end IT2B
    
    
    arglist <- arglist[-which(arglist=="")]
    arglist <- paste(arglist,collapse=", ")
    if(getDefaults()$error){return("Improper data file selected")
    } else {
      trans <- getDefaults()$trans
      if(trans$status==-1) {return(trans$msg)
      } else {
        if(input$runtype=="NPAG") {return(paste("NPrun(",arglist,")",sep=""))}
        if(input$runtype=="IT2B") {return(paste("ITrun(",arglist,")",sep=""))}
      }
    }
  } #end makeRunStatement
  
  makeLoadStatement <- function(){
    if(!getDefaults()$error & length(getDefaults()$trans$blocks)>0){
      if(input$runtype=="NPAG") {return(paste("PMload(",getDefaults()$newdir,")",sep=""))}
      if(input$runtype=="IT2B") {return(paste("PMload(",getDefaults()$newdir,")",sep=""))}
    }
  } #end makeLoadStatement
  
  #################################################### 
  ############### Build the Page #####################
  #################################################### 
  
  output$DataControls <- renderUI({makeDataControls()})
  output$RunControls <-renderUI({makeRunControls()})
  output$RunCode <- renderText({makeRunStatement()})
  output$LoadCode <- renderText({makeLoadStatement()})
  
  ############### Model Report #######################
  
  #model primary variables
  output$primVar <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    primVar <- data.frame(name=trans$blocks$primVar,min=trans$ab[,1],max=trans$ab[,2])
  },include.rownames=F)
  
  #model covariates
  output$covar <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    covnames <- trans$blocks$covar
    if(covnames[1]!="") {
      ncov <- length(covnames)
      ctype <- rep("Interpolated",ncov)
      piecewise <- grep("\\+",covnames)
      if(length(piecewise)>0){
        ctype[piecewise] <- "Piecewise constant"
        covnames <- gsub("\\+","",covnames)
      }
      covar <- data.frame(name=covnames,type=ctype)
    } else {covar <- data.frame(name="None",type="NA")}
    
  },include.rownames=F)
  
  #model secondary variables
  output$secVar <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    secVarDec <- trans$blocks$secVar
    if(secVarDec[1]!="") {
      secVar <- data.frame(declaration=secVarDec)
    } else {secVar <- data.frame(declaration="None")}
  },include.rownames=F,include.colnames=F)
  
  #model bolus equations
  output$bolus <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    bolusDec <- trans$blocks$bolus
    if(bolusDec[1]!="") {
      bolus <- data.frame(declaration=bolusDec)
    } else {bolus <- data.frame(declaration="None")}
  },include.rownames=F,include.colnames=F)
  
  #model initial conditions
  output$IC <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    ICDec <- trans$blocks$ini
    if(ICDec[1]!="") {
      IC <- data.frame(declaration=ICDec)
    } else {IC <- data.frame(declaration="None")}
  },include.rownames=F,include.colnames=F)
  
  #model lag times
  output$tlag <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    tlagDec <- trans$blocks$lag
    if(tlagDec[1]!="") {
      tlag <- data.frame(declaration=tlagDec)
    } else {tlag <- data.frame(declaration="None")}
  },include.rownames=F,include.colnames=F)
  
  #model diffeq
  output$diffeq <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    diffeqDec <- trans$blocks$diffeq
    if(diffeqDec[1]!="") {
      diffeq <- data.frame(declaration=diffeqDec)
    } else {diffeq <- data.frame(declaration="None")}
  },include.rownames=F,include.colnames=F)
  
  #model output
  output$output <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    outputDec <- trans$blocks$output
    if(outputDec[1]!="") {
      output <- data.frame(declaration=outputDec)
    } else {output <- data.frame(declaration="None")}
  },include.rownames=F,include.colnames=F)
  
  #model error
  output$error <- renderTable({
    trans <- getDefaults()$trans
    if(length(trans$blocks)==0) return(NULL)
    errorDec <- tolower(gsub("[[:space:]]","",trans$blocks$error))
    gamlam <- grep("^g|^l",errorDec)
    fixed <- grep("\\+",errorDec[gamlam[1]])
    ierr <- unlist(strsplit(errorDec[gamlam[1]],"="))
    ierrtype <- gsub("[[:space:]]","",tolower(substr(ierr[1],1,1)))
    ierrtype <- switch(ierrtype,g="Gamma",l="Lambda")
    if(length(fixed)>0) {ierrtype <- c(ierrtype,"Fixed")} else { ierrtype <- c(ierrtype,"Estimated")}
    ierrtype[2] <- paste(ierrtype[2],", initial=",ierr[2],sep="")
    
    asserr <- errorDec[-gamlam]
    assfix <- grep("\\+",asserr)
    numeqt <- length(asserr)
    etype <- rep("If not in datafile",numeqt)
    if(length(assfix)>0) {
      etype[assfix] <- "Override"
      asserr <- gsub("\\+","",asserr)
    }
    error <- data.frame(declaration=c(ierrtype[1],asserr),type=c(ierrtype[2],etype))
  },include.rownames=F)
  
  
}) #end shinyServer
