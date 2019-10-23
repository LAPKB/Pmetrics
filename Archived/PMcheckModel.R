#' Report on Pmetrics model characteristics as found in model template file.
#'
#' @title Check Pmetrics Model
#'
#' A new model fortran file will not be written.  This function is a wrapper for the Pmetrics utility function \code{makeModel},
#' which will be called with the \code{write} argument disabled.
#' @param model The filename of the Pmetrics model template file. Default is \dquote{model.txt}.
#' @param data The filename of the Pmetrics data csv file. Default is \dquote{data.csv}.
#' @return NULL, but will print a summary of the model file, including the solver mode (algebraic or ordinary differential equations), number of compartments,
#' primary variables to be estimated, included covariates, secondary variables defined in the model, and model conditions including,
#' bioavailability term(s), initial conditions of compartments, and lag term(s).
#' @author Michael Neely


PMcheckModel <- function(model="model.txt",data="data.csv"){
  #check for files
  while(!file.exists(model)) {
    model <- readline(paste("The model file",shQuote(paste(getwd(),model)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
    if (tolower(model)=="end") {endNicely(paste("No model file specified.\n"),model=-99,data); break}
  }
  while(!file.exists(data)) {
    data <- readline(paste("The data file",shQuote(paste(getwd(),data)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
    if (tolower(data)=="end") {endNicely(paste("No data file specified.\n"),model,data=-99); break}
  }
  
  #get information from datafile
  dataFile <- PMreadMatrix(data,quiet=T)
  dataoffset <- 2*as.numeric("addl" %in% names(dataFile))
  ncov <- ncol(dataFile)-(12+dataoffset)
  if(ncov>0) {covnames <- names(dataFile)[(13+dataoffset):ncol(dataFile)]} else {covnames <- NA}
  numeqt <- max(dataFile$outeq,na.rm=T)  
  
  modeltxt <- model
  #attempt to translate model file into separate fortran model file and instruction files
  engine <- list(alg="NP",ncov=ncov,covnames=covnames,numeqt=numeqt,indpts=-99,limits=NA)
  
  trans <- makeModel(model=model,data=data,engine=engine,write=F,silent=F)
  
  if(trans$status==-1) endNicely(trans$msg,modeltxt,data) #error
  if(trans$status==0) {
    cat(paste("\nAssociated data file: ",data,sep=""))
    cat("\nYou are using a Fortran model file rather than the new text format.\nThis is permitted, but see www.lapk.org/ModelTemp.php for details.\n")
    invisible(return())
  }
  if(trans$status==1){
    cat(paste("\nAssociated data file: ",data,sep=""))
    cat("\nExcellent - there were no errors found in your model file.\n")
    if(trans$model %in% Sys.glob("*",T)){
      file.remove(trans$model)
    }
    invisible(return())
  }
}
