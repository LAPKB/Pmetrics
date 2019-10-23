#' Loads all the data from an \emph{NPAG} or \emph{IT2B} run
#'
#'
#'
#' @title Load Pmetrics NPAG or IT2B output
#' @param run The numerical value of the folder number containing the run results.  This
#' number will also be used to name objects uniquely by appending \dQuote{.\code{run}}, 
#' e.g. NPdata.1 or ITdata.1 if run=1. This parameter is \code{1} by default.
#' @param \dots Additional runs to load if desired.
#' @return The following objects are loaded into R.
#' \item{NPdata/ITdata }{List with all output from NPAG/IT2B}
#' \item{pop }{ NPAG only: Population predictions for each output equation}
#' \item{post }{ NPAG only: Individual posterior predictions for each output equation}
#' \item{final }{Final cycle population support points and parameter summary statistics}
#' \item{cycle }{Cycle log-likelihood, AIC, BIC, Gamma/lambda, and normalized parameter means, medians and SDs}
#' \item{op }{List of observed vs. population and posterior predicted plots for each output equation}
#' \item{cov }{Data frame of subject ID, covariate values, and Bayesian posterior parameter estimates}
#' \item{mdata }{The original .csv data file used in the run}
#' \item{npde }{If \code{\link{makeNPDE}} has been run after a run, this object will be added to 
#' the save data.  It contains the information required to plot and analzye normalized prediction
#' error discrepancies via the npde package of Comets et al}
#' \item{sim }{If \code{\link{makeNPDE}} has been run after a run, this list object will be added to 
#' the save data.  It contains the results of each subject in the dataset simulated n times (default 1000)
#' using the final model population parameters.  To plot the results of subject 3 from run 2, for example, use the form
#' \code{plot(sim.2[[3]])}}.
#' @author Michael Neely
#' @seealso \code{\link{PMreport}}, \code{\link{NPparse}}, \code{\link{ITparse}}, 
#' \code{\link{makeFinal}}, \code{\link{makeCycle}}, \code{\link{makeOP}}, \code{\link{makeCov}}, 
#' \code{\link{makePop}}, \code{\link{makePost}}

PMload <- function(run=1,...){
  addlruns <- list(...)
  if(length(addlruns)>0){
    allruns <- c(run,unlist(addlruns))
  } else {allruns <- run}
  for(thisrun in allruns){
    #check for NPAG output file
    filename <- "NPAGout.Rdata"
    outfile <- paste(thisrun,"outputs",filename,sep="/")
    if(file.exists(outfile)) filename <- outfile
    if(file.exists(filename)){
      load(filename)
      newNames <- paste(names(NPAGout),".",as.character(thisrun),sep="")
      for (i in 1:length(newNames)){
        assign(newNames[i],NPAGout[[i]],pos=parent.frame())
      }
    } else {
      #check for IT2B output file
      filename <- "IT2Bout.Rdata"
      outfile <- paste(thisrun,"outputs",filename,sep="/")
      if(file.exists(outfile)) filename <- outfile
      if(file.exists(filename)){
        load(filename)
        newNames <- paste(names(IT2Bout),".",as.character(thisrun),sep="")
        for (i in 1:length(newNames)){
          assign(newNames[i],IT2Bout[[i]],pos=parent.frame())
        }
      } else {
        cat(paste(filename," not found in ",getwd(),"/",thisrun,"/outputs or ",getwd(),".\n",sep=""))
        return(invisible(F)) #error, abort
      }
    }  
  } #end thisrun loop
  return(invisible(T)) #no errors
}

