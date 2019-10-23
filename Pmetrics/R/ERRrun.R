#' Runs Assay Error Module
#'
#' \code{ERRrun} will execute an Assay Error run to estimate error polynomial coefficients.
#'
#' If all function arguments are default, the simplest execution of this command is 
#' \code{ERRrun()}.  This will result in generation of a batch file.  On Unix (Mac) systems
#' will be launched automatically in a terminal window.  On Windows systems, the user
#' must execute the batch file from the current working directory, which will launch the estimation
#' program in a command prompt (DOS-like) window.  In either case, it will run independently of R
#' so that R can be used for other purposes if desired.
#'
#' @title Execute an Assay Error Estimation run.
#' @param model Name of a suitable model file template in the working directory or
#' an existing (previous) run number corresponding to a folder in the current working directory that used the same model file as will be used in the current run.
#' If this is supplied, then the model file will be copied into the current 
#' working directory for convenience.  If not supplied, 
#' the default is \dQuote{model.txt}.  This file will be converted to a fortran model file.
#' If it is detected to already be a fortran file, then the analysis will proceed without any further
#' file conversion.
#' @param data Name of a suitable data file (see \code{\link{PMwriteMatrix}}) or
#' an existing (previous) run number corresponding to a folder in the current working directory that used the same data file as will be used in the current run.
#' If this is supplied, then previously made  '.ZMQ' files will be copied into the current 
#' working directory, bypassing the need to re-convert the .csv file and speeding up the run..
#' @param run Specify the run number of the output folder.  Default if missing is the next available number.
#' @param include Vector of subject id values in the data file to include in the analysis.  The default (missing) is all.
#' @param exclude A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)
#' @param ode Ordinary Differential Equation solver log tolerance or stiffness.  Default is -4, i.e. 0.0001.  Higher values will result in faster
#' runs, but parameter estimates may not be as accurate.
#' @param tol Tolerance for convergence, with default of 0.001.
#' @param salt Vector of salt fractions for each ndrug, default is 1 for each drug.  This is not the same as bioavailability.
#' @param cycles Number of cycles to run. Default is 100.
#' @param search Default is "cursory", but can be "medium" or "extensive", which take progressively
#' longer times to converge, but are more accurate.
#' @param xdev Multiple of standard deviations for parameters to be sent to NPAG as a range.  Default is 5.
#' @param auto If \code{auto} is \code{False} you can answer all questions about the run environment manually.  This might
#' be helpful for beginners.  Default is \code{True}.
#' @param intern MacOSX only: Run ERR in the R console without a batch script.  Default is false.  
#' This will be ignored on Windows systems.  On the latter, the behavior of cmd.exe (aka the \dQuote{DOS} window)
#' with R is poor - it does not update until the end of execution, so you cannot see any output that indicates that ERR is running.  
#' If \code{intern=T} the HTML summary page will not be automatically loaded at the end of the run, but all post-run processing will occur normally,
#' and you can find the HTML summary page in the /outputs folder: ERRreport.html.
#' @param silent Boolean operator controlling whether a model summary report is given.  Default is \code{True}.
#' @param nocheck Suppress the automatic checking of the data file with \code{\link{PMcheck}}.  Default is \code{FALSE}.
#' @param overwrite Overwrite existing run result folders.  Default is FALSE.
#' @return A successful  run will result in creation of a new folder in the working
#' directory.  This folder will be named with a date-time stamp in the format "out-YYYYMMMDD-hhmm",
#' e.g. out-2011Apr10-1015.  Under this folder will be four subfolders: etc, inputs, outputs, and
#' wrkcopy, described below.
#' \itemize{
#'  \item \bold{etc}   Control files generally not needed by the user after a completed run.
#'  \item \bold{inputs}   This folder will contain the .csv data file and the model file.
#'  \item \bold{outputs}   This folder will contain the output from the run: a file that will be
#' prefixed by ASS with appended numbers, usually 0001. This file contains all the output of the
#' run, with the estimated assay error polynomical coefficients at the end.
#' \item \bold{wrkcopy}    The working copy format which is used by the program.  Invisibly to the user,
#' the .csv input file is converted to these text files, one file per subject.  
#' }
#' 
#' @author Michael Neely
#' @seealso \code{\link{ITrun}}, \code{\link{NPrun}}


ERRrun <- function(model="model.txt",data="data.csv",run,include,exclude,
                   ode=-4,tol=0.001,salt,cycles=100,search="cursory",
                   xdev=5,auto=T,intern=F,silent=F,
                   overwrite=F,nocheck=F){
  
  if(missing(run)) run <- NULL
  if(missing(include)) include <- NULL
  if(missing(exclude)) exclude <- NULL
  if(missing(salt)) salt <- NULL
  
  
  outpath <- .PMrun(type="ERR",model=model,data=data,run=run,
                   include=include,exclude=exclude,ode=ode,tol=tol,salt=salt,cycles=cycles,
                   search=search,xdev=xdev,icen=NULL,
                   auto=auto,intern=intern,silent=silent,overwrite=overwrite,nocheck=nocheck,parallel=F)
  return(outpath)
  
}
