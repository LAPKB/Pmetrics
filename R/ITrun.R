#' @title Execute an IT2B run.
#' @description
#' `r lifecycle::badge("superseded")`
#' Runs a parametric IT2B search for parameter values.
#'
#' @details
#' This is largely superseded by the `$run` method for [PM_fit] objects. 
#' 
#' If all function arguments are default, the simplest execution of this command is 
#' \code{ITrun()}.  This will result in generation of a batch file.  On Unix (Mac) systems
#' will be launched automatically in a terminal window.  On Windows systems, the user
#' must execute the batch file from the current working directory, which will launch IT2B
#' in a command prompt (DOS-like) window.  In either case, IT2B will run independently of R
#' so that R can be used for other purposes if desired.
#'

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
#' @param xdev Multiple of standard deviations for parameters to be sent to NPAG as a range.  Default is 5.
#' @param icen Summary of parameter distributions to be used to calculate predictions in HTML report.  Default is "median", but could be "mean".  
#' #Predictions based on both summaries will be available in objects loaded by \code{\link{PMload}}.
#' @param auto If \code{auto} is \code{False} you can answer all questions about the run environment manually.  This might
#' be helpful for beginners.  Default is \code{True}.
#' @param intern MacOSX only: Run IT2B in the R console without a batch script.  Default is false.  
#' This will be ignored on Windows systems.  On the latter, the behavior of cmd.exe (aka the \dQuote{DOS} window)
#' with R is poor - it does not update until the end of execution, so you cannot see any output that indicates that IT2B is running.  
#' If \code{intern=T} the HTML summary page will not be automatically loaded at the end of the run, but all post-run processing will occur normally,
#' and you can find the HTML summary page in the /outputs folder: IT2Breport.html.
#' @param quiet Boolean operator controlling whether a model summary report is given.  Default is \code{True}.
#' @param overwrite Overwrite existing run result folders.  Default is \code{FALSE}.
#' @param nocheck Suppress the automatic checking of the data file with \code{\link{PMcheck}}.  Default is \code{FALSE}.
#' @param alq For internal developer use only.  Should be set to \code{FALSE}.
#' @param report Generate a report at the end of a run. Default is `TRUE`.
#' @return A successful IT2B run will result in creation of a new folder in the working
#' directory. This folder will be named numerically and sequentially with respect to previous runs.   
#' Within this folder will be four subfolders: etc, inputs, outputs, and
#' wrkcopy, described below.
#' \itemize{
#'  \item \bold{etc}   Control files for IT2B generally not needed by the user after a completed run.
#'  \item \bold{inputs}   This folder will contain the .csv data file and the model file.
#'  \item \bold{outputs}   This folder will contain the output from the IT2B run.  These files will be
#' prefixed by DENF, ILOG, OUTF, OUFF, LAST, FROM and RFILE, with appended numbers, usually 0001.
#' DEN is the density file which contains the joint posterior density which can be passed to IT2B.
#' OUTF and OUFF are full and truncated textfiles containing all output of IT2B.  OUFF is missing
#' density file.  LAST contains last cycle Bayesian posterior parameters and predictions for
#' each subject.  FROM contains estimated parameter ranges which can be passed to IT2B.
#' RFILE contains IT2B output formatted for easy import into R, and is the file read by
#' the \code{\link{ITparse}} command.  Finally, there will also be an itlog.txt file
#' containing additional run information.
#' \item \bold{wrkcopy}    The working copy format which is used by IT2B.  Invisibly to the user,
#' the .csv input file is converted to these text files, one file per subject.  
#' }
#' 
#' @author Michael Neely
#' @seealso \code{\link{ITparse}}, \code{\link{NPrun}}
#' @export


ITrun <- function(model="model.txt",data="data.csv",run,
                  include,exclude,ode=-4,tol=0.001,salt,cycles=100,
                  xdev=5,icen="median",
                  auto=T,intern=F,quiet=F,overwrite=F,nocheck=F,alq=F, report = T){
  
  if(missing(run)) run <- NULL
  if(missing(include)) include <- NULL
  if(missing(exclude)) exclude <- NULL
  if(missing(salt)) salt <- NULL

  batch <- F
  outpath <- .PMrun(type="IT2B",model=model,data=data,run=run,
                   include=include,exclude=exclude,ode=ode,tol=tol,salt=salt,cycles=cycles,icen=icen,
                   xdev=xdev,
                   auto=auto,intern=intern,quiet=quiet,overwrite=overwrite,nocheck=nocheck,parallel=F,batch=batch,alq=alq,
                   report = report)
  return(outpath)
  
}
