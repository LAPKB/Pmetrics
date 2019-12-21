#' Runs NPAG
#'
#' \code{NPrun} will execute an NPAG run.
#'
#' If all function arguments are default, the simplest execution of this command is 
#' \code{NPrun()}.  This will result in generation of a batch file.  On Unix (Mac) systems
#' will be launched automatically in a terminal window.  On Windows systems, the user
#' must execute the batch file from the current working directory, which will launch NPAG
#' in a command prompt (DOS-like) window.  In either case, NPAG will run independently of R
#' so that R can be used for other purposes if desired.
#'
#' @title Execute an NPAG run.
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
#' @param exclude A vector of subject IDs to exclude in the analysis, e.g. c(4,6:14,16:20)
#' @param ode Ordinary Differential Equation solver log tolerance or stiffness.  Default is -4, i.e. 0.0001.  Higher values will result in faster
#' runs, but parameter estimates may not be as accurate.
#' @param tol Tolerance for convergence of NPAG.  Smaller numbers make it harder to converge.
#' Default value is 0.01.
#' @param salt Vector of salt fractions for each drug in the data file, default is 1 for each drug.  This is not the same as bioavailability.
#' @param cycles Number of cycles to run. Default is 100.
#' @param indpts Index of starting grid point number.  Default is missing, which allows NPAG to choose depending on the number of random parameters: 
#' 1 or 2 = index of 1; 3 = 3; 4 = 4, 5 = 6,
#' 6 or more is 10+number of multiples for each parameter greater than 5, e.g. 6 = 101; 7 = 102, up to 108 for 13 or more parameters.
#' @param icen Summary of parameter distributions to be used to calculate predictions in HTML report.  Default is "median", but could be "mean".  
#' Predictions based on both summaries will be available in objects loaded by \code{\link{PMload}}.
#' @param aucint Maintained for backwards compatibility and not used currently. Interval for AUC calculations.  Default is 24 hours if the number of intervals is not greater than 48; otherwise it defaults
#' to the interval which allows for <= 48 intervals.
#' @param idelta Interval in 1/60 time unit, typically minutes, for predictions at times other than observations.  Default is 12.
#' @param prior Name of a suitable NPAG output object from a prior run loaded with \code{\link{PMload}},
#' i.e. the \emph{NPdata} object.  A \code{prior} may be specified if the user wishes to
#' start from a non-uniform prior distribution for the NPAG run. The default value is -99,
#' which translates in NPAG to a uniform prior distribution.  An alternative is to include a DEN0001 file from the prior
#' NPAG run in the working directory of the new run, and specify this as the value for \code{prior}, e.g.
#' \code{prior = 'DEN0001'}.
#' @param auto If \code{auto} is \code{False} you can answer all questions about the run environment manually.  This might
#' be helpful for beginners.  Default is \code{True}.
#' @param intern MacOSX only: Run NPAG in the R console without a batch script.  Default is false.  
#' This will be ignored if on Windows systems.  On the latter, the behavior of cmd.exe (aka the \dQuote{DOS} window)
#' with R is poor - it does not update until the end of execution, so you cannot see any output that indicates that NPAG is running.  
#' If \code{intern=T} the HTML summary page will not be automatically loaded at the end of the run, but all post-run processing will occur normally,
#' and you can find the HTML summary page in the /outputs folder: NPAGreport.html.
#' @param silent Boolean operator controlling whether a model summary report is given.  Default is \code{TRUE}.
#' @param overwrite Overwrite existing run result folders.  Default is \code{FALSE}.
#' @param nocheck Suppress the automatic checking of the data file with \code{\link{PMcheck}}.  Default is \code{FALSE}.
#' @param batch Set to true when \code{\link{PMbatch}} is used.
#' @param parallel Run NPAG in parallel.  Default is \code{NA}, which will be set to \code{TRUE} for models that use
#' differential equations, and \code{FALSE} for algebraic/explicit models.  The majority of the benefit for parallelization comes
#' in the first cycle, with a speed-up of approximately 80\% of the number of available cores on your machine, e.g. an 8-core machine
#' will speed up the first cycle by 0.8 * 8 = 6.4-fold.  Subsequent cycles approach about 50\%, e.g. 4-fold increase on an 8-core
#' machine.  Overall speed up for a run will therefore depend on the number of cycles ru and the number of cores.
#' @return A successful NPAG run will result in creation of a new folder in the working
#' directory.  This folder will be named with a date-time stamp in the format "out-YYYYMMMDD-hhmm",
#' e.g. out-2011Apr10-1015.  Under this folder will be four subfolders: etc, inputs, outputs, and
#' wrkcopy, described below.
#' \itemize{
#'  \item \bold{etc}   Control files for NPAG generally not needed by the user after a completed run.
#'  \item \bold{inputs}   This folder will contain the .csv data file and the model file.
#'  \item \bold{outputs}   This folder will contain the output from the NPAG run.  These files will be
#' prefixed by DEN, ILOG, OUT, OUTT, PRTB and RFILE, with appended numbers, usually 0001.
#' DEN is the density file which can be used to specifiy a non-uniform prior parameter value
#' distribution for a subsequent NPAG run of the same model via the \code{prior} argument
#' above.  ILOG is a summary of cycle objective function values, gamma/lambda, and gridpoints.
#' OUT and OUTT are full and truncated textfiles containing all output of NPAG.  OUTT is missing
#' density file.  PRTB contains Bayesian posterior individual predictions for each subject and
#' output at timepoints specified in the NPAG instructions (e.g. every 2, 4, 8, 12 minutes) as well
#' as predictions at each observation time.  RFILE contains NPAG output formatted for easy import
#' into R, and is the file read by the \code{\link{NPparse}} command.  Finally, there will also
#' be an nplog.txt file containing additional run information.
#' \item \bold{wrkcopy}    The working copy format which is used by NPAG.  Invisibly to the user,
#' the .csv input file is converted to these text files, one file per subject.  
#' }
#' 
#' @author Michael Neely
#' @seealso \code{\link{NPparse}}, \code{\link{ITrun}}


NPrun <- function(model = "model.txt", data = "data.csv", run,
                  include, exclude, ode = -4, tol = 0.01, salt, cycles = 100,
                  indpts, icen = "median", aucint,
                  idelta = 12, prior,
                  auto = T, intern = F, silent = F, overwrite = F, nocheck = F, parallel = NA, batch = F, server = F,
                  remoterun, remoteuser, remoteproject, remotecontact) {

  if (missing(run)) run <- NULL
  if (missing(include)) include <- NULL
  if (missing(exclude)) exclude <- NULL
  if (missing(salt)) salt <- NULL
  if (missing(indpts)) indpts <- NULL
  if (missing(aucint)) aucint <- NULL
  if (missing(prior)) prior <- NULL
  if (missing(remoterun)) remoterun <- F
  if (missing(remoteuser)) remoteuser <- "you@your.server"
  if (missing(remoteproject)) remoteproject <- "shortNameOfProjectWithNoSpecialCharacters"
  if (missing(remotecontact)) remotecontact <- "yourEmailAddress"

  outpath <- .PMrun(type = "NPAG", model = model, data = data, run = run,
                   include = include, exclude = exclude, ode = ode, tol = tol, salt = salt, cycles = cycles,
                   indpts = indpts, icen = icen, aucint = aucint,
                   idelta = idelta, prior = prior,
                   auto = auto, intern = intern, silent = silent, overwrite = overwrite, nocheck = nocheck, parallel = parallel, batch = batch, server = F,
                   remoterun = remoterun, remoteuser = remoteuser, remoteproject = remoteproject, remotecontact = remotecontact)
  return(outpath)

}

