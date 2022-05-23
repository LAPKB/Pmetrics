#' Sets up a directory tree for a new Pmetrics project
#'
#' This function will create a new project folder tree with appropriate subfolders and a skeleton R script.
#'
#' @title Create a new Pmetrics folder tree
#' @param project A character string of a new project name, e.g. "DrugX"
#' @param folder The full path to the root folder for the new project.  Default is the
#' current working directory.
#' @return A new folder named \code{project} with the following subfolders:
#' \item{Rscript }{The folder for the Rscript containing all run instructions.
#' Within this folder will be a skeleton R script for the project.}
#' \item{Runs }{The folder for all Pmetrics runs.  Put run files, i.e. a data file and a
#' model file in this directory prior to each run.}
#' \item{Sim }{The folder for all simulations related to the project.}
#' \item{src }{The folder for source data files in their original format, to preserve
#' integrity and for audit purposes.}
#' @author Michael Neely
#' @seealso \code{\link{PMmanual}}
#' @examples
#' PMtree("DrugX")
#' @export

PMtree <- function(project="NewProject",folder=getwd()){
  newFolder <- paste(folder,project,sep="/")
  current <- getwd()
  if(file.exists(newFolder)){
    stop(paste(project," exists already.\nDelete it if you are certain, and rerun PMtree.\n"))
  }
  dir.create(newFolder)
  setwd(newFolder)
  dir.create("Runs")
  dir.create("Rscript")
  dir.create("Sim")
  dir.create("src")
  writeLines(c("#Use PMmanual() for help",
               "library(Pmetrics)",
               "",
               "#Run 1 - add your run description here",
               "",
               paste("setwd(\"",newFolder,"/Runs\")",sep=""),
               "#Ensure your model and data files are in the /Runs folder",
               "#Make the data object",
               "dat <- PM_data$new(\"[replace with your data file name]\")",
               "#Make the model object",
               "mod1 <- PM_model$new(\"[replace with your model file name]\")",
               "#Make the fit object",
               "fit1 <- PM_fit$new(dat, mod1)",
               "#Run fit1",
               "fit1$run()",
               "#Load the results after it completes",
               "run1 <- PM_load(1)",
               "",
               "#Plots",
               "",
               "run1$op$plot()",
               "run1$op$plot(pred.type=\"pop\")",
               "",
               "run1$final$plot()",
               "#Summaries",
               "",
               "run1$op$summary()",
               "run1$final$summary()",
               
               ),
             con=paste(newFolder,"/Rscript/",project,".R",sep=""))
  setwd(current) 
}
