#' @title Create a new Pmetrics folder tree
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Sets up a directory tree for a new Pmetrics project
#' @details
#' This function will create a new project folder tree with appropriate subfolders and a skeleton R script.
#'
#' @param project A character string of a new project name, e.g. "DrugX"
#' @param path The full path to the root folder for the new project.  Default is the
#' current working directory.
#' @return A new folder named \code{project} with the following subfolders:
#' \item{Rscript }{The folder for the Rscript containing all run instructions.
#' Within this folder will be a skeleton R script for the project.}
#' \item{Runs }{The folder for all Pmetrics runs.  Put run files, i.e. a data file and a
#' model file in this directory prior to each run.}
#' \item{Sim }{The folder for all simulations related to the project.}
#' \item{src }{The folder for source data files in their original format, to preserve
#' integrity and for audit purposes.}
#' @aliases PMtree
#' @author Michael Neely
#' @seealso [PM_manual]
#' @examples
#' \dontrun{
#' PM_tree("DrugX")
#' }
#' @export

PM_tree <- function(project = "NewProject", path = getwd()) {
  newFolder <- file.path(path, project)
  if (file.exists(newFolder)) {
    stop(paste(project, " exists already.\nDelete it if you are certain, and rerun PMtree.\n"))
  }
  dir.create(newFolder)
  dir.create(file.path(newFolder, "Runs"))
  dir.create(file.path(newFolder, "Rscript"))
  dir.create(file.path(newFolder, "Sim"))
  dir.create(file.path(newFolder, "src"))
  writeLines(
    c(
      "# Use PM_manual() for help",
      "",
      "library(Pmetrics)",
      "",
      "# Run 1 - add your run description here",
      "",
      glue::glue("wd <- \"{newFolder}\""),
      "# Ensure your model and data files are in the /src folder",
      "# Make the data object",
      "dat <- PM_data$new(file.path(wd, \"src/your_Data.csv\")) # replace with your data file name",
      "# Make the model object",
      "mod1 <- PM_model$new(file.path(wd, \"your_Model.txt\")) # replace with your model file name",
      "# Fit model to data",
      "run1 <- mod1$fit(data = dat)",
      "# run1 is in memory after the above completes, but to load the results if returning later...",
      "run1 <- PM_load(1)",
      "",
      "# Plots",
      "run1$op$plot() # posterior predictions based on median",
      "run1$final$plot()",
      "",
      "# Summaries",
      "run1$op$summary()",
      "run1$final$summary()"
    ),
  )
}


#' @rdname PM_tree
#' @param ... Arguments passed to [PM_tree].
#' @export
PMtree <- function(...) {
  PM_tree(...)
}
