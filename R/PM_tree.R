#' @title Create a new Pmetrics folder tree
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Sets up a directory tree for a new Pmetrics project
#' @details
#' This function will create a new project folder tree with appropriate subfolders and a skeleton R script.
#' You can edit the folder names and add new ones as you wish.
#'
#' @param project A character string of a new project name, e.g. "DrugX"
#' @param path The full path to the root folder for the new project.  Default is the
#' current working directory.
#' @return A new folder with the name in `project` and the following subfolders:
#' 
#' * **Rscript** The folder containing a skeleton *Analysis.R* script for the project.
#' * **Runs** The folder for all Pmetrics runs, which will be sequentially numbered.
#' * **Sim** The folder for all simulations related to the project.
#' * **src** The folder for source data files in their original format, to preserve
#' integrity and for audit purposes.
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
    cli::cli_abort(c("x" = "Project {.key {project}} exists already.", "i" = "Delete it if you are certain, and rerun PM_tree."))
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
      "# Set your working directory.",
      glue::glue("setwd(\"{newFolder}\")"),
      "",
      "# Ensure your model and data files are in the /src folder",
      "",
      "# Make the data object",
      "dat <- PM_data$new(\"src/your_Data.csv\") # replace with your data file name",
      "",
      "# Make a model in R. Here is a simple two-compartment IV and bolus model",
      "mod1 <- PM_model$new(",
      "  pri = list(",
      "     Ka = ab(0, 5),",
      "     Ke  = ab(0, 5),",
      "     V   = ab(0, 100)",
      "  ),",
      "  eqn = function(){",
      "    two_comp_bolus  # see other library models with model_lib()",
      "  },",
      "  out = function(){",
      "    Y[1] = X[2]/V",
      "  },",
      "  err = list(",
      "    proportional(3, c(0.5, 0.1, 0, 0))",
      "  )",
      ")",
      "",
      "# Or make the model object from a file you created",
      "mod1 <- PM_model$new(\"src/your_Model.txt\") # replace with your model file name",
      "",
      "# Now fit the model to the data. If you want to omit the path argument, setwd(\"Runs\") first.",
      "run1 <- mod1$fit(data = dat, path = \"Runs\")",
      "",
      "# run1 is in memory after the above completes, but to load the results if returning later...",
      glue::glue("setwd(\"{newFolder}/Runs\") # convenient to repeat here"),
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
    con = file.path(newFolder, "Rscript/Analysis.R")
  )
}


#' @rdname PM_tree
#' @param ... Arguments passed to [PM_tree].
#' @export
PMtree <- function(...) {
  PM_tree(...)
}
