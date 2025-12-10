#' @title Pmetrics tutorial
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Introductory tutorial to Pmetrics
#' @details
#' This function will create a *Learn* folder in the current working
#' directory or one you specify. The folder will contain all files and subfolders necessary to
#' conduct the tutorial found at https://lapkb.github.io/PM_tutorial/. 
#' After the Learn folder is created, open the
#' *Rscript/Learn.R* script to begin the tutorial.
#' @return NULL
#' @export
#'
PM_tutorial <- function() {
  cli::cli_h1("Pmetrics Tutorial Setup")
  cli::cli_bullets(c(">" = "Setup will create a {.path Learn} folder in your current working directory: {.strong {getwd()}}.",
  ">" = "Use the files and folders in {.path Learn} to work through an introductory tutorial found at {.url https://lapkb.github.io/PM_tutorial}."))
  ans <- readline("\nEnter \"q\" to quit, <Return> to use your current working directory, or enter a new directory without quotes: ")
  
  if (tolower(substr(ans, 1, 1)) == "q") { # user said to quit
    cli::cli_text("Quitting...No files copied.")
    return(invisible())
  } 
  
  if (ans == "") { # user wants to use current working directory
    ans <- getwd()
  } 
  
  # now check if Learn exists in the specified location
  if (dir.exists(file.path(ans, "Learn"))) {
    ans2 <- readline("\"Learn\" already exists in this location. Overwrite (y/n)? ")
    if (tolower(substr(ans2, 1, 1)) == "n") {
      cli::cli_text("No files copied.")
      return(invisible())
    }
    
  }
  
  # copy the Learn folder and its contents and replace the working directory placeholder
  unlink(file.path(ans, "Learn"), recursive = TRUE, force = TRUE)
  dir.create(file.path(ans, "Learn"))
  dir.create(file.path(ans, "Learn", "Rscript"))
  dir.create(file.path(ans, "Learn", "Runs"))
  dir.create(file.path(ans, "Learn", "Sim"))
  dir.create(file.path(ans, "Learn", "src"))
  script <- c(
    "# This is an R script you can use to accompany the \"Introduction to Pmetrics\" online book.",
    "# You can find the book at https://lapkb.github.io/PM_tutorial/",
    "",
    "",
    "##### EXECUTE THIS CODE",
    "",
    "library(Pmetrics) # load the Pmetrics library",
    glue::glue("wd <- \"{file.path(ans, 'Learn')}\""),
    "setwd(wd) # all needed files will be here",
    "",
    "##### PASTE THE CODE FROM THE BOOK BELOW THIS LINE"
  )
  readr::write_lines(script, file.path(ans, "Learn", "Rscript", "Learn.R"))
  cli::cli_text("Click {.file {file.path(ans, 'Learn/Rscript/Learn.R')}} to proceed with the tutorial.")

  # create the sample data files
  data(modEx, dataEx)
  readr::write_csv(dataEx$data, file = file.path(ans, "Learn", "src", "ex.csv" ), na = ".")
  modEx$save(file = file.path(ans, "Learn", "src", "model.txt" ))
  # simulation template
  PM_data$new()$
  addEvent(id = 1, time = 0, dose = 500, addl = 5, ii = 24, wt = 46.7, africa = 1, age = 21, gender = 1, height = 180)$
  addEvent(id = 1, time = 144, out = -1)$
  addEvent(id = 2, time = 0, dose = 1000, addl = 5, ii = 24, wt = 46.7, africa = 1, age = 21, gender = 1, height = 180)$
  addEvent(id = 2, time = 144, out = -1)$
  addEvent(id = 3, time = 0, dose = 250, addl = 11, ii = 12, wt = 46.7, africa = 1, age = 21, gender = 1, height = 180)$
  addEvent(id = 3, time = 144, out = -1)$
  addEvent(id = 4, time = 0, dose = 500, addl = 11, ii = 12, wt = 46.7, africa = 1, age = 21, gender = 1, height = 180)$
  addEvent(id = 4, time = 144, out = -1, validate = TRUE, quiet = TRUE)$
  save(file = file.path(ans, "Learn", "src", "simTemp.csv" ))
  
  # pta template
  PM_data$new()$
  addEvent(id = 1, time = 0, dose = 600, addl = 5, ii = 24, wt = 46.7, africa = 1, age = 21, gender = 1, height = 180)$
  addEvent(id = 1, time = 144, out = -1)$
  addEvent(id = 2, time = 0, dose = 1200, addl = 5, ii = 24, wt = 46.7, africa = 1, age = 21, gender = 1, height = 180)$
  addEvent(id = 2, time = 144, out = -1)$
  addEvent(id = 3, time = 0, dose = 300, addl = 11, ii = 12, wt = 46.7, africa = 1, age = 21, gender = 1, height = 180)$
  addEvent(id = 3, time = 144, out = -1)$
  addEvent(id = 4, time = 0, dose = 600, addl = 11, ii = 12, wt = 46.7, africa = 1, age = 21, gender = 1, height = 180)$
  addEvent(id = 4, time = 144, out = -1, validate = TRUE, quiet = TRUE)$
  save(file = file.path(ans, "Learn", "src", "ptaex1.csv" ))
  
  return(invisible())
}
