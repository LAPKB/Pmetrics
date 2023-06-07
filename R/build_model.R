#' Launch Model Builder app
#' 
#' Open the shiny model builder app.
#' 
#' @details
#' The app will open in a separate window.
#'
#' @param data Optional [PM_data] object which will be used to read
#' covariates.
#' @param ... Currently unused
#' @return Launches the shiny app.
#' @export
#' @author Michael Neely
#' 
build_model <- function(data, ...) {
  if(missing(data)) data <- NULL
  file_path <- system.file("Shiny/ModelBuilder/app.R", package = "Pmetrics")
  #file_path <- "inst/Shiny/ModelBuilder/app.R" #comment out this when installed
  
  if (!nzchar(file_path)) stop("Shiny app not found")
  ui <- server <- NULL # avoid NOTE about undefined globals
  source(file_path, local = TRUE)
  server_env <- environment(server)
  server_env$data <- data
  
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, ...)
}

