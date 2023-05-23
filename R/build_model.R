#'
#' @export
#' 
build_model <- function(data, ...) {
  #if(missing(data) || !inherits(data,"PM_data")) stop("Please specify a PM_data object.\n")
  if(missing(data)) data <- NULL
  #file_path <- system.file("Shiny/ModelBuilder/app.R", package = "Pmetrics")
  file_path <- "inst/Shiny/ModelBuilder/app.R" #comment out this when installed
  
  if (!nzchar(file_path)) stop("Shiny app not found")
  ui <- server <- NULL # avoid NOTE about undefined globals
  #source("Utils.R", local = TRUE)
  source(file_path, local = TRUE)
  server_env <- environment(server)
  
  # Here you add any variables that your server can find
  server_env$data <- data
  
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, ...)
  #shiny::runGadget(app, viewer = dialogViewer("Pmetrics Model Builder", width = 1200, height = 1200))
}

