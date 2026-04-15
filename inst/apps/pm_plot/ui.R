ui <- shiny::fluidPage(
  shiny::titlePanel("Pmetrics Plot"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("data", "Choose a Pmetrics object to plot:", choices = character(0)),
      shiny::uiOutput("method_info"),
      shiny::tags$hr(),
      shiny::tags$label("Plot arguments"),
      shiny::helpText("Enter named arguments exactly as you would inside plot(...), separated by commas."),
      shiny::textAreaInput(
        "plot_args",
        label = NULL,
        rows = 6,
        placeholder = 'icen = "median", outeq = 1'
      ),
      shiny::checkboxInput("auto_refresh", "Auto-refresh plot", value = TRUE),
      shiny::actionButton("plot_now", "Update plot", class = "btn-primary"),
      shiny::tags$hr(),
      shiny::checkboxInput("show_code", "Show reproducible code", value = TRUE)
    ),
    shiny::mainPanel(
      shiny::plotOutput("PMplot", height = "700px"),
      shiny::conditionalPanel(
        condition = "input.show_code == true",
        shiny::tags$h4("Code"),
        shiny::verbatimTextOutput("plotCode")
      )
    )
  )
)

ui
