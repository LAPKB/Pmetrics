ui <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

shiny::shinyApp(ui = ui, server = server)
