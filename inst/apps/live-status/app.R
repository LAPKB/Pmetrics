shiny::shinyApp(
  ui = Pmetrics:::pmetrics_live_app_ui,
  server = Pmetrics:::pmetrics_live_app_server
)