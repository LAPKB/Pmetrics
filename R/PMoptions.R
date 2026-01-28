#' @title Get Pmetrics User Options
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Get user options for Pmetrics
#' @details
#' This function will get user options for Pmetrics. It will look for a *PMoptions.json* file
#' in a hidden folder outside of the Pmetrics package. If that does not exist,
#' it will look for a default options file in the package options folder. See [setPMoptions] for 
#' details on where the options file is stored and how to set options.
#'
#' @param opt The option to retrieve.  If omitted, all option values will be returned.
#' @param warn Warn if options file doesn't exist. Default `TRUE`.
#' @param quiet Suppress warning messages. Default `FALSE`.
#' @return A list with the current options.
#' @author Michael Neely
#' @export

getPMoptions <- function(opt, warn = TRUE, quiet = FALSE) {
  # check for existing options
  opt_dir <- dplyr::case_when(
    getOS() == 1 | getOS() == 3 ~ "~/.PMopts", # Mac, Linux
    getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts")
  )
  
  if (dir.exists(opt_dir)) { # external options file exists
    PMoptionsFile <- file.path(opt_dir, "PMoptions.json")
  } else { # external options file does not exist
    PMoptionsFile <- paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/")
  }
  
  
  # if it doesn't exist, warn and exit
  if (!file.exists(PMoptionsFile)) {
    if (warn & !quiet) cli::cli_inform("Run {.help setPMoptions} to create a Pmetrics options file.")
    return(invisible(-1))
  }
  
  # read the options file
  PMopts <- jsonlite::read_json(path = PMoptionsFile, simplifyVector = TRUE)
  if (missing(opt)) {
    return(PMopts)
  } else {
    index <- which(names(PMopts) == opt)
    if (length(index) == 0) {
      return(NULL)
    } else {
      return(PMopts[[index]])
    }
  }
}

#' @title Set Pmetrics User Options
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Set user options for Pmetrics
#' @details
#' When you call this function with the default `launch.app = TRUE`, it will start
#' a Shiny app to set options for the Pmetrics package.
#' Also, when the Pmetrics package is first loaded with `library(Pmetrics)`,
#' this function will be called with `launch.app = TRUE` to read saved options from
#' a *PMoptions.json* file stored in a folder outside
#' of the Pmetrics package, so that your options will persist when Pmetrics is updated. 
#'
#' @param launch.app Launch the app to set options. Default `TRUE`.
#' @return The user preferences file will be updated.  This will persist from session to session
#' and if stored in the external location, through Pmetrics versions.
#' @author Michael Neely
#' @export

setPMoptions <- function(launch.app = TRUE) {
  
  
  # --- Helper: OS Detection Function ---
  getOS <- function() {
    sysname <- Sys.info()[["sysname"]]
    if (sysname == "Darwin") return(1)      # Mac
    if (sysname == "Windows") return(2)     # Windows
    if (sysname == "Linux") return(3)       # Linux
    return(0)  # unknown
  }
  
  opt_dir <- dplyr::case_when(
    getOS() %in% c(1, 3) ~ fs::path_expand("~/.PMopts"),
    getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts"),
    TRUE ~ tempdir()  # fallback
  )
  
  fs::dir_create(opt_dir)  # ensure directory exists
  PMoptionsUserFile <- file.path(opt_dir, "PMoptions.json")
  
  # If file doesn't exist in user space, copy default
  if (!fs::file_exists(PMoptionsUserFile)) {
    PMoptionsFile <- glue::glue(system.file("options", package = "Pmetrics"), "/PMoptions.json")
    fs::file_copy(PMoptionsFile, PMoptionsUserFile, overwrite = TRUE)
  }
  
  app <- shiny::shinyApp(
    
    # --- UI ---
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(
        bootswatch = "flatly",
        primary = "#2c3e50",
        "card-border-radius" = "0.5rem"
      ),
      title = "Pmetrics Options",
      
      shiny::tags$div(
        class = "container-fluid p-4",
        
        # Header
        shiny::tags$div(
          class = "mb-4",
          shiny::tags$h2(
            class = "mb-1",
            shiny::icon("cog", class = "me-2"),
            "Pmetrics Options"
          ),
          shiny::tags$p(class = "text-muted mb-0", "Configure your Pmetrics preferences")
        ),
        
        # Main content layout - use fluidRow for proper spacing
        shiny::fluidRow(
          # Left column
          shiny::column(
            width = 6,
            # Data File Reading Card
            bslib::card(
              class = "mb-3",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::icon("file-csv", class = "me-2"),
                "Data File Reading"
              ),
              bslib::card_body(
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::selectInput(
                      "sep", 
                      bslib::tooltip(
                        shiny::tags$span("Field separator", shiny::icon("circle-question", class = "ms-1 text-muted")),
                        "Character used to separate fields in data files"
                      ),
                      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                      selected = ","
                    )
                  ),
                  shiny::column(
                    width = 6,
                    shiny::selectInput(
                      "dec", 
                      bslib::tooltip(
                        shiny::tags$span("Decimal mark", shiny::icon("circle-question", class = "ms-1 text-muted")),
                        "Character used as decimal point in numbers"
                      ),
                      choices = c(Period = ".", Comma = ","),
                      selected = "."
                    )
                  )
                )
              )
            ),
            
            # Formatting Options Card
            bslib::card(
              class = "mb-3",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::icon("hashtag", class = "me-2"),
                "Display Formatting"
              ),
              bslib::card_body(
                shiny::numericInput(
                  "digits", 
                  bslib::tooltip(
                    shiny::tags$span("Decimal places", shiny::icon("circle-question", class = "ms-1 text-muted")),
                    "Number of decimal places to show in output"
                  ),
                  value = 3, min = 0, max = 10, step = 1
                )
              )
            ),
            
            # Report Generation Card
            bslib::card(
              class = "mb-3",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::icon("file-lines", class = "me-2"),
                "Fit Report Template"
              ),
              bslib::card_body(
                shiny::selectInput(
                  "report_template", 
                  bslib::tooltip(
                    shiny::tags$span("Plot library", shiny::icon("circle-question", class = "ms-1 text-muted")),
                    "HTML summary of model fit to open in browser"
                  ),
                  choices = c(
                    "Interactive (plotly)" = "plotly", 
                    "Static (ggplot2)" = "ggplot2"
                  ),
                  selected = "plotly"
                )
              )
            )
          ), # end left column
          
          # Right column - Prediction Error Metrics Card
          shiny::column(
            width = 6,
            bslib::card(
              class = "mb-3",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::icon("chart-line", class = "me-2"),
                "Prediction Error Metrics"
              ),
              bslib::card_body(
                shiny::tags$div(
                  class = "mb-3",
                  bslib::input_switch(
                    "show_metrics", 
                    shiny::tags$span(
                      "Show metrics on plots",
                      bslib::tooltip(
                        shiny::icon("circle-question", class = "ms-1 text-muted"),
                        "Display error metrics on observed vs. predicted plots"
                      )
                    ),
                    value = TRUE
                  )
                ),
                
                shiny::tags$hr(class = "my-3"),
                
                shiny::selectInput(
                  "bias_method", 
                  bslib::tooltip(
                    shiny::tags$span("Bias method", shiny::icon("circle-question", class = "ms-1 text-muted")),
                    "Method to calculate prediction bias (accuracy)"
                  ),
                  choices = c(
                    "Mean Absolute Error (MAE)" = "mae", 
                    "Mean Weighted Error (MWE)" = "mwe"
                  ),
                  selected = "mwe"
                ),
                
                shiny::selectInput(
                  "imp_method", 
                  bslib::tooltip(
                    shiny::tags$span("Imprecision method", shiny::icon("circle-question", class = "ms-1 text-muted")),
                    "Method to calculate prediction imprecision (scatter)"
                  ),
                  choices = c(
                    "Mean Squared Error (MSE)" = "mse", 
                    "Mean Weighted Squared Error (MWSE)" = "mwse", 
                    "Root Mean Squared Error (RMSE)" = "rmse", 
                    "Mean Bias-Adjusted Squared Error (MBASE)" = "mbase", 
                    "Mean Bias-Adjusted Weighted Squared Error (MBAWSE)" = "mbawse", 
                    "Root Mean Bias-Adjusted Weighted Squared Error (RMBAWSE)" = "rmbawse"
                  ),
                  selected = "rmbawse"
                ),
                
                shiny::tags$div(
                  class = "mb-3",
                  bslib::input_switch(
                    "use_percent", 
                    shiny::tags$span(
                      "Report as percentages",
                      bslib::tooltip(
                        shiny::icon("circle-question", class = "ms-1 text-muted"),
                        "Express error metrics as percentages"
                      )
                    ),
                    value = TRUE
                  )
                ),
                
                shiny::tags$hr(class = "my-3"),
                
                shiny::selectInput(
                  "ic_method", 
                  bslib::tooltip(
                    shiny::tags$span("Information criterion", shiny::icon("circle-question", class = "ms-1 text-muted")),
                    "Method for model comparison"
                  ),
                  choices = c(
                    "Akaike Information Criterion (AIC)" = "aic", 
                    "Bayesian Information Criterion (BIC)" = "bic"
                  ),
                  selected = "aic"
                )
              )
            )
          ) # end right column
        ), # end fluidRow
        
        # Footer with buttons and file location
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bslib::card(
              class = "mt-3",
              bslib::card_body(
                class = "py-3",
                shiny::fluidRow(
                  # Left side: action buttons with unsaved indicator below
                  shiny::column(
                    width = 6,
                    shiny::tags$div(
                      id = "button-container",
                      style = "display: inline-block;",
                      shiny::tags$div(
                        class = "d-flex gap-2",
                        shiny::actionButton(
                          "save", 
                          shiny::tags$span(shiny::icon("floppy-disk", class = "me-1"), "Save"),
                          class = "btn-success"
                        ),
                        shiny::actionButton(
                          "exit", 
                          shiny::tags$span(shiny::icon("xmark", class = "me-1"), "Close"),
                          class = "btn-secondary"
                        )
                      ),
                      shiny::uiOutput("save_status")
                    )
                  ),
                  # Right side: file location
                  shiny::column(
                    width = 6,
                    shiny::tags$div(
                      class = "d-flex gap-2 align-items-center justify-content-end",
                      shiny::tags$small(
                        class = "text-muted me-2",
                        shiny::tags$span(
                          shiny::icon("folder", class = "me-1"),
                          "Options file: ",
                          shiny::textOutput("settings_path", inline = TRUE)
                        )
                      ),
                      shiny::actionButton(
                        "open_file",
                        shiny::tags$span(shiny::icon("external-link-alt", class = "me-1"), "Open"),
                        class = "btn-outline-primary btn-sm"
                      )
                    )
                  )
                )
              )
            )
          )
        ) # end footer fluidRow
      ) # end container div
    ),
    
    # --- Server ---
    server = function(input, output, session) {
      
      # Track if there are unsaved changes
      unsaved_changes <- shiny::reactiveVal(FALSE)
      
      # Load settings from external file
      settings <- tryCatch({
        jsonlite::fromJSON(PMoptionsUserFile)
      }, error = function(e) NULL)
      
      # Apply saved settings to inputs
      if (!is.null(settings)) {
        # Select inputs
        if (!is.null(settings$sep)) shiny::updateSelectInput(session, "sep", selected = settings$sep)
        if (!is.null(settings$dec)) shiny::updateSelectInput(session, "dec", selected = settings$dec)
        if (!is.null(settings$digits)) shiny::updateNumericInput(session, "digits", value = settings$digits)
        if (!is.null(settings$report_template)) shiny::updateSelectInput(session, "report_template", selected = settings$report_template)
        if (!is.null(settings$ic_method)) shiny::updateSelectInput(session, "ic_method", selected = settings$ic_method)
        
        # Bias/imprecision methods - strip percent_ prefix for display
        if (!is.null(settings$bias_method)) {
          shiny::updateSelectInput(session, "bias_method", selected = stringr::str_remove(settings$bias_method, "^percent_"))
        }
        if (!is.null(settings$imp_method)) {
          shiny::updateSelectInput(session, "imp_method", selected = stringr::str_remove(settings$imp_method, "^percent_"))
        }
        
        # Switch inputs - bslib::update_switch uses 'id' not 'inputId'
        if (!is.null(settings$show_metrics)) bslib::update_switch(id = "show_metrics", value = settings$show_metrics, session = session)
        if (!is.null(settings$use_percent)) {
          # Determine use_percent from the bias_method prefix
          use_pct <- grepl("^percent_", settings$bias_method)
          bslib::update_switch(id = "use_percent", value = use_pct, session = session)
        }
      }
      
      # Flag to track if initial load is complete
      # We use a timer to wait for the async update cycle to complete:
      # 1) Server sends update messages to client
      # 2) Client updates inputs and sends new values back
      # 3) Server receives the updated values
      # This round-trip needs time to complete before we start tracking changes
      initialized <- shiny::reactiveVal(FALSE)
      init_timer <- shiny::reactiveTimer(1000, session)
      
      shiny::observeEvent(init_timer(), {
        initialized(TRUE)
      }, once = TRUE, ignoreInit = TRUE)
      
      # Mark changes when any input changes (only after initialization)
      shiny::observe({
        # Only mark changes after initial load is complete
        if (initialized()) {
          unsaved_changes(TRUE)
        }
      }) |> shiny::bindEvent(
        input$sep, input$dec, input$digits, input$show_metrics,
        input$bias_method, input$imp_method, input$use_percent,
        input$ic_method, input$report_template,
        ignoreInit = TRUE
      )
      
      # Display path to user settings file (truncated for display)
      output$settings_path <- shiny::renderText({
        # Truncate path for display if too long
        path <- PMoptionsUserFile
        if (nchar(path) > 50) {
          path <- paste0("...", substr(path, nchar(path) - 47, nchar(path)))
        }
        path
      })
      
      # Show save status indicator below the buttons (full width of button container)
      output$save_status <- shiny::renderUI({
        if (unsaved_changes()) {
          shiny::tags$div(
            class = "mt-2 badge bg-warning text-dark d-flex align-items-center justify-content-center py-2",
            style = "width: 100%;",
            shiny::icon("exclamation-triangle", class = "me-1"),
            "Unsaved changes"
          )
        } else {
          NULL
        }
      })
      
      ### Action button handlers
      
      # Save updated settings
      shiny::observeEvent(input$save, {
        settings <- list(
          sep = input$sep, 
          dec = input$dec, 
          digits = input$digits, 
          show_metrics = input$show_metrics,
          bias_method = glue::glue(c("", "percent_")[1 + as.numeric(input$use_percent)], input$bias_method), 
          imp_method = glue::glue(c("", "percent_")[1 + as.numeric(input$use_percent)], input$imp_method),
          ic_method = input$ic_method,
          report_template = input$report_template
          # backend = input$backend, 
          # model_template_path = input$model_template_path
        )
        
        tryCatch({
          jsonlite::write_json(settings, PMoptionsUserFile, pretty = TRUE, auto_unbox = TRUE)
          unsaved_changes(FALSE)
          shiny::showNotification(
            shiny::tags$span(shiny::icon("check", class = "me-1"), "Settings saved successfully!"),
            type = "message", 
            duration = 3
          )
        }, error = function(e) {
          shiny::showNotification(
            shiny::tags$span(shiny::icon("times-circle", class = "me-1"), paste("Error saving:", e$message)),
            type = "error", 
            duration = 5
          )
        })
      })
      
      # # Reset model template path to default
      # shiny::observeEvent(input$reset_model_template, {
      #   shiny::updateTextAreaInput(
      #     session,
      #     inputId = "model_template_path",
      #     value   = system.file(package = "Pmetrics")
      #   )
      # })
      
      # Exit the app with confirmation if unsaved changes
      shiny::observeEvent(input$exit, {
        if (unsaved_changes()) {
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$span(shiny::icon("exclamation-triangle", class = "me-2 text-warning"), "Unsaved Changes"),
            "You have unsaved changes. Are you sure you want to exit?",
            footer = shiny::tagList(
              shiny::actionButton("confirm_exit", "Exit without saving", class = "btn-danger"),
              shiny::modalButton("Cancel")
            ),
            easyClose = TRUE
          ))
        } else {
          shiny::stopApp()
        }
      })
      
      # Confirm exit without saving
      shiny::observeEvent(input$confirm_exit, {
        shiny::removeModal()
        shiny::stopApp()
      })
      
      # Open the options file in the default application (cross-platform)
      shiny::observeEvent(input$open_file, {
        os <- getOS()
        if (os == 1) {
          # macOS
          system2("open", PMoptionsUserFile)
        } else if (os == 2) {
          # Windows - use shell command
          system2("cmd", c("/c", "start", "", shQuote(PMoptionsUserFile)))
        } else if (os == 3) {
          # Linux
          system2("xdg-open", PMoptionsUserFile)
        } else {
          shiny::showNotification(
            "Unable to open file on this operating system.",
            type = "warning",
            duration = 3
          )
        }
      })
    } #end server
  ) #end shinyApp
    
    
  # Launch the app without trying to launch another browser
  if(launch.app){
    shiny::runApp(app, launch.browser = TRUE)
  }
  
  return(invisible(NULL))
    
    
} # end of PM_options function
  
  
  
  
  