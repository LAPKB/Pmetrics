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
      theme = bslib::bs_theme(bootswatch = "flatly"),
      title = "Pmetrics Options",
      
      tags$details(
        tags$summary("\u1F4C1 Data File Reading"),
        selectInput("sep", "Field separator",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
        selected = ","),
        
        selectInput("dec", "Decimal mark",
        choices = c(Period = ".", Comma = ","),
        selected = ".")
      ),
      # Formatting options
      tags$details(
        tags$summary("\u1F4CF Formatting Options"),
        numericInput("digits", "Number of digits to display",
        value = 3, min = 0, max = 10, step = 1)
      ),
      
      
      #C ompile  options
      tags$details(
        tags$summary("\u2699\uFE0F Compile Options"),
        markdown("Default Rust model template path is in Pmetrics package installation folder. Change if you have write permission errors."),
        tags$div(
          style = "display: flex; align-items: flex-start; gap: 8px;",
          textAreaInput("model_template_path", NULL, value = system.file(package = "Pmetrics"), autoresize = TRUE),
          actionButton("reset_model_template", "Reset to default", class = "btn-secondary")
        ),
        conditionalPanel(
          condition = "input.show == false",selectInput("backend", "Default backend",
          choices = c("Rust" = "rust"),
          selected = "rust"),
          markdown("*Rust is the only backend currently supported by Pmetrics.*")
        )
      ),
      
      tags$details(
        tags$summary("\u1F4CA Prediction Error Metrics"),
        br(),
        checkboxInput("show_metrics", "Display error metrics on obs-pred plots with linear regression", TRUE),
        selectInput("bias_method", "Bias Method",
        choices = c(
          "Mean absolute error (MAE)" = "mae", 
          "Mean weighted error (MWE)" = "mwe"
        ),
        selected = "mwe"),
        
        selectInput("imp_method", "Imprecision Method",
        choices = c(
          
          "Mean squared error (MSE)" = "mse", 
          "Mean weighted squared error (MWSE)" = "mwse", 
          "Root mean squared error (RMSE)" = "rmse", 
          "Mean, bias-adjusted, squared error (MBASE)" = "mbase", 
          "Mean, bias-adjusted, weighted, squared error (MBAWSE)" = "mbawse", 
          "Root mean, bias-adjusted, weighted, squared error (RMBAWSE)" = "rmbawse"
        ),
        selected = "rmbawse"),
        
        checkboxInput("use_percent", "Use percent for error metrics", value = TRUE),
        
        selectInput("ic_method", "Information Criterion Method",
        choices = c(
          "Akaike Information Criterion (AIC)" = "aic", 
          "Bayesian Information Criterion (BIC)" = "bic"
        ),
        selected = "aic")
        
      ),
      
      tags$details(
        tags$summary("\u1F4DD Report Generation"),
        selectInput("report_template", "Default report template", 
        choices = c("plotly", "ggplot2"),
        selected = "plotly")
      ),
      br(),
      div(
        class = "d-flex gap-2",
        actionButton("save", "Save"),
        actionButton("exit", "Exit"),
      ),
      
      br(),
      br(),
      shiny::verbatimTextOutput("settings_location"),
      br(),
      
      actionButton("open_file", "Open Options File", 
      icon = icon("folder-open"), class = "btn-primary")
    ),
    
    # --- Server ---
    server = function(input, output, session) {
      
      # Load settings from external file
      settings <- tryCatch({
        jsonlite::fromJSON(PMoptionsUserFile)
      }, error = function(e) NULL)
      
      # update this list every time a new option is added
      input_types <- list(
        sep = updateSelectInput,
        dec = updateSelectInput,
        show_metrics = updateCheckboxInput,
        digits = updateNumericInput,
        bias_method = updateSelectInput,
        imp_method = updateSelectInput,
        use_percent = updateCheckboxInput,
        ic_method = updateSelectInput,
        report_template = updateSelectInput,
        backend = updateSelectInput,
        model_template_path = updateTextAreaInput
      )
      
      
      # Apply updates
      purrr::imap(settings, function(val, name) {
        updater <- input_types[[name]]
        arg_name <- input_types[[name]] %>% formals() %>% names() %>% keep(~ .x %in% c("value", "selected"))
        
        if (!is.null(updater) && !is.null(arg_name)) {
          args <- list(session = session, inputId = name)
          args[[arg_name]] <- val %>% stringr::str_remove("^percent_")  # remove 'percent_' prefix if present
          do.call(updater, args)
        } 
      })
      
      # Display path to user settings file
      output$settings_location <- renderText({
        glue::glue("Options file path:\n{PMoptionsUserFile}")
      })
      
      
      ### Action button handlers
      
      # Save updated settings
      observeEvent(input$save, {
        settings <- list(sep = input$sep, dec = input$dec, digits = input$digits, show_metrics = input$show_metrics,
          bias_method = glue::glue(c("","percent_")[1+as.numeric(input$use_percent)], input$bias_method), 
          imp_method = glue::glue(c("","percent_")[1+as.numeric(input$use_percent)], input$imp_method),
          ic_method = input$ic_method,
          report_template = input$report_template, backend = input$backend, 
          model_template_path = input$model_template_path)
          
          save_status <- tryCatch(jsonlite::write_json(settings, PMoptionsUserFile, pretty = TRUE, auto_unbox = TRUE),
          error = function(e) {
            shiny::showNotification(
              paste("Error saving settings:", e$message),
              type = "error", duration = 5
            )
            return(FALSE)
          })
          shiny::showNotification(
            "Settings saved", type = "message", duration = 3
          )
        })
        
        # Reset model template path to default
        observeEvent(input$reset_model_template, {
          updateTextAreaInput(
            session,
            inputId = "model_template_path",
            value   = system.file(package = "Pmetrics")
          )
        })
        
        
        # Exit the app
        observeEvent(input$exit, {
          if (file.access(input$model_template_path, 0) == 0 & file.access(input$model_template_path, 2) == 0){
            shiny::stopApp()
          } else {
            shiny::showModal(shiny::modalDialog(
              title = "Permission Error",
              "The specified model template path is not writable. Please choose a different path with write permissions before exiting.",
              easyClose = TRUE,
              footer = NULL
            ))
          }
        })
        
        # Open the options file in the default application
        observeEvent(input$open_file, {
          system(glue::glue("open {PMoptionsUserFile}"))
        })
      } #end server
    ) #end shinyApp
    
    
    # Launch the app without trying to launch another browser
    if(launch.app){
      shiny::runApp(app, launch.browser = TRUE)
    }
    
    return(invisible(NULL))
    
    
  } # end of PM_options function
  
  
  
  
  