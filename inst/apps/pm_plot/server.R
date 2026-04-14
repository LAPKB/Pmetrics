server <- function(input, output, session) {
  get_object <- function(name) {
    if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
      return(get(name, envir = .GlobalEnv, inherits = FALSE))
    }

    if ("package:Pmetrics" %in% search()) {
      pkg_env <- as.environment("package:Pmetrics")
      if (exists(name, envir = pkg_env, inherits = FALSE)) {
        return(get(name, envir = pkg_env, inherits = FALSE))
      }
    }

    stop(sprintf("Object '%s' not found in .GlobalEnv or package:Pmetrics.", name), call. = FALSE)
  }

  supported_plot_classes <- reactive({
    method_names <- methods("plot")
    method_names <- method_names[grepl("^plot\\.PM_", method_names)]
    sub("^plot\\.", "", method_names)
  })

  plottable_object_names <- reactive({
    global_names <- ls(.GlobalEnv)
    pkg_names <- if ("package:Pmetrics" %in% search()) {
      ls(as.environment("package:Pmetrics"))
    } else {
      character(0)
    }

    object_names <- unique(c(global_names, pkg_names))
    if (length(object_names) == 0) {
      return(character(0))
    }

    is_plottable <- vapply(object_names, function(name) {
      obj <- tryCatch(get_object(name), error = function(e) NULL)
      if (is.null(obj)) {
        return(FALSE)
      }
      obj_classes <- class(obj)

      any(vapply(obj_classes, function(cls) {
        !is.null(utils::getS3method("plot", cls, optional = TRUE)) &&
          (startsWith(cls, "PM_") || identical(cls, "PMvalid"))
      }, logical(1)))
    }, logical(1))

    object_names[is_plottable]
  })

  observe({
    choices <- plottable_object_names()
    selected <- input$data

    if (length(choices) == 0) {
      shiny::updateSelectInput(session, "data", choices = c("No plottable PM objects found" = ""), selected = "")
      return()
    }

    new_selected <- if (!is.null(selected) && nzchar(selected) && selected %in% choices) {
      selected
    } else {
      choices[[1]]
    }

    shiny::updateSelectInput(session, "data", choices = choices, selected = new_selected)
  })

  selected_object <- reactive({
    shiny::req(input$data)
    shiny::req(nzchar(input$data))
    get_object(input$data)
  })

  selected_plot_class <- reactive({
    obj_classes <- class(selected_object())
    plotted_class <- obj_classes[vapply(obj_classes, function(cls) {
      !is.null(utils::getS3method("plot", cls, optional = TRUE))
    }, logical(1))]

    if (length(plotted_class) == 0) {
      return(NA_character_)
    }

    plotted_class[[1]]
  })

  selected_plot_formals <- reactive({
    cls <- selected_plot_class()
    if (is.na(cls)) {
      return(character(0))
    }

    fun <- utils::getS3method("plot", cls, optional = TRUE)
    if (is.null(fun)) {
      return(character(0))
    }

    arg_names <- names(formals(fun))
    setdiff(arg_names, c("x", "..."))
  })

  parsed_plot_args <- reactive({
    txt <- trimws(if (is.null(input$plot_args)) "" else input$plot_args)
    if (!nzchar(txt)) {
      return(list())
    }

    parsed <- tryCatch(
      eval(parse(text = paste0("list(", txt, ")"), keep.source = FALSE), envir = .GlobalEnv),
      error = function(e) e
    )

    if (inherits(parsed, "error")) {
      stop(paste("Argument parse error:", parsed$message), call. = FALSE)
    }

    parsed
  })

  output$method_info <- shiny::renderUI({
    choices <- plottable_object_names()
    if (length(choices) == 0 || is.null(input$data) || !nzchar(input$data)) {
      return(shiny::helpText("Load PM objects in .GlobalEnv or use packaged Pmetrics data objects."))
    }

    cls <- selected_plot_class()
    args <- selected_plot_formals()

    shiny::tagList(
      shiny::helpText(paste("Detected method:", ifelse(is.na(cls), "none", paste0("plot.", cls)))),
      shiny::helpText(paste("Supported PM methods:", paste(supported_plot_classes(), collapse = ", "))),
      shiny::helpText(
        if (length(args) > 0) {
          paste("Common arguments:", paste(args, collapse = ", "))
        } else {
          "No additional named arguments detected for this method."
        }
      )
    )
  })

  plot_trigger <- reactive({
    if (isTRUE(input$auto_refresh)) {
      list(input$data, input$plot_args)
    } else {
      input$plot_now
    }
  })

  output$PMplot <- shiny::renderPlot({
    plot_trigger()
    shiny::req(input$data)
    shiny::req(nzchar(input$data))

    x <- selected_object()
    args <- parsed_plot_args()

    result <- tryCatch({
      do.call(graphics::plot, c(list(x = x), args))
    }, error = function(e) {
      stop(e$message)
    })

    invisible(result)
  })

  output$plotCode <- shiny::renderText({
    shiny::req(input$data)
    shiny::req(nzchar(input$data))

    args_text <- trimws(if (is.null(input$plot_args)) "" else input$plot_args)
    if (nzchar(args_text)) {
      paste0("plot(", input$data, ", ", args_text, ")")
    } else {
      paste0("plot(", input$data, ")")
    }
  })
}

server
