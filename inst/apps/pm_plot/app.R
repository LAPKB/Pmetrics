# Load required packages
library(shiny)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(DT)
library(MASS)
library(Matrix)
library(clipr) # For copying to clipboard
library(tcltk) # For the system save dialog


get_obj <- function(name) {
  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    return(get(name, envir = .GlobalEnv, inherits = FALSE))
  }
  if ("package:Pmetrics" %in% search()) {
    pkg_env <- as.environment("package:Pmetrics")
    if (exists(name, envir = pkg_env, inherits = FALSE)) {
      return(get(name, envir = pkg_env, inherits = FALSE))
    }
  }
  NULL
}

ClassFilter <- function(name) {
  obj <- get_obj(name)
  if (is.null(obj)) {
    return(FALSE)
  }
  any(grepl("^PM_", class(obj)))
}

choices_global <- Filter(ClassFilter, ls(globalenv()))
choices_pkg <- if ("package:Pmetrics" %in% search()) {
  Filter(ClassFilter, ls("package:Pmetrics"))
} else {
  character(0)
}

choices <- unique(c(choices_global, choices_pkg))



ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(bootswatch = "zephyr"),
  title = "Pmetrics Plot",
  sidebar = bslib::sidebar(
    width = 400,
    bslib::accordion(
      multiple = FALSE,
      open = "Data",
      bslib::accordion_panel(
        "Data",
        selectInput("data", "Choose a Pmetrics object to plot:", choices = choices),
        uiOutput("DataControls")
      ),
      uiOutput("FormattingPanel"),
      uiOutput("AxesPanel")
    ) # end accordion Panel
  ), # end sidebarPanel
  
  
  h3("Copy and paste the code below into your R script to reproduce the plot:"),
  shiny::helpText(
    "Note: If you accepted the default value for an argument,",
    "it is not necessary to include that argument in the call to plot",
    "and it has been omitted here, following standard R practice."
  ),
  shiny::helpText(htmlOutput("help")),
  bslib::card(textOutput("plotCode"),
  max_height = "100px"
),
shiny::fluidRow(
  shiny::column(
    width = 6,
    shiny::actionButton("copy_exit_btn", "Copy and Exit", class = "btn-success")
  ),
  shiny::column(
    width = 6,
    shiny::actionButton("exit_btn", "Exit", class = "btn-danger")
  )
),
bslib::card(uiOutput("plotPM")),
tags$style(
  type = "text/css",
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
)
)


server <- function(input, output, session) {
      #########  HELPER FUNCTIONS #################

      getXlim <- function() {
        if (length(input$xmin) > 0 & length(input$xmax) > 0) {
          if (input$xmin != "" & input$xmax != "") {
            xlim <- as.numeric(c(input$xmin, input$xmax))
          } else {
            xlim <- NULL
          }
        } else {
          xlim <- NULL
        }
        return(xlim)
      }

      getYlim <- function() {
        if (length(input$ymin) > 0 & length(input$ymax) > 0) {
          if (input$ymin != "" & input$ymax != "") {
            ylim <- as.numeric(c(input$ymin, input$ymax))
          } else {
            ylim <- NULL
          }
        } else {
          ylim <- NULL
        }
        return(ylim)
      }

      current_data_obj <- function() {
        if (is.null(input$data) || length(input$data) == 0 || !nzchar(input$data)) {
          return(NULL)
        }
        get_obj(input$data)
      }

      getXlab <- function() {
        obj <- current_data_obj()
        if (length(input$xlab) == 0 & !is.null(obj) & inherits(obj, "PMmatrix")) {
          return("Time (h)")
        }
        if (length(input$xlab) == 0) {
          return(NULL)
        }
        if (input$xlab == "") {
          return(NULL)
        }
        return(input$xlab)
      }

      getYlab <- function() {
        obj <- current_data_obj()
        if (length(input$ylab) == 0 & !is.null(obj) & inherits(obj, "PMmatrix")) {
          return("Observation")
        }
        if (length(input$ylab) == 0) {
          return(NULL)
        }
        if (input$ylab == "") {
          return(NULL)
        }
        return(input$ylab)
      }

      getFormula <- function(x = "x", y = "y", charac = FALSE, choices) {
        if (length(input[[x]]) == 0 || length(input[[y]]) == 0) {
          return(NULL)
        }
        if (input[[x]] == "Select" || input[[y]] == "Select") {
          return(NULL)
        }
        if (!input[[x]] %in% choices) {
          return(NULL)
        }
        if (!input[[y]] %in% choices) {
          return(NULL)
        }
        if (charac) {
          return(paste(input[[y]], input[[x]], sep = "~"))
        } else {
          return(as.formula(paste(input[[y]], input[[x]], sep = "~")))
        }
      }

      getProbs <- function() {
        if (length(input$probs) == 0) {
          return(c(0.05, 0.25, 0.5, 0.75, 0.95))
        } else {
          return(as.numeric(input$probs))
        }
      }

      getPred <- function(icen) {
        if (length(input$incl_pred) == 0) {
          return(NULL)
        }
        if (input$incl_pred == "none") {
          return(NULL)
        }
        if (icen == "post") {
          return(paste0(input$data, "$post"))
        }
        if (icen == "pop") {
          return(paste0(input$data, "$pop"))
        }
      }

      getGroup <- function(group) {
        x <- current_data_obj()
        if (is.null(x)) {
          return(NULL)
        }
        if (length(group) == 0) {
          return(NULL)
        }
        colfac <- which(names(x) == group)
        if (length(colfac) > 0) {
          return(x[, colfac])
        }
        return(NULL)
      }

      setVal <- function(par, def) {
        if (is.null(input[[par]])) {
          return(def)
        } else {
          return(input[[par]])
        }
      }

      plotly_marker_symbols <- c(
        "circle", "circle-open", "square", "square-open", "diamond", "diamond-open",
        "cross", "x", "triangle-up", "triangle-up-open", "triangle-down", "triangle-down-open",
        "triangle-left", "triangle-left-open", "triangle-right", "triangle-right-open",
        "pentagon", "pentagon-open", "hexagon", "hexagon-open", "hexagon2", "hexagon2-open",
        "octagon", "octagon-open", "star", "star-open", "hexagram", "hexagram-open",
        "star-triangle-up", "star-triangle-up-open", "star-triangle-down", "star-triangle-down-open",
        "star-square", "star-square-open", "star-diamond", "star-diamond-open",
        "diamond-tall", "diamond-tall-open", "diamond-wide", "diamond-wide-open",
        "hourglass", "hourglass-open", "bowtie", "bowtie-open",
        "circle-cross", "circle-cross-open", "circle-x", "circle-x-open",
        "square-cross", "square-cross-open", "square-x", "square-x-open",
        "diamond-cross", "diamond-cross-open", "diamond-x", "diamond-x-open",
        "cross-thin", "x-thin", "asterisk", "hash",
        "y-up", "y-up-open", "y-down", "y-down-open", "y-left", "y-left-open", "y-right", "y-right-open",
        "line-ew", "line-ew-open", "line-ns", "line-ns-open", "line-ne", "line-ne-open", "line-nw", "line-nw-open"
      )

      marker_controls <- function(default_color = "red", default_symbol = "circle", include_symbol = TRUE) {
        controls <- list(
          radioButtons(
            "mrk_col_mode",
            "Color source:",
            choices = c("Manual list" = "manual", "Brewer palette" = "palette"),
            selected = "manual",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.mrk_col_mode == 'manual'",
            shiny::helpText("Enter color names separated by commas. Values are recycled as needed."),
            textInput("mrk_col", "Colors:", default_color)
          ),
          conditionalPanel(
            condition = "input.mrk_col_mode == 'palette'",
            selectInput("mrk_palette", "Palette:",
              choices = rownames(RColorBrewer::brewer.pal.info),
              selected = "Set1"
            )
          )
        )

        if (include_symbol) {
          controls <- c(controls, list(
            selectInput(
              "mrk_symbol",
              "Symbol(s):",
              choices = plotly_marker_symbols,
              selected = default_symbol,
              multiple = TRUE
            )
          ))
        }

        controls
      }

      parse_marker_color <- function(default_color) {
        mrk_col_mode <- setVal("mrk_col_mode", "manual")
        mrk_col <- setVal("mrk_col", default_color)
        if (identical(mrk_col_mode, "palette")) {
          return(setVal("mrk_palette", "Set1"))
        }

        mrk_col <- unlist(stringr::str_split(mrk_col, "\\s*,\\s*"))
        mrk_col <- stringr::str_trim(mrk_col)
        mrk_col <- mrk_col[nzchar(mrk_col)]
        if (length(mrk_col) == 0) {
          mrk_col <- default_color
        }
        mrk_col
      }

      parse_marker_symbol <- function(default_symbol = "circle") {
        mrk_symbol <- setVal("mrk_symbol", default_symbol)
        if (length(mrk_symbol) == 0) {
          return(default_symbol)
        }
        mrk_symbol
      }

      parse_marker_color_scalar <- function(default_color) {
        mrk_col <- parse_marker_color(default_color)
        if (length(mrk_col) > 1) {
          mrk_col <- mrk_col[[1]]
        }
        mrk_col
      }

      parse_marker_symbol_scalar <- function(default_symbol = "circle") {
        mrk_symbol <- parse_marker_symbol(default_symbol)
        if (length(mrk_symbol) > 1) {
          mrk_symbol <- mrk_symbol[[1]]
        }
        mrk_symbol
      }

      do_plot <- function(args) {
        x_obj <- args$x
        x_classes <- class(x_obj)
        plot_method <- NULL

        for (cls in x_classes) {
          plot_method <- utils::getS3method("plot", cls, optional = TRUE)
          if (!is.null(plot_method)) {
            break
          }
        }

        if (!is.null(plot_method)) {
          method_formals <- names(formals(plot_method))
          if ("print" %in% method_formals) {
            args$print <- FALSE
          }
        }

        do.call(plot, args)
      }


      ############### Make Data Controls #####################

      # can use this for PM_result$data and for PM_data
      recycleDataControl <- function(src) {
        if (src == "PM_result") {
          data_obj <- get(input$data)$data$standard_data
        }

        if (src == "PM_data") {
          data_obj <- get(input$data)$standard_data
        }

        return(list(
          shiny::helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
          radioButtons("data_include", "", c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
          selectInput("data_select", "",
            choices = unique(data_obj$id),
            selected = unique(data_obj$id),
            multiple = TRUE, selectize = FALSE
          ),
          checkboxGroupInput(
            "data_outeq",
            "Output equations:",
            choices = as.character(1:max(data_obj$outeq, na.rm = TRUE)),
            selected = "1",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.data_outeq && input.data_outeq.length > 1",
            shiny::helpText("One name per output, separate by commas."),
            textInput("data_out_names", "Output Names:")
          ),
          numericInput("data_block", "Block number:", 1, min = 1, step = 1),
          selectInput("data_group", "Grouping factor", choices = c("None" = "none", getCov(data_obj)$covnames)), # in PMutilities
          conditionalPanel(
            condition = "input.data_group !== 'none'",
            shiny::helpText("One name per group, separate by commas."),
            textInput("data_group_names", "Group Names:")
          )

        ))
      }


      makeDataControls <- function() {
        obj <- current_data_obj()
        if (is.null(obj)) {
          return(NULL)
        }

        ############### Data: PM_data #####################
        if (inherits(obj, "PM_data")) {
          return(list(
            recycleDataControl("PM_data")
          ))
        }



        ############### Data: PM_result #####################

        if (inherits(obj, "PM_result")) {
          res_sub_choices <- c(
            "Data" = "dat",
            "Model" = "mod",
            "Obs/Pred" = "op",
            "Population" = "pop",
            "Posterior" = "post",
            "Final" = "fin",
            "Cycle" = "cyc",
            "Covariate" = "cov"
          )
          if (!is.null(obj$valid)) {
            res_sub_choices <- c(res_sub_choices, "Validation" = "valid")
          }

          return(list(
            radioButtons("res_sub", "Plot which field?",
              selected = "dat",
              res_sub_choices
            ),

            ############### Data: PM_result$data #####################

            conditionalPanel(
              condition = "input.res_sub == 'dat'",
              recycleDataControl("PM_result") # end data conditional panel
            ),

            ############### Data: PM_result$op #####################

            conditionalPanel(
              condition = "input.res_sub == 'op'",
              radioButtons("pred.type", "", c("Posterior Predictions" = "post", "Population Predictions" = "pop"), selected = "post"),
              checkboxInput("resid", "Residual Plot", FALSE),
              selectInput("op_icen", "Predictions based on:", choices = c("mean", "median"), "median"),
              shiny::helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("op_include", "", c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("op_select", "",
                choices = unique(get(input$data)$op$id), selected = unique(get(input$data)$op$id),
                multiple = TRUE, selectize = FALSE
              ),
              checkboxGroupInput(
                "outeq",
                "Output equations:",
                choices = as.character(1:max(get(input$data)$op$outeq)),
                selected = "1",
                inline = TRUE
              ),
              shiny::helpText("Only the first selected output equation is used for plotting."),
              selectInput("block", "Block:", choices = c("All", 1:max(get(input$data)$op$block)))
            ), # end op conditional panel

            ############### Data: PM_result$final #####################

            conditionalPanel(
              condition = "input.res_sub == 'fin'",
              radioButtons("ptype", "Plot type:", c("Univariate" = "uni", "Bivariate" = "bi")),
              conditionalPanel(
                condition = "input.ptype=='bi'",
                selectInput("x", "x-axis", choices = c("Select", names(get(input$data)$final$popMean)), selected = "Select"),
                selectInput("y", "y-axis", choices = c("Select", names(get(input$data)$final$popMean)), selected = "Select")
              ) # end conditional panel
            ), # end final conditional panel

            ############### Data: PM_result$cycle #####################

            conditionalPanel(
              condition = "input.res_sub == 'cyc'",
              numericInput("omit", "Proportion of burn-in cycles to omit:", 0.2, min = 0, max = 1, step = 0.1)
            ), # end cycle conditional panel

            ############### Data: PM_result$cov #####################

            conditionalPanel(
              condition = "input.res_sub == 'cov'",
              selectInput("covY", "Y-axis",
                choices = c("Select", names(get(input$data)$cov$data)[names(get(input$data)$cov$data) != "icen"]),
                selected = "Select"
              ),
              selectInput("covX", "X-axis",
                choices = c("Select", names(get(input$data)$cov$data)[names(get(input$data)$cov$data) != "icen"]),
                selected = "Select"
              ),
              shiny::helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("cov_include", "", c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("cov_select", "",
                choices = unique(get(input$data)$cov$data$id),
                selected = unique(get(input$data)$cov$data$id), multiple = T, selectize = FALSE
              ),
              selectInput("icen", "Summary method for changing covariates:",
                choices = c("mean", "median", "mode", "none"), "mean"
              )
            ), # end cov conditional panel

            ############### Data: PM_result$pop #####################

            conditionalPanel(
              condition = "input.res_sub == 'pop'",
              selectInput(
                "pop_icen", "Predictions based on:",
                choices = c("median", "mean"), selected = "median"
              ),
              checkboxGroupInput(
                "pop_outeq",
                "Output equations:",
                choices = as.character(1:max(get(input$data)$pop$data$outeq)),
                selected = "1",
                inline = TRUE
              ),
              selectInput("pop_block", "Block:",
                choices = c("All", sort(unique(get(input$data)$pop$data$block)))
              ),
              shiny::helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("pop_include", "", c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("pop_select", "",
                choices = unique(get(input$data)$pop$data$id),
                selected = unique(get(input$data)$pop$data$id),
                multiple = TRUE, selectize = FALSE
              )
            ), # end pop conditional panel

            ############### Data: PM_result$post #####################

            conditionalPanel(
              condition = "input.res_sub == 'post'",
              selectInput(
                "post_icen", "Predictions based on:",
                choices = c("median", "mean"), selected = "median"
              ),
              checkboxGroupInput(
                "post_outeq",
                "Output equations:",
                choices = as.character(1:max(get(input$data)$post$data$outeq)),
                selected = "1",
                inline = TRUE
              ),
              selectInput("post_block", "Block:",
                choices = c("All", sort(unique(get(input$data)$post$data$block)))
              ),
              shiny::helpText("Use Shift or CTRL (Windows) or CMD (Mac) + Click to (de)select subjects."),
              radioButtons("post_include", "", c("Include subjects" = "yes", "Exclude subjects" = "no"), selected = "yes"),
              selectInput("post_select", "",
                choices = unique(get(input$data)$post$data$id),
                selected = unique(get(input$data)$post$data$id),
                multiple = TRUE, selectize = FALSE
              )
            ) # end post conditional panel

            ############### Data: PM_result$model #####################

            # not needed
          )) # end return list
        } # end PM_result

        ############### Data: PM_sim #####################

        if (inherits(obj, "PM_sim")) {
          x_data <- obj$data

          extract_outeq <- function(sim) {
            if (is.data.frame(sim) && "outeq" %in% names(sim)) {
              return(sim[["outeq"]])
            }
            if (is.list(sim)) {
              sim_obs <- sim[["obs"]]
              if (!is.null(sim_obs) && is.data.frame(sim_obs) && "outeq" %in% names(sim_obs)) {
                return(sim_obs[["outeq"]])
              }
            }
            numeric(0)
          }

          outeq_vals <- unlist(lapply(x_data, extract_outeq), use.names = FALSE)
          outeq_vals <- suppressWarnings(as.numeric(outeq_vals))
          outeq_vals <- outeq_vals[is.finite(outeq_vals)]
          if (length(outeq_vals) == 0) {
            outeq_choices <- 1
          } else {
            outeq_choices <- seq_len(max(outeq_vals))
          }

          OPFilter <- function(x) any(grepl("^PM_result", class(get(x))))
          OPchoices <- Filter(OPFilter, ls(globalenv()))
          if (length(OPchoices) == 0) {
            OPchoices <- "None"
          } else {
            OPchoices <- c("None", OPchoices)
          }


          return(list(
            checkboxGroupInput(
              "outeq",
              "Output equations:",
              choices = as.character(outeq_choices),
              selected = "1",
              inline = TRUE
            ),
            shiny::helpText("Only the first selected output equation is used for plotting."),
            selectInput("sim_obs", "Observed (for VPC)", choices = OPchoices)
          )) # end list
        } # end PMsim

        ############### Data: PM_model #####################

        if (inherits(obj, "PM_model")) {
          return(NULL)
        }
      } # end makeDataControls function

      ############### Make Format Controls #####################
      makeFormatControls <- function() {
        obj <- current_data_obj()
        if (is.null(obj)) {
          return(NULL)
        }

        ############### Format: PM_result$cov #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_result") &&
          !is.null(input$res_sub) && input$res_sub == "cov") {
          return(list(
            bslib::accordion(
              bslib::accordion_panel(
                "Line Options",
                bslib::navset_card_tab(
                  bslib::nav_panel(
                    "Linear",
                    checkboxInput("cov_lm", "Include?", FALSE),
                    checkboxInput("def_lm_fmt", "Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_lm_fmt",
                      numericInput("lm_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      textInput("lm_col", "Color:", "dodgerblue"),
                      numericInput("lm_lwd", "Line width", 1, step = 0.5),
                      selectInput("lm_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  ),
                  bslib::nav_panel(
                    "Loess",
                    checkboxInput("cov_loess", "Include?", TRUE),
                    checkboxInput("def_loess_fmt", "Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_loess_fmt",
                      numericInput("loess_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      textInput("loess_col", "Color:", "dodgerblue"),
                      numericInput("loess_lwd", "Line width", 1, step = 0.5),
                      selectInput("loess_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash")
                    )
                  ),
                  bslib::nav_panel(
                    "Reference",
                    checkboxInput("cov_ref", "Include?", FALSE),
                    checkboxInput("def_ref_fmt", "Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_ref_fmt",
                      textInput("ref_col", "Color:", "dodgerblue"),
                      numericInput("ref_lwd", "Line width", 1, step = 0.5),
                      selectInput("ref_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  )
                )
              ), # end accordion panel
              bslib::accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  marker_controls(default_color = "orange", default_symbol = "circle"),
                  numericInput("mrk_size", "Size", 10, step = 1),
                  numericInput("mrk_opacity", "Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd", "Outline width", 1, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                )
              ), # end accordion_panel
              bslib::accordion_panel(
                "Plot Options",
                checkboxInput("log", "Log-log plot", FALSE),
                checkboxInput("grid", "Grid", TRUE),
                shiny::helpText("Only relevant for linear regression"),
                checkboxInput("def_stats_fmt", "Use default statistics formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_stats_fmt",
                  textInput("stats_col", "Color:", "black"),
                  numericInput("stats_size", "Size", 14, step = 1),
                  checkboxInput("stats_bold", "Bold?", FALSE),
                  numericInput("stats_x", "Horizontal pos", 0.8, step = 0.1),
                  numericInput("stats_y", "Vertical pos", 0.1, step = 0.1)
                ),
                checkboxInput("def_title_fmt", "Omit title", TRUE),
                conditionalPanel(
                  condition = "!input.def_title_fmt",
                  textInput("title_text", "Title:", ""),
                  textInput("title_col", "Color:", "black"),
                  numericInput("title_size", "Size", 20, step = 1),
                  checkboxInput("title_bold", "Bold?", TRUE)
                )
              ) # end accordion_panel
            ) # end accordion
          )) # end list
        } # end PMcov

        ############### Format: PM_result$final (NPAG) #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_result") &&
          !is.null(input$res_sub) && input$res_sub == "fin" & inherits(get(input$data)$final, "NPAG")) {
          return(list(
            conditionalPanel(
              condition = "input.ptype == 'uni'",
              bslib::navset_card_tab(
                bslib::nav_panel(
                  "Bars",
                  checkboxInput("def_bar_fmt", "Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_bar_fmt",
                    textInput("bar_col", "Color:", "dodgerblue"),
                    numericInput("bar_width", "Width:", value = 0.02, min = 0, step = 0.01),
                    numericInput("bar_opacity", "Opacity:", value = 0.5, min = 0, max = 1, step = 0.1),
                    textInput("bar_lcol", "Outline Color:", "black"),
                    numericInput("bar_lwd", "Outline Width:", value = 1, min = 0)
                  )
                ),
                bslib::nav_panel(
                  "Density Line",
                  checkboxInput("incl_line", "Include?", FALSE),
                  checkboxInput("def_line_fmt", "Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_line_fmt",
                    textInput("line_col", "Color:", "black"),
                    numericInput("line_width", "Width:", value = 1),
                    selectInput("line_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.ptype == 'bi'",
              bslib::navset_card_tab(
                bslib::nav_panel(
                  "Markers",
                  checkboxInput("def_mrk_fmt", "Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_mrk_fmt",
                    marker_controls(default_color = "dodgerblue", default_symbol = "circle"),
                    numericInput("mrk_size", "Size", 5, step = 1),
                    numericInput("mrk_opacity", "Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                    numericInput("mrk_lwd", "Outline width", 1, step = 0.5),
                    textInput("mrk_lcol", "Outline color:", "black")
                  )
                ),
                bslib::nav_panel(
                  "Drop Lines",
                  checkboxInput("incl_drop", "Include?", TRUE),
                  checkboxInput("def_drop_fmt", "Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_drop_fmt",
                    textInput("drop_col", "Color:", "black"),
                    numericInput("drop_width", "Width:", value = 1),
                    selectInput("drop_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash")
                  )
                )
              )
            ) # end conditional panel
          )) # end list
        } # end PM_final, NPAG

        ############### Format: PM_result$final IT2B #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_result") &&
          !is.null(input$res_sub) && input$res_sub == "fin" & inherits(get(input$data)$final, "IT2B")) {
          return(list(
            conditionalPanel(
              condition = "input.ptype=='uni'",
              checkboxInput("standard", "Standardize IT2B marginals", FALSE),
              numericInput("lwd1", "Line width:", 4),
              textInput("col1", "Color:", "red")
            ),
            conditionalPanel(
              condition = "input.ptype=='bi'",
              checkboxInput("grid", "Grid", TRUE),
              checkboxInput("legend", "Legend", TRUE),
              numericInput("lwd2", "Line width:", 1),
              textInput("col2", "Color:", "white"),
              numericInput("cex", "Point size:", 1, step = 0.1),
              numericInput("pch", "Plotting character:", 3, min = 1, step = 1),
              selectInput("probs", "Quantiles", choices = c(0.01, 0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 0.99), selected = c(0.05, 0.25, 0.5, 0.75, 0.95), multiple = T)
            ) # end conditional panel
          )) # end list
        } # end PM_final, IT2B

        ############### Format: PM_result$op #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_result") &&
          !is.null(input$res_sub) && input$res_sub == "op") {
          return(list(
            bslib::accordion(
              bslib::accordion_panel(
                "Line Options",
                # h4("Line Options:"),
                bslib::navset_card_tab(
                  bslib::nav_panel(
                    "Linear",
                    checkboxInput("op_lm", "Include?", TRUE),
                    checkboxInput("def_lm_fmt", "Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_lm_fmt",
                      numericInput("lm_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      textInput("lm_col", "Color:", "dodgerblue"),
                      numericInput("lm_lwd", "Line width", 1, step = 0.5),
                      selectInput("lm_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  ),
                  bslib::nav_panel(
                    "Loess",
                    checkboxInput("op_loess", "Include?", FALSE),
                    checkboxInput("def_loess_fmt", "Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_loess_fmt",
                      numericInput("loess_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      textInput("loess_col", "Color:", "dodgerblue"),
                      numericInput("loess_lwd", "Line width", 1, step = 0.5),
                      selectInput("loess_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash")
                    )
                  ),
                  bslib::nav_panel(
                    "Reference",
                    checkboxInput("op_ref", "Include?", TRUE),
                    checkboxInput("def_ref_fmt", "Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_ref_fmt",
                      textInput("ref_col", "Color:", "dodgerblue"),
                      numericInput("ref_lwd", "Line width", 1, step = 0.5),
                      selectInput("ref_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  )
                )
              ), # end accordion panel
              bslib::accordion_panel(
                "Marker Options",
                # h4("Marker Options:"),
                checkboxInput("def_marker_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  marker_controls(default_color = "orange", default_symbol = "circle"),
                  numericInput("mrk_size", "Size", 10, step = 1),
                  numericInput("mrk_opacity", "Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd", "Outline width", 1, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                )
              ), # end accordion_panel
              bslib::accordion_panel(
                "Plot Options",
                numericInput("mult", "Multiplication factor for axes:", "1"),
                checkboxInput("log", "Log-log plot", FALSE),
                checkboxInput("grid", "Grid", TRUE),
                shiny::helpText("Only relevant for linear regression"),
                checkboxInput("def_stats_fmt", "Use default statistics formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_stats_fmt",
                  textInput("stats_col", "Color:", "black"),
                  numericInput("stats_size", "Size", 14, step = 1),
                  checkboxInput("stats_bold", "Bold?", FALSE),
                  numericInput("stats_x", "Horizontal pos", 0.8, step = 0.1),
                  numericInput("stats_y", "Vertical pos", 0.1, step = 0.1)
                ),
                checkboxInput("def_title_fmt", "Omit title", TRUE),
                conditionalPanel(
                  condition = "!input.def_title_fmt",
                  textInput("title_text", "Title:", ""),
                  textInput("title_col", "Color:", "black"),
                  numericInput("title_size", "Size", 20, step = 1),
                  checkboxInput("title_bold", "Bold?", TRUE)
                )
              ) # end accordion_panel
            ) # end accordion
          )) # end list
        } # end PM_op

        ############### Format: PM_result$pop #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_result") &&
          !is.null(input$res_sub) && input$res_sub == "pop") {
          return(list(
            bslib::accordion(
              bslib::accordion_panel(
                "Line Options",
                checkboxInput("pop_join", "Include?", TRUE),
                checkboxInput("def_pop_line_fmt", "Use default formatting", TRUE),
                shiny::helpText("Line color matches marker color."),
                conditionalPanel(
                  condition = "!input.def_pop_line_fmt",
                  numericInput("pop_line_lwd", "Line width", 1, step = 0.5),
                  selectInput("pop_line_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                )
              ), # end accordion_panel
              bslib::accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  marker_controls(default_color = "red", default_symbol = "circle"),
                  numericInput("mrk_size", "Size", 10, step = 1),
                  numericInput("mrk_opacity", "Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd", "Outline width", 1, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                )
              ), # end accordion_panel
              bslib::accordion_panel(
                "Plot Options",
                numericInput("mult", "Multiplication factor for axes:", "1"),
                checkboxInput("log", "Semi-log plot", FALSE),
                checkboxInput("grid", "Grid", FALSE),
                checkboxInput("legend", "Legend", FALSE),
                textInput("pop_out_names", "Output names (comma-separated):", ""),
                shiny::helpText("Optional: label each output equation in order, e.g. 'Drug A, Drug B'."),
                checkboxInput("def_title_fmt", "Omit title", TRUE),
                conditionalPanel(
                  condition = "!input.def_title_fmt",
                  textInput("title_text", "Title:", ""),
                  textInput("title_col", "Color:", "black"),
                  numericInput("title_size", "Size", 20, step = 1),
                  checkboxInput("title_bold", "Bold?", TRUE)
                )
              ) # end accordion_panel
            ) # end accordion
          )) # end list
        } # end PM_pop format

        ############### Format: PM_result$post #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_result") &&
          !is.null(input$res_sub) && input$res_sub == "post") {
          return(list(
            bslib::accordion(
              bslib::accordion_panel(
                "Line Options",
                checkboxInput("post_join", "Include?", TRUE),
                checkboxInput("def_post_line_fmt", "Use default formatting", TRUE),
                shiny::helpText("Line color matches marker color."),
                conditionalPanel(
                  condition = "!input.def_post_line_fmt",
                  numericInput("post_line_lwd", "Line width", 1, step = 0.5),
                  selectInput("post_line_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                )
              ), # end accordion_panel
              bslib::accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  marker_controls(default_color = "red", default_symbol = "circle"),
                  numericInput("mrk_size", "Size", 10, step = 1),
                  numericInput("mrk_opacity", "Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd", "Outline width", 1, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                )
              ), # end accordion_panel
              bslib::accordion_panel(
                "Plot Options",
                numericInput("mult", "Multiplication factor for axes:", "1"),
                checkboxInput("log", "Semi-log plot", FALSE),
                checkboxInput("grid", "Grid", FALSE),
                checkboxInput("legend", "Legend", FALSE),
                textInput("post_out_names", "Output names (comma-separated):", ""),
                shiny::helpText("Optional: label each output equation in order, e.g. 'Drug A, Drug B'."),
                checkboxInput("def_title_fmt", "Omit title", TRUE),
                conditionalPanel(
                  condition = "!input.def_title_fmt",
                  textInput("title_text", "Title:", ""),
                  textInput("title_col", "Color:", "black"),
                  numericInput("title_size", "Size", 20, step = 1),
                  checkboxInput("title_bold", "Bold?", TRUE)
                )
              ) # end accordion_panel
            ) # end accordion
          )) # end list
        } # end PM_post format

        ############### Format: PM_result$cycle #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_result") &&
          !is.null(input$res_sub) && input$res_sub == "cyc") {
          return(list(
            bslib::accordion(
              bslib::accordion_panel(
                "Line Options",
                checkboxInput("def_ab_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_ab_fmt",
                  bslib::navset_card_tab(
                    bslib::nav_panel(
                      "Both Rows",
                      shiny::helpText("Applies to all plots"),
                      numericInput("ab_lwd", "Line width", 1, step = 0.5),
                    ),
                    bslib::nav_panel(
                      "Row A",
                      shiny::helpText("Applies to LL, AIC, gamma/lambda"),
                      textInput("a_col", "Color:", "dodgerblue"),
                      selectInput("a_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    ),
                    bslib::nav_panel(
                      "Row B",
                      shiny::helpText("Applies to normalized parameter statistics"),
                      shiny::helpText("Choose colors for each parameter trace. Select 'Other' for custom."),
                      selectInput("b_col", "Color palette:",
                        choices = c(getPalettes(), "Other"), selected = "Spectral"
                      ),
                      conditionalPanel(
                        condition = "input.b_col == 'Other'",
                        shiny::helpText("Enter color names separated by commas. Values will be recycled as needed."),
                        textInput("b_custom_colors", "Custom Colors:", value = "")
                      ),
                      shiny::helpText("Choose dash styles for each parameter trace. Values will be recycled if needed."),
                      selectInput("b_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash", multiple = TRUE)
                    ) # end nav_panel
                  ) # end navset_card_tab
                ) # end conditional panel
              ), # end accordion panel
              bslib::accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  marker_controls(default_color = "dodgerblue", default_symbol = "circle"),
                  numericInput("mrk_size", "Size", 4, step = 1),
                  numericInput("mrk_opacity", "Opacity", value = 1, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd", "Outline width", 0, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                ),
              ), # end accordion_panel
              bslib::accordion_panel(
                "Plot Options",
                checkboxInput("grid", "Grid", TRUE),
              ) # end accordion_panel
            ) # end accordion
          )) # end list
        } # end PM_cycle

        ############### Format: PM_result$data #####################

        if (!is.null(input$data) &&
          (inherits(get(input$data), "PM_data") |
            (inherits(get(input$data), "PM_result") &&
              !is.null(input$res_sub) && input$res_sub == "dat"))) {
          return(list(
            bslib::accordion(
              bslib::accordion_panel(
                "Line Options",
                # h4("Line Options:"),
                bslib::navset_card_tab(
                  bslib::nav_panel(
                    "Join",
                    checkboxInput("join", "Include?", TRUE),
                    checkboxInput("def_join_fmt", "Use default formatting", TRUE),
                     shiny::helpText("Line color matches marker color."),
                    conditionalPanel(
                      condition = "!input.def_join_fmt",
                      numericInput("join_lwd", "Line width", 1, step = 0.5),
                      selectInput("join_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  ),
                  bslib::nav_panel(
                    "Pred",
                    radioButtons("incl_pred", "Include Predictions?", c("None" = "none", "Population" = "pop", "Posterior" = "post")),
                    checkboxInput("def_pred_fmt", "Use default formatting", TRUE),
                     shiny::helpText("Line color matches marker color."),
                    conditionalPanel(
                      condition = "!input.def_pred_fmt",
                      numericInput("pred_lwd", "Line width", 1, step = 0.5),
                      selectInput("pred_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), selected = "dash")
                    )
                  ) # end nav_panel
                ) # end navset_card_tab
              ), # end accordion panel
              bslib::accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  marker_controls(default_color = "red", default_symbol = "circle"),
                  numericInput("mrk_size", "Size", 10, step = 1),
                  numericInput("mrk_opacity", "Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd", "Outline width", 1, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                ),
              ), # end accordion_panel
              bslib::accordion_panel(
                "Plot Options",
                checkboxInput("tad", "Use time after dose", FALSE),
                numericInput("mult", "Multiplication factor for axes:", value = 1, min = 0, step = 1),
                checkboxInput("log", "Semi-log plot", FALSE),
                checkboxInput("grid", "Grid", FALSE),
                checkboxInput("def_title_fmt", "Omit title", TRUE),
                conditionalPanel(
                  condition = "!input.def_title_fmt",
                  textInput("title_text", "Title:", ""),
                  textInput("title_col", "Color:", "black"),
                  numericInput("title_size", "Size", 20, step = 1),
                  checkboxInput("title_bold", "Bold?", TRUE)
                ),
                conditionalPanel(
                  condition = "input.data_group !== 'none'",
                  checkboxInput("legend", "Legend", value = TRUE)
                )
              ) # end accordion_panel
            ) # end accordion
          )) # end list
        } # end PM_data

        ############### Format: PM_sim #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_sim")) {
          return(
            list(
              bslib::accordion(
                bslib::accordion_panel(
                  "Line Options",
                  bslib::navset_card_tab(
                  bslib::nav_panel(
                    "Line",
                    checkboxInput("sim_line", "Include?", TRUE),
                    checkboxInput("def_sim_line_fmt", "Use default formatting", TRUE),
                    conditionalPanel(
                      condition = "!input.def_sim_line_fmt",
                      numericInput("sim_ci", "Confidence Interval", 0.95, min = 0.1, max = 0.99, step = 0.05),
                      selectInput("sim_probs", "Quantiles",
                        choices = c(NA, 0.01, 0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 0.99),
                        selected = c(0.05, 0.25, 0.5, 0.75, 0.95), multiple = TRUE
                      ),
                      textInput("sim_col", "Color:", "dodgerblue"),
                      numericInput("sim_lwd", "Line width", 1, step = 0.5),
                      selectInput("sim_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                    )
                  )
                  ) # end navset_card_tab
                ), # end accordion panel
                bslib::accordion_panel(
                  "Marker Options",
                  checkboxInput("def_sim_marker_fmt", "Use default formatting", TRUE),
                  conditionalPanel(
                    condition = "!input.def_sim_marker_fmt",
                    marker_controls(default_color = "black", default_symbol = "circle-open"),
                    numericInput("mrk_size", "Size", 8, step = 1),
                    numericInput("mrk_opacity", "Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                    numericInput("mrk_lwd", "Outline width", 1, step = 0.5),
                    textInput("mrk_lcol", "Outline color:", "black")
                  ),
                ), # end accordion_panel
                bslib::accordion_panel(
                  "Plot Options",
                  numericInput("binSize", "Bin Size", value = 0, min = 0, step = 1),
                  numericInput("mult", "Multiplication factor for axes:", value = 1),
                  checkboxInput("log", "Semi-log plot", TRUE),
                  checkboxInput("grid", "Grid", TRUE),
                  checkboxInput("def_title_fmt", "Omit title", TRUE),
                  conditionalPanel(
                    condition = "!input.def_title_fmt",
                    textInput("title_text", "Title:", ""),
                    textInput("title_col", "Color:", "black"),
                    numericInput("title_size", "Size", 20, step = 1),
                    checkboxInput("title_bold", "Bold?", TRUE)
                  )
                )
              ) # end accordion
            )
          ) # end list
        } # end PM_sim

        ############### Format: PM_model #####################

        if (!is.null(input$data) &&
          (inherits(get(input$data), "PM_model") |
            (inherits(get(input$data), "PM_result") &&
              !is.null(input$res_sub) && input$res_sub == "mod"))) {
          return(list(
            bslib::accordion(
              bslib::accordion_panel(
                "Line Options",
                checkboxInput("join", "Include?", TRUE),
                checkboxInput("def_join_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_join_fmt",
                  textInput("join_col", "Color:", "black"),
                  numericInput("join_lwd", "Line width", 1, step = 0.5),
                  selectInput("join_dash", "Dash style:", choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"))
                )
              ), # end accordion panel
              bslib::accordion_panel(
                "Marker Options",
                checkboxInput("def_marker_fmt", "Use default formatting", TRUE),
                conditionalPanel(
                  condition = "!input.def_marker_fmt",
                  marker_controls(default_color = "dodgerblue", default_symbol = "circle"),
                  numericInput("mrk_size", "Size", value = 0.25, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_opacity", "Opacity", value = 0.5, min = 0, max = 1, step = 0.1),
                  numericInput("mrk_lwd", "Outline width", 0.5, step = 0.5),
                  textInput("mrk_lcol", "Outline color:", "black")
                )
              )
            ) # end accordion
          ))
        } # end PM_model
      } # end makeFormatControls function

      ############### Make Axis Controls #####################
      makeAxisControls <- function() {
        obj <- current_data_obj()
        if (is.null(obj)) {
          return(NULL)
        }

        ############### Axis: all but PM_data #####################

        if (!is.null(input$data) && (inherits(get(input$data), "PM_sim") ||
          (inherits(get(input$data), "PM_result") && input$res_sub %in% c("cov", "op", "fin", "pop", "post")))) {
          # if(inherits(get(input$data),c("PMcov","PMfinal","PMop", "PMsim"))){
          return(list(
            h3("Axes"),
            textInput("xmin", "X min"),
            textInput("xmax", "X max"),
            textInput("ymin", "Y min"),
            textInput("ymax", "Y max"),
            textInput("xlab", "X label"),
            textInput("ylab", "Y label"),
            numericInput("axis_label_size", "Axis label size", 1.2, step = 0.1)
          )) # end list
        } # end PM_cov, PM_final, PM_op, PM_sim


        ############### Axis: PM_result$data #####################

        if (!is.null(input$data) &&
          (inherits(get(input$data), "PM_data") |
            (inherits(get(input$data), "PM_result") && input$res_sub == "dat"))) {
          return(list(
            h3("Axes"),
            textInput("xmin", "X min"),
            textInput("xmax", "X max"),
            textInput("ymin", "Y min"),
            textInput("ymax", "Y max"),
            textInput("xlab", "X label", "Time (h)"),
            textInput("ylab", "Y label", "Observation"),
            numericInput("axis_label_size", "Axis label size", 1.2, step = 0.1)
          )) # end list
        } # end PM_data

        if (!is.null(input$data) &&
          (inherits(get(input$data), "PM_model") |
            (inherits(get(input$data), "PM_result") && input$res_sub == "mod"))) {
          return(NULL)
        }
      } # end makeAxisControls

      ############### Make PMplot and Statement #####################
      makePMplot <- function(code = FALSE) {
        obj <- current_data_obj()
        if (is.null(obj)) {
          return(NULL)
        }

        if (!is.null(input$data) &&
          (inherits(obj, "PM_data") |
            inherits(obj, "PM_result"))) {
          ############### Plot and Code: PM_result$cov #####################

          if (!is.null(input$res_sub) && input$res_sub == "cov") {
            # x argument
            x <- get(input$data)$cov

            # line argument
            cov_lm <- setVal("cov_lm", FALSE)
            def_lm_fmt <- setVal("def_lm_fmt", TRUE)
            def_lm_args <- list(ci = 0.95, color = "dodgerblue", width = 1, dash = "solid")
            lm_ci <- setVal("lm_ci", def_lm_args$ci)
            lm_col <- setVal("lm_col", def_lm_args$color)
            lm_lwd <- setVal("lm_lwd", def_lm_args$width)
            lm_dash <- setVal("lm_dash", def_lm_args$dash)

            if (cov_lm) { # yes have lm
              if (!def_lm_fmt) { # not default format?
                line <- list(lm = list(ci = lm_ci, color = lm_col, width = lm_lwd, dash = lm_dash))
                line$lm <- line$lm[!line$lm %in% def_lm_args] # keep only non-defaults
                if (length(line$lm) == 0) line$lm <- TRUE
              } else { # default format
                line <- list(lm = TRUE)
              }
            } else { # don't have lm
              line <- list() # default with cov plot
            }

            cov_loess <- setVal("cov_loess", TRUE)
            def_loess_fmt <- setVal("def_loess_fmt", TRUE)
            def_loess_args <- list(ci = 0.95, color = "dodgerblue", width = 1, dash = "dash")
            loess_ci <- setVal("loess_ci", def_loess_args$ci)
            loess_col <- setVal("loess_col", def_loess_args$color)
            loess_lwd <- setVal("loess_lwd", def_loess_args$width)
            loess_dash <- setVal("loess_dash", def_loess_args$dash)

            if (cov_loess) {
              if (!def_loess_fmt) { # not default format?
                line <- modifyList(line, list(loess = list(ci = loess_ci, color = loess_col, width = loess_lwd, dash = loess_dash)))
                line$loess <- line$loess[!line$loess %in% def_loess_args] # keep only non-defaults
                if (length(line$loess) == 0) line$loess <- TRUE
              }
            } else { # don't have loess
              line <- modifyList(line, list(loess = FALSE)) # default for this is TRUE
            }

            cov_ref <- setVal("cov_ref", FALSE)
            def_ref_fmt <- setVal("def_ref_fmt", TRUE)
            def_ref_args <- list(color = "black", width = 1, dash = "dash")
            ref_col <- setVal("ref_col", def_ref_args$color)
            ref_lwd <- setVal("ref_lwd", def_ref_args$width)
            ref_dash <- setVal("ref_dash", def_ref_args$dash)

            if (cov_ref) {
              if (!def_ref_fmt) {
                line <- modifyList(line, list(ref = list(color = ref_col, width = ref_lwd, dash = ref_dash)))
                line$ref <- line$ref[!line$ref %in% def_ref_args] # keep only non-defaults
                if (length(line$ref) == 0) line$ref <- TRUE
              } else {
                line <- modifyList(line, list(ref = TRUE))
              }
            }


            # marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(
              color = "orange", symbol = "circle", size = 10, opacity = 0.5,
              line = list(width = 1, color = "black")
            )

            mrk_col <- parse_marker_color_scalar(def_marker_args$color)
            mrk_symbol <- parse_marker_symbol_scalar(def_marker_args$symbol)
            mrk_size <- setVal("mrk_size", 10)
            mrk_opacity <- setVal("mrk_opacity", 0.5)
            mrk_lwd <- setVal("mrk_lwd", 1)
            mrk_lcol <- setVal("mrk_lcol", "black")

            if (def_marker_fmt) {
              marker <- TRUE
            } else {
              marker <- list(
                color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol)
              )
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] # keep only non-defaults
              if (length(marker$line) == 0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args]
              if (length(marker) == 0) marker <- NULL
            }

            # include/exclude arguments
            if (!is.null(input$cov_include)) {
              if (input$cov_include == "yes") {
                include <- input$cov_select
                exclude <- NULL
              } else {
                include <- NULL
                exclude <- input$cov_select
              }
            } else {
              include <- exclude <- NULL
            }

            # title argument
            def_title_fmt <- setVal("def_title_fmt", TRUE)
            def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
            title_text <- setVal("title_text", "")
            title_col <- setVal("title_col", "black")
            title_bold <- setVal("title_bold", TRUE)
            title_size <- setVal("title_size", 20)
            if (def_title_fmt) {
              title <- ""
            } else {
              title <- list(text = title_text, font = list(
                color = title_col, size = title_size,
                bold = title_bold
              ))
              title$font <- title$font[!title$font %in% def_title_args$font] # keep only non-defaults
              if (length(title$font) == 0) title$font <- NULL
            }

            # stats argument
            def_stats_fmt <- setVal("def_stats_fmt", TRUE)
            def_stats_args <- list(x = 0.8, y = 0.1, font = list(color = "black", size = 14, bold = FALSE))
            stats_col <- setVal("stats_col", "black")
            stats_size <- setVal("stats_size", 14)
            stats_bold <- setVal("stats_bold", FALSE)
            stats_x <- setVal("stats_x", 0.8)
            stats_y <- setVal("stats_y", 0.1)
            if (def_stats_fmt) {
              stats <- TRUE
            } else {
              stats <- list(x = stats_x, y = stats_y, font = list(
                color = stats_col, size = stats_size,
                bold = stats_bold
              ))
              stats$font <- stats$font[!stats$font %in% def_stats_args$font] # keep only non-defaults
              if (length(stats$font) == 0) stats$font <- NULL
              stats <- stats[!stats %in% def_stats_args]
              if (length(stats) == 0) stats <- NULL
            }

            # formula
            cov_choices <- names(get(input$data)$cov$data)
            cov_choices <- cov_choices[cov_choices != "icen"]

            formula <- getFormula(
              x = "covX", y = "covY",
              choices = cov_choices
            )

            # Other defaults
            icen <- setVal("icen", "mean")
            log <- setVal("log", FALSE)
            grid <- setVal("grid", TRUE)


            args <- list(
              x = x,
              formula = formula,
              line = line,
              marker = marker,
              icen = icen,
              include = include, exclude = exclude,
              log = log, grid = grid,
              title = title, stats = stats,
              xlab = getXlab(), ylab = getYlab(),
              xlim = getXlim(), ylim = getYlim()
            )
            args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]

            # now make the code
            Name <- paste0(input$data, "$cov$plot(")
            # default args
            def_args <- list(
              formula = NULL,
              line = list(lm = FALSE, loess = TRUE, ref = FALSE),
              marker = TRUE,
              icen = "median",
              include = as.character(unique(get(input$data)$cov$data$id)), exclude = NULL,
              log = FALSE,
              grid = TRUE,
              xlab = NULL, ylab = NULL,
              title = "",
              stats = TRUE,
              xlim = NULL, ylim = NULL
            )

            arglist <- args[-1] # remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))] # remove args that are in default list
            if (length(arglist) > 0) {
              arglist <- paste(deparse(arglist), collapse = "") %>%
                stringr::str_replace("^list\\(", "") %>%
                stringr::str_replace_all("(\\d+)L", "\\1") %>%
                stringr::str_replace_all(" +", " ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }

            if (!is.null(formula)) {
              p <- do_plot(args)
              if (code) {
                return(codeStatement)
              } else {
                return(p)
              }
            } else {
              return(NULL)
            }
          } # end PMcov


          ############### Plot and Code: PM_result$final, NPAG #####################

          if (!is.null(input$res_sub) &&
            (input$res_sub == "fin" & inherits(get(input$data)$final, "NPAG"))) {
            ptype <- setVal("ptype", "uni")

            # x argument
            x <- get(input$data)$final

            # marker argument
            def_bar_fmt <- setVal("def_bar_fmt", TRUE)
            def_mrk_fmt <- setVal("def_mrk_fmt", TRUE)

            def_bar_args <- list(
              color = "dodgerblue",
              width = 0.02, opacity = 0.5,
              line = list(color = "black", width = 1)
            )
            def_mrk_args <- list(
              color = "dodgerblue", size = 5, symbol = "circle",
              opacity = 0.5,
              line = list(color = "black", width = 1)
            )

            bar_col <- setVal("bar_col", def_bar_args$color)
            bar_width <- setVal("bar_width", def_bar_args$width)
            bar_size <- setVal("bar_size", def_bar_args$size)
            bar_shape <- setVal("bar_shape", def_bar_args$shape)
            bar_opacity <- setVal("bar_opacity", def_bar_args$opacity)
            bar_lcol <- setVal("bar_lcol", def_bar_args$line$color)
            bar_lwd <- setVal("bar_lwd", def_bar_args$line$width)

            mrk_col <- parse_marker_color_scalar(def_mrk_args$color)
            mrk_width <- setVal("mrk_width", def_mrk_args$width)
            mrk_size <- setVal("mrk_size", def_mrk_args$size)
            mrk_symbol <- parse_marker_symbol_scalar(def_mrk_args$symbol)
            mrk_opacity <- setVal("mrk_opacity", def_mrk_args$opacity)
            mrk_lcol <- setVal("mrk_lcol", def_mrk_args$line$color)
            mrk_lwd <- setVal("mrk_lwd", def_mrk_args$line$width)

            if (ptype == "uni") {
              if (def_bar_fmt) {
                marker <- TRUE
              } else {
                marker <- list(
                  color = bar_col, width = bar_width,
                  opacity = bar_opacity,
                  line = list(color = bar_lcol, width = bar_lwd)
                )
                marker$line <- marker$line[!marker$line %in% def_bar_args$line] # keep only non-defaults
                if (length(marker$line) == 0) marker$line <- NULL
                marker <- marker[!marker %in% def_bar_args]
                if (length(marker) == 0) marker <- NULL
              }
            }

            if (ptype == "bi") {
              if (def_mrk_fmt) {
                marker <- TRUE
              } else {
                marker <- list(
                  color = mrk_col, size = mrk_size,
                  symbol = mrk_symbol, opacity = mrk_opacity,
                  line = list(color = mrk_lcol, width = mrk_lwd)
                )
                marker$line <- marker$line[!marker$line %in% def_mrk_args$line] # keep only non-defaults
                if (length(marker$line) == 0) marker$line <- NULL
                marker <- marker[!marker %in% def_mrk_args]
                if (length(marker) == 0) marker <- NULL
              }
            }

            # line format

            incl_line <- setVal("incl_line", FALSE)
            incl_drop <- setVal("incl_drop", TRUE)

            def_line_fmt <- setVal("def_line_fmt", TRUE)
            def_line_args <- list(
              color = "black", width = 1,
              dash = "solid"
            )
            def_line_color <- setVal("line_col", def_line_args$color)
            def_line_width <- setVal("line_width", def_line_args$width)
            def_line_dash <- setVal("line_dash", def_line_args$dash)

            def_drop_fmt <- setVal("def_drop_fmt", TRUE)
            def_drop_args <- list(
              color = "black", width = 1,
              dash = "dash"
            )
            def_drop_color <- setVal("drop_col", def_drop_args$color)
            def_drop_width <- setVal("drop_width", def_drop_args$width)
            def_drop_dash <- setVal("drop_dash", def_drop_args$dash)


            if (ptype == "uni") {
              if (incl_line) { # we have a density line
                if (def_line_fmt) { # use default
                  line <- TRUE
                } else {
                  line <- list(color = def_line_color, width = def_line_width, dash = def_line_dash)
                }
              } else {
                line <- FALSE
              }
              line <- line[!line %in% def_line_args]
              if (length(line) == 0) line <- NULL
            }

            if (ptype == "bi") {
              if (incl_drop) { # we have drop lines
                if (def_drop_fmt) { # use default
                  line <- TRUE
                } else {
                  line <- list(color = def_drop_color, width = def_drop_width, dash = def_drop_dash)
                }
              } else {
                line <- FALSE
              }

              line <- line[!line %in% def_drop_args]
              if (length(line) == 0) line <- NULL
            }



            args <- list(
              x = x,
              formula = getFormula(choices = names(get(input$data)$final$popMean)),
              line = line,
              marker = marker,
              xlim = getXlim(), ylim = getYlim(),
              xlab = getXlab(), ylab = getYlab()
            )

            if (ptype == "uni") {
              args$formula <- NULL
            } # reset formula


            args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]

            # now make the code
            Name <- paste0(input$data, "$final$plot(")
            # default args
            def_args <- list(
              formula = NULL,
              line = ifelse(ptype == "uni", FALSE, TRUE),
              marker = TRUE,
              xlab = NULL, ylab = NULL, zlab = NULL,
              title = "",
              xlim = NULL, ylim = NULL
            )

            arglist <- args[-1] # remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))] # remove args that are in default list

            if (length(arglist) > 0) {
              arglist <- paste(deparse(arglist), collapse = "") %>%
                stringr::str_replace("^list\\(", "") %>%
                stringr::str_replace_all("(\\d+)L", "\\1") %>%
                stringr::str_replace_all(" +", " ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }

            p <- do_plot(args)
            if (code) {
              return(codeStatement)
            } else {
              return(p)
            }
          } # end PMfinal, NPAG

          ############### Plot and Code: PM_result$final, IT2B #####################

          # if(!is.null(input$res_sub) &&
          #    (input$res_sub == "fin" & inherits(get(input$data)$final,"IT2B"))){
          #
          #   x <- get(input$data)
          #   args <- list(x=x,formula=getFormula(choices=names(get(input$data)$popMean)),cex.lab=input$cex.lab,col=getFinalPlotType()$col,
          #                pch=input$pch,cex=input$cex,lwd=getFinalPlotType()$lwd,probs=getProbs(),standard=input$standard,legend=input$legend,
          #                grid=input$grid,xlim=getXlim(),ylim=getYlim(),xlab=getXlab(),ylab=getYlab())
          #   if(getFinalPlotType()$ptype=="uni"){args$formula <- NULL} #reset formula
          #   args <- args[which(sapply(args,function(x) length(x)>0),arr.ind=T)]
          #   do.call(plot.PMfinal,args)
          # } #end PMfinal, IT2B

          ############### Plot and Code: PM_result$op #####################

          if (!is.null(input$res_sub) && input$res_sub == "op") {
            # x argument
            x <- get(input$data)$op

            # resid sets different defaults for lines to follow
            resid <- setVal("resid", FALSE)

            # line argument
            op_lm <- setVal("op_lm", ifelse(resid, FALSE, TRUE))
            def_lm_fmt <- setVal("def_lm_fmt", TRUE)
            def_lm_args <- list(ci = 0.95, color = "dodgerblue", width = 1, dash = "solid")
            lm_ci <- setVal("lm_ci", def_lm_args$ci)
            lm_col <- setVal("lm_col", def_lm_args$color)
            lm_lwd <- setVal("lm_lwd", def_lm_args$width)
            lm_dash <- setVal("lm_dash", def_lm_args$dash)

            if (op_lm) { # yes have lm
              if (!def_lm_fmt) { # not default format?
                line <- list(lm = list(ci = lm_ci, color = lm_col, width = lm_lwd, dash = lm_dash))
                line$lm <- line$lm[!line$lm %in% def_lm_args] # keep only non-defaults
                if (length(line$lm) == 0) line$lm <- TRUE
              } else { # default format
                if (resid) {
                  line <- list(lm = TRUE) # only need if residual plot, as default for op plot
                } else {
                  line <- list() # we have lm but don't need with default format and op plot
                }
              }
            } else { # don't have lm
              line <- list(lm = FALSE)
            }

            op_loess <- setVal("op_loess", ifelse(resid, TRUE, FALSE))
            def_loess_fmt <- setVal("def_loess_fmt", TRUE)
            def_loess_args <- list(ci = 0.95, color = "dodgerblue", width = 1, dash = "dash")
            loess_ci <- setVal("loess_ci", def_loess_args$ci)
            loess_col <- setVal("loess_col", def_loess_args$color)
            loess_lwd <- setVal("loess_lwd", def_loess_args$width)
            loess_dash <- setVal("loess_dash", def_loess_args$dash)

            if (op_loess) {
              if (!def_loess_fmt) { # not default format?
                line <- modifyList(line, list(loess = list(ci = loess_ci, color = loess_col, width = loess_lwd, dash = loess_dash)))
                line$loess <- line$loess[!line$loess %in% def_loess_args] # keep only non-defaults
                if (length(line$loess) == 0) line$loess <- TRUE
              } else { # default format
                if (!resid) line <- modifyList(line, list(loess = TRUE)) # only need if op plot, as default for resid plot
              }
            } else { # don't have loess
              if (resid) line <- modifyList(line, list(loess = FALSE)) # only add if resid plot, as default for this is TRUE
            }

            op_ref <- setVal("op_ref", TRUE) # regardless of resid
            def_ref_fmt <- setVal("def_ref_fmt", TRUE)
            def_ref_args <- list(color = "black", width = 1, dash = "dash")
            ref_col <- setVal("ref_col", def_ref_args$color)
            ref_lwd <- setVal("ref_lwd", def_ref_args$width)
            ref_dash <- setVal("ref_dash", def_ref_args$dash)

            if (op_ref) {
              if (!def_ref_fmt) {
                line <- modifyList(line, list(ref = list(color = ref_col, width = ref_lwd, dash = ref_dash)))
                line$ref <- line$ref[!line$ref %in% def_ref_args] # keep only non-defaults
                if (length(line$ref) == 0) line$ref <- TRUE
              }
            } else {
              line <- modifyList(line, list(ref = FALSE))
            }

            # marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(
              color = "orange", symbol = "circle", size = 10, opacity = 0.5,
              line = list(width = 1, color = "black")
            )

            mrk_col <- parse_marker_color_scalar(def_marker_args$color)
            mrk_symbol <- parse_marker_symbol_scalar(def_marker_args$symbol)
            mrk_size <- setVal("mrk_size", 10)
            mrk_opacity <- setVal("mrk_opacity", 0.5)
            mrk_lwd <- setVal("mrk_lwd", 1)
            mrk_lcol <- setVal("mrk_lcol", "black")

            if (def_marker_fmt) {
              marker <- TRUE
            } else {
              marker <- list(
                color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol)
              )
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] # keep only non-defaults
              if (length(marker$line) == 0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args]
              if (length(marker) == 0) marker <- NULL
            }

            # include/exclude arguments
            if (!is.null(input$op_include)) {
              if (input$op_include == "yes") {
                include <- input$op_select
                exclude <- NULL
              } else {
                include <- NULL
                exclude <- input$op_select
              }
            } else {
              include <- exclude <- NULL
            }

            # title argument
            def_title_fmt <- setVal("def_title_fmt", TRUE)
            def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
            title_text <- setVal("title_text", "")
            title_col <- setVal("title_col", "black")
            title_bold <- setVal("title_bold", TRUE)
            title_size <- setVal("title_size", 20)
            if (def_title_fmt) {
              title <- ""
            } else {
              title <- list(text = title_text, font = list(
                color = title_col, size = title_size,
                bold = title_bold
              ))
              title$font <- title$font[!title$font %in% def_title_args$font] # keep only non-defaults
              if (length(title$font) == 0) title$font <- NULL
            }

            # stats argument
            def_stats_fmt <- setVal("def_stats_fmt", TRUE)
            def_stats_args <- list(x = 0.8, y = 0.1, font = list(color = "black", size = 14, bold = FALSE))
            stats_col <- setVal("stats_col", "black")
            stats_size <- setVal("stats_size", 14)
            stats_bold <- setVal("stats_bold", FALSE)
            stats_x <- setVal("stats_x", 0.8)
            stats_y <- setVal("stats_y", 0.1)
            if (def_stats_fmt) {
              stats <- TRUE
            } else {
              stats <- list(x = stats_x, y = stats_y, font = list(
                color = stats_col, size = stats_size,
                bold = stats_bold
              ))
              stats$font <- stats$font[!stats$font %in% def_stats_args$font] # keep only non-defaults
              if (length(stats$font) == 0) stats$font <- NULL
              stats <- stats[!stats %in% def_stats_args]
              if (length(stats) == 0) stats <- NULL
            }

            # Other defaults
            icen <- setVal("op_icen", "median")
            pred.type <- setVal("pred.type", "post")
            outeq <- setVal("outeq", 1)
            outeq <- as.numeric(outeq)
            if (length(outeq) == 0) {
              outeq <- 1
            } else {
              outeq <- outeq[[1]]
            }
            block <- setVal("block", "All")
            mult <- setVal("mult", 1)
            log <- setVal("log", FALSE)
            grid <- setVal("grid", TRUE)

            if (block == "All") block <- NULL


            args <- list(
              x = x, line = line, marker = marker, resid = resid,
              icen = icen, pred.type = pred.type, outeq = outeq,
              block = block, include = include, exclude = exclude,
              mult = as.numeric(mult), log = log, grid = grid,
              title = title, stats = stats,
              xlab = getXlab(), ylab = getYlab(),
              xlim = getXlim(), ylim = getYlim()
            )
            args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]

            # now make the code
            Name <- paste0(input$data, "$op$plot(")
            # default args
            def_args <- list(
              line = list(lm = TRUE, loess = FALSE, ref = TRUE),
              marker = TRUE,
              resid = FALSE,
              icen = "median", pred.type = "post", outeq = 1, block = NULL,
              include = as.character(unique(get(input$data)$op$id)), exclude = NULL,
              mult = 1,
              log = FALSE,
              grid = TRUE,
              xlab = NULL, ylab = NULL,
              title = "",
              stats = TRUE,
              xlim = NULL, ylim = NULL
            )

            if (resid) modifyList(def_args$line, list(lm = FALSE, loess = TRUE, ref = TRUE)) # modify default

            arglist <- args[-1] # remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))] # remove args that are in default list
            if (length(arglist) > 0) {
              arglist <- paste(deparse(arglist), collapse = "") %>%
                stringr::str_replace("^list\\(", "") %>%
                stringr::str_replace_all("(\\d+)L", "\\1") %>%
                stringr::str_replace_all(" +", " ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }

            p <- do_plot(args)
            if (code) {
              return(codeStatement)
            } else {
              return(p)
            }
          } # end PM_op

          ############### Plot and Code: PM_result$pop #####################

          if (!is.null(input$res_sub) && input$res_sub == "pop") {
            # x argument
            x <- get(input$data)$pop

            # icen argument (single value)
            icen <- setVal("pop_icen", "median")
            if (length(icen) == 0) icen <- "median"
            icen <- icen[1]

            # outeq argument (can be multiple)
            outeq <- setVal("pop_outeq", 1)
            outeq <- as.numeric(outeq)
            if (length(outeq) == 0) outeq <- 1

            # block argument
            block_val <- setVal("pop_block", "All")
            if (identical(block_val, "All") || length(block_val) == 0) {
              block <- sort(unique(x$data$block))
            } else {
              block <- as.numeric(block_val)
            }

            # include/exclude arguments
            if (!is.null(input$pop_include)) {
              if (input$pop_include == "yes") {
                include <- input$pop_select
                exclude <- NULL
              } else {
                include <- NULL
                exclude <- input$pop_select
              }
            } else {
              include <- exclude <- NULL
            }

            # line argument
            pop_join <- setVal("pop_join", TRUE)
            def_pop_line_fmt <- setVal("def_pop_line_fmt", TRUE)
            def_pop_line_args <- list(width = 1, dash = "solid")
            pop_line_lwd <- setVal("pop_line_lwd", def_pop_line_args$width)
            pop_line_dash <- setVal("pop_line_dash", def_pop_line_args$dash)

            if (pop_join) {
              if (!def_pop_line_fmt) {
                line_join <- list(width = pop_line_lwd, dash = pop_line_dash)
                line_join <- line_join[!line_join %in% def_pop_line_args]
                if (length(line_join) == 0) {
                  line <- list(join = TRUE)
                } else {
                  line <- list(join = line_join)
                }
              } else {
                line <- list(join = TRUE)
              }
            } else {
              line <- list(join = FALSE)
            }

            # marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(
              color = "red", symbol = "circle", size = 10, opacity = 0.5,
              line = list(width = 1, color = "black")
            )

            mrk_col <- parse_marker_color(def_marker_args$color)
            mrk_symbol <- parse_marker_symbol(def_marker_args$symbol)
            mrk_size <- setVal("mrk_size", def_marker_args$size)
            mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
            mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
            mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)

            if (def_marker_fmt) {
              marker <- FALSE
            } else {
              marker <- list(
                color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol)
              )
              marker$line <- marker$line[!marker$line %in% def_marker_args$line]
              if (length(marker$line) == 0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args]
              if (length(marker) == 0) marker <- NULL
            }

            # out_names argument
            pop_out_names_raw <- setVal("pop_out_names", "")
            out_names_arg <- NULL
            if (!is.null(pop_out_names_raw) && nzchar(pop_out_names_raw)) {
              out_names_arg <- unlist(stringr::str_split(pop_out_names_raw, "\\s*,\\s*"))
              out_names_arg <- stringr::str_trim(out_names_arg)
              out_names_arg <- out_names_arg[nzchar(out_names_arg)]
              if (length(out_names_arg) == 0) out_names_arg <- NULL
            }

            # other arguments
            mult <- setVal("mult", 1)
            log <- setVal("log", FALSE)
            grid <- setVal("grid", FALSE)
            legend <- setVal("legend", FALSE)

            # title argument
            def_title_fmt <- setVal("def_title_fmt", TRUE)
            def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
            title_text <- setVal("title_text", "")
            title_col <- setVal("title_col", "black")
            title_bold <- setVal("title_bold", TRUE)
            title_size <- setVal("title_size", 20)
            if (def_title_fmt) {
              title <- ""
            } else {
              title <- list(text = title_text, font = list(
                color = title_col, size = title_size,
                bold = title_bold
              ))
              title$font <- title$font[!title$font %in% def_title_args$font]
              if (length(title$font) == 0) title$font <- NULL
            }

            args <- list(
              x = x, line = line, marker = marker,
              out_names = out_names_arg,
              icen = icen, outeq = outeq, block = block,
              include = include, exclude = exclude,
              mult = as.numeric(mult), log = log, grid = grid,
              legend = legend,
              title = title,
              xlab = getXlab(), ylab = getYlab(),
              xlim = getXlim(), ylim = getYlim()
            )
            args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]

            # now make the code
            Name <- paste0(input$data, "$pop$plot(")
            all_blocks <- sort(unique(get(input$data)$pop$data$block))
            all_subs <- as.character(unique(get(input$data)$pop$data$id))
            def_args <- list(
              line = list(join = TRUE),
              marker = FALSE,
              out_names = NULL,
              icen = "median", outeq = 1, block = all_blocks,
              include = all_subs, exclude = NULL,
              mult = 1,
              log = FALSE,
              grid = FALSE,
              legend = FALSE,
              title = "",
              xlab = NULL, ylab = NULL,
              xlim = NULL, ylim = NULL
            )

            arglist <- args[-1] # remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))]
            if (length(arglist) > 0) {
              arglist <- paste(deparse(arglist), collapse = "") %>%
                stringr::str_replace("^list\\(", "") %>%
                stringr::str_replace_all("(\\d+)L", "\\1") %>%
                stringr::str_replace_all(" +", " ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }

            p <- do_plot(args)
            if (code) {
              return(codeStatement)
            } else {
              return(p)
            }
          } # end PM_pop

          ############### Plot and Code: PM_result$post #####################

          if (!is.null(input$res_sub) && input$res_sub == "post") {
            # x argument
            x <- get(input$data)$post

            # icen argument (single value)
            icen <- setVal("post_icen", "median")
            if (length(icen) == 0) icen <- "median"
            icen <- icen[1]

            # outeq argument (can be multiple)
            outeq <- setVal("post_outeq", 1)
            outeq <- as.numeric(outeq)
            if (length(outeq) == 0) outeq <- 1

            # block argument
            block_val <- setVal("post_block", "All")
            if (identical(block_val, "All") || length(block_val) == 0) {
              block <- sort(unique(x$data$block))
            } else {
              block <- as.numeric(block_val)
            }

            # include/exclude arguments
            if (!is.null(input$post_include)) {
              if (input$post_include == "yes") {
                include <- input$post_select
                exclude <- NULL
              } else {
                include <- NULL
                exclude <- input$post_select
              }
            } else {
              include <- exclude <- NULL
            }

            # line argument
            post_join <- setVal("post_join", TRUE)
            def_post_line_fmt <- setVal("def_post_line_fmt", TRUE)
            def_post_line_args <- list(width = 1, dash = "solid")
            post_line_lwd <- setVal("post_line_lwd", def_post_line_args$width)
            post_line_dash <- setVal("post_line_dash", def_post_line_args$dash)

            if (post_join) {
              if (!def_post_line_fmt) {
                line_join <- list(width = post_line_lwd, dash = post_line_dash)
                line_join <- line_join[!line_join %in% def_post_line_args]
                if (length(line_join) == 0) {
                  line <- list(join = TRUE)
                } else {
                  line <- list(join = line_join)
                }
              } else {
                line <- list(join = TRUE)
              }
            } else {
              line <- list(join = FALSE)
            }

            # marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(
              color = "red", symbol = "circle", size = 10, opacity = 0.5,
              line = list(width = 1, color = "black")
            )

            mrk_col <- parse_marker_color(def_marker_args$color)
            mrk_symbol <- parse_marker_symbol(def_marker_args$symbol)
            mrk_size <- setVal("mrk_size", def_marker_args$size)
            mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
            mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
            mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)

            if (def_marker_fmt) {
              marker <- FALSE
            } else {
              marker <- list(
                color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol)
              )
              marker$line <- marker$line[!marker$line %in% def_marker_args$line]
              if (length(marker$line) == 0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args]
              if (length(marker) == 0) marker <- NULL
            }

            # out_names argument
            post_out_names_raw <- setVal("post_out_names", "")
            out_names_arg <- NULL
            if (!is.null(post_out_names_raw) && nzchar(post_out_names_raw)) {
              out_names_arg <- unlist(stringr::str_split(post_out_names_raw, "\\s*,\\s*"))
              out_names_arg <- stringr::str_trim(out_names_arg)
              out_names_arg <- out_names_arg[nzchar(out_names_arg)]
              if (length(out_names_arg) == 0) out_names_arg <- NULL
            }

            # other arguments
            mult <- setVal("mult", 1)
            log <- setVal("log", FALSE)
            grid <- setVal("grid", FALSE)
            legend <- setVal("legend", FALSE)

            # title argument
            def_title_fmt <- setVal("def_title_fmt", TRUE)
            def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
            title_text <- setVal("title_text", "")
            title_col <- setVal("title_col", "black")
            title_bold <- setVal("title_bold", TRUE)
            title_size <- setVal("title_size", 20)
            if (def_title_fmt) {
              title <- ""
            } else {
              title <- list(text = title_text, font = list(
                color = title_col, size = title_size,
                bold = title_bold
              ))
              title$font <- title$font[!title$font %in% def_title_args$font]
              if (length(title$font) == 0) title$font <- NULL
            }

            args <- list(
              x = x, line = line, marker = marker,
              out_names = out_names_arg,
              icen = icen, outeq = outeq, block = block,
              include = include, exclude = exclude,
              mult = as.numeric(mult), log = log, grid = grid,
              legend = legend,
              title = title,
              xlab = getXlab(), ylab = getYlab(),
              xlim = getXlim(), ylim = getYlim()
            )
            args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]

            # now make the code
            Name <- paste0(input$data, "$post$plot(")
            all_blocks <- sort(unique(get(input$data)$post$data$block))
            all_subs <- as.character(unique(get(input$data)$post$data$id))
            def_args <- list(
              line = list(join = TRUE),
              marker = FALSE,
              out_names = NULL,
              icen = "median", outeq = 1, block = all_blocks,
              include = all_subs, exclude = NULL,
              mult = 1,
              log = FALSE,
              grid = FALSE,
              legend = FALSE,
              title = "",
              xlab = NULL, ylab = NULL,
              xlim = NULL, ylim = NULL
            )

            arglist <- args[-1] # remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))]
            if (length(arglist) > 0) {
              arglist <- paste(deparse(arglist), collapse = "") %>%
                stringr::str_replace("^list\\(", "") %>%
                stringr::str_replace_all("(\\d+)L", "\\1") %>%
                stringr::str_replace_all(" +", " ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }

            p <- do_plot(args)
            if (code) {
              return(codeStatement)
            } else {
              return(p)
            }
          } # end PM_post

          ############### Plot and Code: PM_result$cycle #####################

          if (!is.null(input$res_sub) && input$res_sub == "cyc") {
            x <- get(input$data)$cycle

            if (length(input$omit) == 0) {
              omit <- 0.2
            } else {
              omit <- input$omit
            }

            # line argument
            def_ab_fmt <- setVal("def_ab_fmt", TRUE)
            def_a_args <- list(color = "dodgerblue", width = 1, dash = "solid")
            def_b_args <- list(colors = "Spectral", custom_colors = "", linetypes = "dash")
            a_col <- setVal("a_col", def_a_args$color)
            a_dash <- setVal("a_dash", def_a_args$dash)
            ab_lwd <- setVal("ab_lwd", def_a_args$width)
            b_col <- setVal("b_col", def_b_args$colors)
            b_custom_colors <- setVal("b_custom_colors", def_b_args$custom_colors)
            b_dash <- setVal("b_dash", def_b_args$linetypes)


            if (!def_ab_fmt) { # not default format?
              line <- list(color = a_col, width = ab_lwd, dash = a_dash)
              line <- line[!line %in% def_a_args] # keep only non-defaults
              if (length(line) == 0) line <- TRUE

              linetypes <- b_dash

              if (b_custom_colors != "") {
                colors <- unlist(stringr::str_split(b_custom_colors, "\\s*,\\s*"))
              } else {
                colors <- b_col # built in palette
              }
            } else { # default format
              line <- list() # we have join but don't need with default format and data plot
              colors <- list()
              linetypes <- list()
            }

            # marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(
              color = "dodgerblue", symbol = "circle", size = 4, opacity = 1,
              line = list(width = 0, color = "black")
            )

            mrk_col <- parse_marker_color_scalar(def_marker_args$color)
            mrk_symbol <- parse_marker_symbol_scalar(def_marker_args$symbol)
            mrk_size <- setVal("mrk_size", def_marker_args$size)
            mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
            mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
            mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)

            if (def_marker_fmt) {
              marker <- TRUE
            } else {
              marker <- list(
                color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol)
              )
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] # keep only non-defaults
              if (length(marker$line) == 0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args]
              if (length(marker) == 0) marker <- NULL
            }

            # Other defaults
            grid <- setVal("grid", TRUE)

            args <- list(
              x = x,
              line = line,
              marker = marker,
              colors = colors,
              linetypes = linetypes,
              omit = omit,
              grid = grid,
              xlab = getXlab(), ylab = getYlab()
            )

            args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]
            # now make the code
            Name <- paste0(input$data, "$cycle$plot(")
            # default args
            def_args <- list(
              line = TRUE,
              marker = TRUE,
              colors = NULL,
              linetypes = NULL,
              omit = 0.2,
              grid = TRUE,
              xlab = NULL, ylab = NULL
            )

            arglist <- args[-1] # remove the data object
            arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))] # remove args that are in default list
            if (length(arglist) > 0) {
              arglist <- paste(deparse(arglist), collapse = "") %>%
                stringr::str_replace("^list\\(", "") %>%
                stringr::str_replace_all("(\\d+)L", "\\1") %>%
                stringr::str_replace_all(" +", " ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }

            p <- do_plot(args)
            if (code) {
              return(codeStatement)
            } else {
              return(p)
            }
          } # end PMcycle


          ############### Plot and Code: PM_data & PM_result$data #####################

          if (inherits(get(input$data), "PM_data") ||
            (!is.null(input$res_sub) && input$res_sub == "dat")) {
            # x argument
            if (inherits(get(input$data), "PM_result")) {
              x <- get(input$data)$data
            }

            if (inherits(get(input$data), "PM_data")) {
              x <- get(input$data)
            }

            # line argument
            join <- setVal("join", TRUE)
            def_join_fmt <- setVal("def_join_fmt", TRUE)
            def_join_args <- list(color = "dodgerblue", width = 1, dash = "solid")
            join_col <- setVal("join_col", def_join_args$color)
            join_lwd <- setVal("join_lwd", def_join_args$width)
            join_dash <- setVal("join_dash", def_join_args$dash)

            if (join) { # yes have join
              if (!def_join_fmt) { # not default format?
                line <- list(join = list(color = join_col, width = join_lwd, dash = join_dash))
                line$join <- line$join[!line$join %in% def_join_args] # keep only non-defaults
                if (length(line$join) == 0) line$join <- TRUE
              } else { # default format
                line <- list() # we have join but don't need with default format and data plot
              }
            } else { # don't have join
              line <- list(join = FALSE)
            }

            incl_pred <- setVal("incl_pred", "none")
            def_pred_fmt <- setVal("def_pred_fmt", TRUE)
            def_pred_args <- list(color = "", width = 1, dash = "dash")
            pred_col <- setVal("pred_col", def_pred_args$color)
            pred_lwd <- setVal("pred_lwd", def_pred_args$width)
            pred_dash <- setVal("pred_dash", def_pred_args$dash)

            if (incl_pred != "none") {
              pred_obj <- get(input$data)[[incl_pred]]
              if (!def_pred_fmt) { # not default format?
                pred <- list(color = pred_col, width = pred_lwd, dash = pred_dash)
                pred <- pred[!pred %in% def_pred_args] # keep only non-defaults
                if (length(pred) == 0) {
                  line$pred <- pred_obj
                } else {
                  line$pred <- c(pred_obj, pred)
                }
              } else { # default format
                line$pred <- pred_obj
              }
            }

            # marker argument
            def_marker_fmt <- setVal("def_marker_fmt", TRUE)
            def_marker_args <- list(
              color = "red", symbol = "circle", size = 10, opacity = 0.5,
              line = list(width = 1, color = "black")
            )

            mrk_col <- parse_marker_color(def_marker_args$color)
            mrk_symbol <- parse_marker_symbol(def_marker_args$symbol)
            mrk_size <- setVal("mrk_size", def_marker_args$size)
            mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
            mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
            mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)

            if (def_marker_fmt) {
              marker <- TRUE
            } else {
              marker <- list(
                color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol)
              )
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] # keep only non-defaults
              if (length(marker$line) == 0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args]
              if (length(marker) == 0) marker <- NULL
            }

            # include/exclude arguments
            if (!is.null(input$data_include)) {
              if (input$data_include == "yes") {
                include <- input$data_select
                exclude <- NULL
              } else {
                include <- NULL
                exclude <- input$data_select
              }
            } else {
              include <- exclude <- NULL
            }

            # title argument
            def_title_fmt <- setVal("def_title_fmt", TRUE)
            def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
            title_text <- setVal("title_text", "")
            title_col <- setVal("title_col", "black")
            title_bold <- setVal("title_bold", TRUE)
            title_size <- setVal("title_size", 20)
            if (def_title_fmt) {
              title <- ""
            } else {
              title <- list(text = title_text, font = list(
                color = title_col, size = title_size,
                bold = title_bold
              ))
              title$font <- title$font[!title$font %in% def_title_args$font] # keep only non-defaults
              if (length(title$font) == 0) title$font <- NULL
            }

            # group names
            group_names <- setVal("data_group_names", NULL)
            if (!is.null(group_names)) {
              group_names <- unlist(stringr::str_split(group_names, "\\s*,\\s*"))
              group_names <- stringr::str_trim(group_names)
              group_names <- group_names[nzchar(group_names)]
              if (length(group_names) == 0) {
                group_names <- NULL
              }
            }

            # output names
            out_names <- setVal("data_out_names", NULL)
            if (!is.null(out_names)) {
              out_names <- unlist(stringr::str_split(out_names, "\\s*,\\s*"))
              out_names <- stringr::str_trim(out_names)
              out_names <- out_names[nzchar(out_names)]
              if (length(out_names) == 0) {
                out_names <- NULL
              }
            }

            # legend
            if (!is.null(input$data_group) && input$data_group != "none") {
              legend <- setVal("legend", TRUE)
            } else {
              legend <- setVal("legend", FALSE)
            }


            # Other defaults
            color <- setVal("data_group", "none")
            tad <- setVal("tad", FALSE)
            outeq <- setVal("data_outeq", 1)
            block <- setVal("data_block", 1)
            mult <- setVal("mult", 1)
            log <- setVal("log", FALSE)
            grid <- setVal("grid", FALSE)

            if (color == "none") color <- NULL
            if (block == "All") block <- 1
            if (length(as.numeric(outeq)) <= 1) out_names <- NULL

            args <- list(
              x = x,
              include = include, exclude = exclude,
              line = line, marker = marker,
              group = color,
              group_names = group_names,
              outeq = as.numeric(outeq),
              out_names = out_names,
              block = as.numeric(block),
              tad = tad,
              # overlay = FALSE,
              legend = legend,
              mult = as.numeric(mult), log = log, grid = grid,
              title = title,
              xlab = getXlab(), ylab = getYlab(),
              xlim = getXlim(), ylim = getYlim()
            )

            args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]



            # now make the code
            if (inherits(get(input$data), "PM_result")) {
              Name <- paste0(input$data, "$data$plot(")
              id_obj <- as.character(unique(get(input$data)$data$data$id))
            }

            if (inherits(get(input$data), "PM_data")) {
              Name <- paste0(input$data, "$plot(")
              id_obj <- as.character(unique(get(input$data)$data$id))
            }

            # default args

            def_args <- list(
              include = id_obj, exclude = NULL,
              line = list(join = TRUE, pred = FALSE),
              marker = TRUE,
              group = NULL,
              group_names = NULL,
              mult = 1,
              outeq = 1,
              out_names = NULL,
              block = 1,
              tad = FALSE,
              legend = ifelse(!is.null(input$data_group) && input$data_group != "none", TRUE, FALSE),
              log = FALSE,
              grid = FALSE,
              xlab = NULL, ylab = NULL,
              title = "",
              stats = TRUE,
              xlim = NULL, ylim = NULL
            )

            arglist <- args[-1] # remove the data object
            if (incl_pred != "none") { # replace pred object with its name if there
              if (inherits(arglist$line$pred, c("PM_pop", "PM_post"))) {
                arglist$line$pred <- paste(input$data, incl_pred, sep = "$")
              } else {
                arglist$line$pred[[1]] <- paste(input$data, incl_pred, sep = "$")
              }
            }
            arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))] # remove args that are in default list
            if (length(arglist) > 0) {
              arglist <- paste(deparse(arglist), collapse = "") %>%
                stringr::str_replace("^list\\(", "") %>%
                stringr::str_replace_all("(\\d+)L", "\\1") %>%
                stringr::str_replace_all(" +", " ")
              codeStatement <- paste0(Name, arglist)
            } else {
              codeStatement <- paste0(Name, ")")
            }

            codeStatement <- stringr::str_replace(
              codeStatement,
              "pred = \"(\\S+)\"",
              "pred = \\1"
            )

            codeStatement <- stringr::str_replace(
              codeStatement,
              "pred = list\\(\"(\\S+)\"",
              "pred = list\\(\\1"
            )

            p <- do_plot(args)
            if (code) {
              return(codeStatement)
            } else {
              return(p)
            }
          } # end PM_data
        } # end check for PM_result

        ############### Plot and Code: PM_sim #####################

        if (!is.null(input$data) && inherits(get(input$data), "PM_sim")) {
          # x argument
          x <- get(input$data)

          # line argument
          sim_line <- setVal("sim_line", TRUE)
          def_sim_line_fmt <- setVal("def_sim_line_fmt", TRUE)
          def_sim_args <- list(
            ci = 0.95,
            sim_probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
            color = "dodgerblue", width = 1, dash = "solid"
          )
          sim_ci <- setVal("sim_ci", def_sim_args$ci)
          sim_probs <- setVal("sim_probs", def_sim_args$sim_probs)
          sim_col <- setVal("sim_col", def_sim_args$color)
          sim_lwd <- setVal("sim_lwd", def_sim_args$width)
          sim_dash <- setVal("sim_dash", def_sim_args$dash)

          if (sim_line) { # yes have line
            if (!def_sim_line_fmt) { # not default format?
              line <- list(probs = as.numeric(sim_probs), color = sim_col, width = sim_lwd, dash = sim_dash)
              line <- line[!line %in% def_sim_args] # keep only non-defaults
              if (length(line) == 0) line <- TRUE
            } else { # default format
              line <- list()
            }
          } else { # don't have line
            line <- FALSE
          }


          # marker argument
          sim_obs <- setVal("sim_obs", NULL)
          obs_name <- ""
          if (!is.null(sim_obs)) {
            if (sim_obs == "None") {
              sim_obs <- NULL
            } else {
              obs_name <- sim_obs
              sim_obs <- get(sim_obs)
            }
          }


          def_sim_marker_fmt <- setVal("def_sim_marker_fmt", TRUE)
          def_marker_args <- list(
            color = "black", symbol = "circle-open", size = 8, opacity = 0.5,
            line = list(width = 1, color = "black")
          )

          mrk_col <- parse_marker_color_scalar(def_marker_args$color)
          mrk_symbol <- parse_marker_symbol_scalar(def_marker_args$symbol)
          mrk_size <- setVal("mrk_size", def_marker_args$size)
          mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
          mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
          mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)

          if (!is.null(sim_obs)) {
            if (def_sim_marker_fmt) {
              marker <- TRUE
            } else {
              marker <- list(
                color = mrk_col, symbol = mrk_symbol, size = mrk_size,
                opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol)
              )
              marker$line <- marker$line[!marker$line %in% def_marker_args$line] # keep only non-defaults
              if (length(marker$line) == 0) marker$line <- NULL
              marker <- marker[!marker %in% def_marker_args]
              if (length(marker) == 0) marker <- NULL
            }
          } else {
            marker <- FALSE
          }

          # title argument
          def_title_fmt <- setVal("def_title_fmt", TRUE)
          def_title_args <- list(text = "", font = list(color = "black", size = 20, bold = TRUE))
          title_text <- setVal("title_text", "")
          title_col <- setVal("title_col", "black")
          title_bold <- setVal("title_bold", TRUE)
          title_size <- setVal("title_size", 20)
          if (def_title_fmt) {
            title <- ""
          } else {
            title <- list(text = title_text, font = list(
              color = title_col, size = title_size,
              bold = title_bold
            ))
            title$font <- title$font[!title$font %in% def_title_args$font] # keep only non-defaults
            if (length(title$font) == 0) title$font <- NULL
          }

          # Other defaults
          log <- setVal("log", TRUE)
          grid <- setVal("grid", TRUE)
          mult <- setVal("mult", 1)
          outeq <- setVal("outeq", 1)
          outeq <- as.numeric(outeq)
          if (length(outeq) == 0) {
            outeq <- 1
          } else {
            outeq <- outeq[[1]]
          }
          binSize <- setVal("binSize", 0)



          args <- list(
            x = x,
            mult = as.numeric(mult),
            ci = as.numeric(sim_ci),
            binSize = as.numeric(binSize),
            outeq = outeq,
            line = line,
            marker = marker,
            obs = sim_obs,
            log = log,
            grid = grid,
            title = title,
            xlab = getXlab(), ylab = getYlab(),
            xlim = getXlim(), ylim = getYlim()
          )



          args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]

          # now make the code
          Name <- paste0(input$data, "$plot(")
          # default args
          def_args <- list(
            mult = 1,
            ci = 0.95,
            binSize = 0,
            outeq = 1,
            line = TRUE,
            marker = ifelse(is.null(sim_obs), FALSE, TRUE),
            obs = NULL,
            log = TRUE,
            grid = TRUE,
            xlab = NULL, ylab = NULL,
            title = "",
            xlim = NULL, ylim = NULL
          )

          arglist <- args[-1] # remove the data object
          if (!is.null(arglist$obs)) {
            arglist$obs <- obs_name
          }
          arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))] # remove args that are in default list

          if (length(arglist) > 0) {
            arglist <- paste(deparse(arglist), collapse = "") %>%
              stringr::str_replace("^list\\(", "") %>%
              stringr::str_replace_all("(\\d+)L", "\\1") %>%
              stringr::str_replace_all(" +", " ")
            codeStatement <- paste0(Name, arglist)
          } else {
            codeStatement <- paste0(Name, ")")
          }



          codeStatement <- stringr::str_replace(
            codeStatement,
            "obs = \"(\\S+)\"",
            "obs = \\1"
          )

          p <- do_plot(args)
          if (code) {
            return(codeStatement)
          } else {
            return(p$p)
          }
        } # end PM_sim

        ############### Plot and Code: PM_model & PM_result$model #####################

        if (!is.null(input$data) &&
          (inherits(get(input$data), "PM_model") |
            (inherits(get(input$data), "PM_result") &&
              !is.null(input$res_sub) && input$res_sub == "mod"))) {
          # x argument
          if (inherits(get(input$data), "PM_result")) {
            x <- get(input$data)$model
          }

          if (inherits(get(input$data), "PM_model")) {
            x <- get(input$data)
          }

          # line argument
          join <- setVal("join", TRUE)
          def_join_fmt <- setVal("def_join_fmt", TRUE)
          def_join_args <- list(color = "black", width = 1, dash = "solid")
          join_col <- setVal("join_col", def_join_args$color)
          join_lwd <- setVal("join_lwd", def_join_args$width)
          join_dash <- setVal("join_dash", def_join_args$dash)

          if (join) { # yes have join
            if (!def_join_fmt) { # not default format?
              line <- list(color = join_col, width = join_lwd, dash = join_dash)
              line <- line[!line %in% def_join_args] # keep only non-defaults
              if (length(line) == 0) line <- TRUE
            } else { # default format
              line <- list() # we have line but don't need with default format and model plot
            }
          } else { # don't have join
            line <- FALSE
          }

          # marker argument
          def_marker_fmt <- setVal("def_marker_fmt", TRUE)
          def_marker_args <- list(
            color = "dodgerblue", symbol = "circle", size = 0.25, opacity = 0.5,
            line = list(width = 1, color = "black")
          )

          mrk_col <- parse_marker_color_scalar(def_marker_args$color)
          mrk_symbol <- parse_marker_symbol_scalar(def_marker_args$symbol)
          mrk_size <- setVal("mrk_size", def_marker_args$size)
          mrk_opacity <- setVal("mrk_opacity", def_marker_args$opacity)
          mrk_lwd <- setVal("mrk_lwd", def_marker_args$line$width)
          mrk_lcol <- setVal("mrk_lcol", def_marker_args$line$color)

          if (def_marker_fmt) {
            marker <- TRUE
          } else {
            marker <- list(
              color = mrk_col, symbol = mrk_symbol, size = mrk_size,
              opacity = mrk_opacity, line = list(width = mrk_lwd, color = mrk_lcol)
            )
            marker$line <- marker$line[!marker$line %in% def_marker_args$line] # keep only non-defaults
            if (length(marker$line) == 0) marker$line <- NULL
            marker <- marker[!marker %in% def_marker_args]
            if (length(marker) == 0) marker <- NULL
          }

          # Other defaults
          args <- list(
            x = x,
            line = line, marker = marker
          )


          args <- args[which(sapply(args, function(x) length(x) > 0), arr.ind = T)]



          # now make the code
          if (inherits(get(input$data), "PM_result")) {
            Name <- paste0(input$data, "$model$plot(")
          }

          if (inherits(get(input$data), "PM_model")) {
            Name <- paste0(input$data, "$plot(")
          }


          # default args

          def_args <- list(
            line = TRUE,
            marker = TRUE
          )

          arglist <- args[-1] # remove the data object
          arglist <- arglist[map_lgl(intersect(names(arglist), names(def_args)), \(x) !identical(arglist[[x]], def_args[[x]]))] # remove args that are in default list
          if (length(arglist) > 0) {
            arglist <- paste(deparse(arglist), collapse = "") %>%
              stringr::str_replace("^list\\(", "") %>%
              stringr::str_replace_all("(\\d+)L", "\\1") %>%
              stringr::str_replace_all(" +", " ")
            codeStatement <- paste0(Name, arglist)
          } else {
            codeStatement <- paste0(Name, ")")
          }

          p <- do_plot(args)
          if (code) {
            return(codeStatement)
          } else {
            return(p)
          }
        } # end PM_model

        return(NULL) # plot method not defined
      } # end makePMplot



      ####################################################
      ############### Build the Page #####################
      ####################################################

      output$help <- renderText({
        obj <- current_data_obj()
        req(!is.null(obj))

        if (inherits(obj, "PM_cov")) {
          return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PM_cov</span>.")
        }
        if (inherits(obj, "PM_final")) {
          return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PM_final</span>.")
        }
        if (inherits(obj, "PM_op")) {
          return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PM_op</span>.")
        }
        if (inherits(obj, "PM_cycle")) {
          return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PM_cycle</span>.")
        }
        if (inherits(obj, "PM_data")) {
          return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PM_data</span>.")
        }
        if (inherits(obj, "PM_sim")) {
          return("To get help with this plot in R, type <span style=\"font-family:'Courier',Courier,monospace\">?plot.PM_sim</span>.")
        }
      })

      # Set up the inputs
      output$DataControls <- renderUI({
        makeDataControls()
      })
      output$FormattingPanel <- renderUI({
        obj <- current_data_obj()
        if (is.null(obj)) {
          return(NULL)
        }

        is_model_plot <-
          inherits(obj, "PM_model") ||
          (inherits(obj, "PM_result") &&
            !is.null(input$res_sub) && input$res_sub == "mod")

        if (is_model_plot) {
          return(NULL)
        }

        bslib::accordion_panel(
          "Formatting",
          uiOutput("FormatControls")
        )
      })
      output$AxesPanel <- renderUI({
        obj <- current_data_obj()
        if (is.null(obj)) {
          return(NULL)
        }

        is_model_plot <-
          inherits(obj, "PM_model") ||
          (inherits(obj, "PM_result") &&
            !is.null(input$res_sub) && input$res_sub == "mod")

        if (is_model_plot) {
          return(NULL)
        }

        bslib::accordion_panel(
          "Axes",
          uiOutput("AxesControls")
        )
      })
      output$FormatControls <- renderUI({
        makeFormatControls()
      })
      output$AxesControls <- renderUI({
        makeAxisControls()
      })


      plot_code <- reactive({
        makePMplot(code = TRUE)
      })

      # Build the Pmetrics plot statements
      output$plotCode <- renderText({
        plot_code()
      })

      observeEvent(input$copy_exit_btn, {
        code <- plot_code()
        if (requireNamespace("clipr", quietly = TRUE)) {
          clipr::write_clip(code)
          shiny::stopApp()
        } else {
          shiny::showNotification(
            "Install the 'clipr' package to enable clipboard copy.",
            type = "warning",
            duration = 5
          )
        }
      })

      observeEvent(input$exit_btn, {
        shiny::stopApp()
      })

      is_base_plot <- reactive({
        obj <- current_data_obj()
        !is.null(obj) &&
          (inherits(obj, "PM_model") ||
            (inherits(obj, "PM_result") &&
              !is.null(input$res_sub) && input$res_sub == "mod"))
      })

      output$plotPM <- renderUI({
        if (isTRUE(is_base_plot())) {
          plotOutput("plotPM_base")
        } else {
          plotly::plotlyOutput("plotPM_plotly")
        }
      })

      output$plotPM_base <- renderPlot({
        req(isTRUE(is_base_plot()))
        makePMplot()
      })

      output$plotPM_plotly <- plotly::renderPlotly({
        req(!isTRUE(is_base_plot()))
        p <- makePMplot()
        req(inherits(p, "plotly"))
        p
      })
    }

shiny::shinyApp(ui = ui, server = server)
