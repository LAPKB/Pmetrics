pkg_ns <- asNamespace("Pmetrics")
mod_list <- get("mod_list", envir = pkg_ns)
get_model_library_entry <- get("get_model_library_entry", envir = pkg_ns)
func_to_char <- get("func_to_char", envir = pkg_ns)
generate_model_code_text <- get("generate_model_code_text", envir = pkg_ns)

model_names <- purrr::map_chr(mod_list, \(x) x$name)

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    primary = "#2c3e50",
    "card-border-radius" = "0.5rem"
  ),
  title = "Pmetrics Model Library",
  shiny::tags$div(
    class = "container-fluid p-4",
    shiny::tags$div(
      class = "mb-4",
      shiny::tags$h2(
        class = "mb-1",
        shiny::icon("book-open", class = "me-2"),
        "Pmetrics Model Library"
      ),
      shiny::tags$p(
        class = "text-muted mb-0",
        "Browse and copy pharmacokinetic model templates"
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        bslib::card(
          class = "mb-3",
          bslib::card_header(
            class = "bg-primary text-white",
            shiny::icon("list", class = "me-2"),
            "Model Templates"
          ),
          bslib::card_body(
            class = "p-2",
            shiny::textInput(
              "model_search",
              NULL,
              value = "",
              placeholder = "Search primary or alt names"
            ),
            shiny::selectInput(
              "model_name",
              NULL,
              choices = model_names,
              size = length(model_names),
              selectize = FALSE,
              width = "100%"
            )
          )
        ),
        bslib::card(
          class = "mb-3",
          bslib::card_header(
            class = "bg-primary text-white",
            shiny::icon("filter", class = "me-2"),
            "How to Use"
          ),
          bslib::card_body(
            shiny::tags$ul(
              class = "small ps-3 mb-3",
              shiny::tags$ol(
                class = "ps-3",
                shiny::tags$li("Select a model from the list to view its details."),
                shiny::tags$li("Click the button below to copy template code for the selected model and close the app."),
                shiny::tags$li(
                  "Paste the copied code into your script, then edit as needed for your analysis.",
                  shiny::tags$details(
                    shiny::tags$summary(
                      shiny::tags$strong("Editing tips after pasting in R script")
                    ),
                    shiny::tags$ul(
                      class = "mt-1",
                      shiny::tags$li(
                        "Parameter names and ranges in the ",
                        shiny::tags$code("PRI"),
                        " block can be changed."
                      ),
                      shiny::tags$li(
                        shiny::tags$strong("However"),
                        ", if you change names in the ",
                        shiny::tags$code("PRI"),
                        " block, ensure the original variable(s) are defined in a ",
                        shiny::tags$code("SEC"),
                        " block, e.g., if you change ",
                        shiny::tags$code("CL"),
                        " to ",
                        shiny::tags$code("CL1"),
                        " in the ",
                        shiny::tags$code("PRI"),
                        " block, include a ",
                        shiny::tags$code("SEC"),
                        " block with something like ",
                        shiny::tags$code("CL = CL1 * wt"),
                        "."
                      ),
                      shiny::tags$li(
                        "Add additional parameters for use in ",
                        shiny::tags$code("LAG"),
                        ", ",
                        shiny::tags$code("FA"),
                        " or ",
                        shiny::tags$code("INI"),
                        " blocks after the other parameters in the ",
                        shiny::tags$code("PRI"),
                        " block."
                      )
                    )
                  )
                )
              )
            ),
            shiny::actionButton(
              "copy_btn",
              shiny::tags$span(
                shiny::icon("clipboard", class = "me-1"),
                "Copy and Close"
              ),
              class = "btn-success btn-sm"
            )
          )
        ),
        bslib::card(
          class = "mb-3",
          bslib::card_header(
            class = "bg-primary text-white",
            shiny::icon("circle-info", class = "me-2"),
            "Notes"
          ),
          bslib::card_body(
            shiny::tags$ul(
              class = "small ps-3 mb-3",
              shiny::tags$li(
                "By common convention, names exclude the bolus compartment, e.g., ",
                shiny::tags$code("two_comp_bolus"),
                "has 3 compartments: bolus, central, and peripheral."
              ),
              shiny::tags$li(
                "Parameter ranges are general and may be appropriate for your model. Adjust as needed."
              ),
              shiny::tags$li(
                "Each primary name is also a function in R that will copy the model code to the clipboard, e.g., type ",
                shiny::tags$code("one_comp_iv()"),
                " in the R console or call it from a script."
              )
            )
          )
        )
      ),
      shiny::column(
        width = 8,
        bslib::card(
          class = "mb-3",
          bslib::card_header(
            class = "bg-primary text-white",
            shiny::icon("circle-info", class = "me-2"),
            "Overview"
          ),
          bslib::card_body(shiny::uiOutput("model_overview"))
        ),
        bslib::card(
          class = "mb-3",
          bslib::card_header(
            class = "bg-primary text-white",
            shiny::icon("infinity", class = "me-2"),
            "Equations"
          ),
          bslib::card_body(shiny::uiOutput("model_equations"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_model_names <- shiny::reactive({
    q <- trimws(if (is.null(input$model_search)) "" else input$model_search)

    if (identical(q, "")) {
      return(model_names)
    }

    keep <- purrr::map_lgl(mod_list, \(m) {
      grepl(q, m$name, ignore.case = TRUE) ||
        any(grepl(q, m$alt_names, ignore.case = TRUE))
    })

    purrr::map_chr(mod_list[keep], \(x) x$name)
  })

  shiny::observe({
    choices <- filtered_model_names()
    selected_now <- input$model_name

    if (length(choices) == 0) {
      shiny::updateSelectInput(
        session,
        "model_name",
        choices = character(0),
        selected = character(0)
      )
    } else {
      selected_new <- if (!is.null(selected_now) && selected_now %in% choices) {
        selected_now
      } else {
        choices[[1]]
      }

      shiny::updateSelectInput(
        session,
        "model_name",
        choices = choices,
        selected = selected_new
      )
    }
  })

  selected <- shiny::reactive({
    shiny::req(input$model_name)
    get_model_library_entry(input$model_name)
  })

  output$model_overview <- shiny::renderUI({
    m <- selected()
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::tags$h6(shiny::tags$strong(
          shiny::icon("tag", class = "me-1 text-primary"), "Primary Name"
        )),
        shiny::tags$p(shiny::tags$code(m$name)),
        shiny::tags$h6(shiny::tags$strong(
          shiny::icon("circle-nodes", class = "me-1 text-primary"), "Type"
        )),
        shiny::tags$p(if (isTRUE(m$analytical)) "Analytic" else "ODE"),
        shiny::tags$h6(shiny::tags$strong(
          shiny::icon("tags", class = "me-1 text-primary"), "Alt Names"
        )),
        shiny::tags$p(
          class = "text-muted",
          if (length(m$alt_names) == 0 || all(is.na(m$alt_names))) "—"
          else paste(m$alt_names, collapse = ", ")
        ),
        shiny::tags$h6(shiny::tags$strong(
          shiny::icon("align-left", class = "me-1 text-primary"), "Description"
        )),
        shiny::tags$ul(purrr::map(m$description, shiny::tags$li))
      ),
      shiny::column(
        width = 6,
        shiny::tags$h6(shiny::tags$strong(
          shiny::icon("diagram-project", class = "me-1 text-primary"), "Compartments"
        )),
        shiny::tags$ul(purrr::map(m$compartments, shiny::tags$li)),
        shiny::tags$h6(shiny::tags$strong(
          shiny::icon("sliders", class = "me-1 text-primary"), "Parameters"
        )),
        shiny::tags$table(
          class = "table table-sm table-bordered table-hover",
          style = "font-size: 0.85rem;",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Name"),
              shiny::tags$th("Min"),
              shiny::tags$th("Max")
            )
          ),
          shiny::tags$tbody(
            purrr::imap(m$arg_list$pri, \(p, nm) {
              shiny::tags$tr(
                shiny::tags$td(shiny::tags$code(nm)),
                shiny::tags$td(p$min),
                shiny::tags$td(p$max)
              )
            })
          )
        )
      )
    )
  })

  output$model_equations <- shiny::renderUI({
    m <- selected()
    ode_lines <- func_to_char(m$arg_list$eqn)
    out_lines <- func_to_char(m$arg_list$out)

    shiny::tags$div(
      shiny::tags$h6(shiny::tags$strong(
        shiny::icon("arrows-rotate", class = "me-1 text-primary"),
        "Differential Equations"
      )),
      if (isTRUE(m$analytical)) {
        shiny::tags$p(
          class = "text-muted small fst-italic mb-2",
          "Shown for illustrative purposes only. Model is solved algebraically."
        )
      },
      shiny::tags$pre(
        class = "bg-light p-2 rounded border",
        style = "font-size: 0.82rem;",
        paste(ode_lines, collapse = "\n")
      ),
      shiny::tags$h6(
        class = "mt-3",
        shiny::tags$strong(
          shiny::icon("arrow-right-from-bracket", class = "me-1 text-primary"),
          "Output Equations"
        )
      ),
      shiny::tags$pre(
        class = "bg-light p-2 rounded border",
        style = "font-size: 0.82rem;",
        paste(out_lines, collapse = "\n")
      )
    )
  })

  model_code_text <- shiny::reactive({
    generate_model_code_text(selected())
  })

  shiny::observeEvent(input$copy_btn, {
    code <- model_code_text()
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
}

shiny::shinyApp(ui = ui, server = server)
