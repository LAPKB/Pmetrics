
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

# ---- Example defaults ----
default_params <- tibble::tibble(
  param    = c("Km","Vc","Q","Vp","Ka","Fa","CL"),
  theta    = c(3030, 0.807, 0.609, 2.17, 0.849, 44.6, 0.582),
  rse_theta= c(45,   14,    13,    11,   40,    14,   19),
  cv_iiv   = c(131,  0,     0,     0,    0,     69.7, 52.8),
  rse_iiv  = c(31,   0,     0,     0,    0,     41,   48),
  min      = c(0,    0,     0,     0,    0,     0,    0),
  max      = c(Inf,  Inf,   Inf,   Inf,  Inf,   Inf,  Inf)
)

default_corr_edges <- tibble::tibble(
  i   = c("Km","Fa","Km"),
  j   = c("CL","CL","Fa"),
  rho = c(-0.685, 0.66, -0.646)
)

# ---- Helpers ----
build_correlation <- function(params, edges) {
  p <- length(params)
  R <- diag(1, p)
  dimnames(R) <- list(params, params)
  if (!is.null(edges) && nrow(edges) > 0) {
    edges2 <- edges |>
      mutate(i = as.character(i),
             j = as.character(j)) |>
      filter(i %in% params, j %in% params, i != j) |>
      mutate(rho = pmax(pmin(as.numeric(rho), 0.9999), -0.9999))
    for (k in seq_len(nrow(edges2))) {
      ii <- edges2$i[k]
      jj <- edges2$j[k]
      r <- edges2$rho[k]
      R[ii, jj] <- r
      R[jj, ii] <- r
    }
  }
  R
}

draw_omega <- function(n, cv_percent, rse_iiv_percent) {
  if (is.na(cv_percent) || is.na(rse_iiv_percent) || cv_percent <= 0) {
    return(rep(0, n))
  }
  cv <- cv_percent / 100
  se_cv <- (rse_iiv_percent / 100) * cv_percent
  sdlog <- sqrt(log(1 + (se_cv / cv_percent)^2))
  rlnorm(n, meanlog = log(cv), sdlog = sdlog)
}

make_omega_cov <- function(omega_vec, R) {
  D <- diag(omega_vec, nrow = length(omega_vec))
  Omega <- D %*% R %*% D
  Omega <- (Omega + t(Omega)) / 2
  if (all(abs(Omega) < .Machine$double.eps^0.5)) return(Omega)
  npd <- tryCatch(nearPD(Omega, corr = FALSE), error = function(e) NULL)
  if (!is.null(npd)) {
    as.matrix((npd$mat + t(npd$mat))/2)
  } else {
    as.matrix(Omega + diag(1e-12, nrow(Omega)))
  }
}

sample_etas <- function(n, theta_names, omega_samples_by_param, R) {
  p <- length(theta_names)
  out <- matrix(0, nrow = n, ncol = p)
  colnames(out) <- paste0("eta_", theta_names)
  for (i in 1:n) {
    omegas <- omega_samples_by_param[i, , drop = TRUE]
    if (all(omegas == 0)) {
      next
    } else {
      Omega <- make_omega_cov(omegas, R)
      eta_i <- tryCatch(
        MASS::mvrnorm(n = 1, mu = rep(0, p), Sigma = Omega),
        error = function(e) {
          Omega2 <- as.matrix(Omega + diag(1e-10, p))
          MASS::mvrnorm(n = 1, mu = rep(0, p), Sigma = Omega2)
        }
      )
      zero_idx <- which(omegas == 0)
      if (length(zero_idx)) eta_i[zero_idx] <- 0
      out[i, ] <- eta_i
    }
  }
  as_tibble(out)
}

sample_thetas <- function(n, params_tbl) {
  # Ensure min/max columns exist with defaults
  if (!"min" %in% names(params_tbl)) {
    params_tbl$min <- 0
  }
  if (!"max" %in% names(params_tbl)) {
    params_tbl$max <- Inf
  }
  purrr::pmap_dfc(
    list(params_tbl$param, params_tbl$theta, params_tbl$rse_theta, 
         params_tbl$min, params_tbl$max),
    function(par, th, rse, min_val, max_val) {
      # Handle NA/NULL for min/max
      min_val <- ifelse(is.na(min_val), -Inf, as.numeric(min_val))
      max_val <- ifelse(is.na(max_val), Inf, as.numeric(max_val))
      
      cv <- ifelse(is.na(rse) || rse == 0, 0, rse / 100)
      sd <- cv * abs(th)
      
      # Sample with rejection to enforce bounds
      samples <- numeric(n)
      remaining <- seq_len(n)
      max_attempts <- 100
      attempt <- 0
      
      while (length(remaining) > 0 && attempt < max_attempts) {
        attempt <- attempt + 1
        new_samples <- stats::rnorm(length(remaining), mean = th, sd = sd)
        valid <- new_samples >= min_val & new_samples <= max_val
        samples[remaining[valid]] <- new_samples[valid]
        remaining <- remaining[!valid]
      }
      
      # If still remaining after max attempts, clamp to bounds
      if (length(remaining) > 0) {
        samples[remaining] <- pmax(pmin(stats::rnorm(length(remaining), mean = th, sd = sd), max_val), min_val)
      }
      
      tibble::tibble(!!paste0("theta_", par) := samples)
    }
  )
}

sample_omegas <- function(n, params_tbl) {
  ome <- purrr::pmap(
    list(params_tbl$cv_iiv, params_tbl$rse_iiv),
    ~ draw_omega(n, ..1, ..2)
  )
  names(ome) <- paste0("omega_", params_tbl$param)
  dplyr::bind_cols(ome)
}

compose_params <- function(theta_df, eta_df, param_names, params_tbl = NULL) {
  out <- tibble::tibble(.rows = nrow(theta_df))
  for (par in param_names) {
    tcol <- paste0("theta_", par)
    ecol <- paste0("eta_", par)
    if (!ecol %in% names(eta_df)) {
      out[[par]] <- theta_df[[tcol]]
    } else {
      out[[par]] <- theta_df[[tcol]] * exp(eta_df[[ecol]])
    }
    
    # Apply min/max constraints to final composed values
    if (!is.null(params_tbl) && par %in% params_tbl$param) {
      par_row <- params_tbl[params_tbl$param == par, ]
      min_val <- if ("min" %in% names(par_row)) par_row$min else 0
      max_val <- if ("max" %in% names(par_row)) par_row$max else Inf
      min_val <- ifelse(is.na(min_val), -Inf, min_val)
      max_val <- ifelse(is.na(max_val), Inf, max_val)
      out[[par]] <- pmax(pmin(out[[par]], max_val), min_val)
    }
  }
  out
}

maybe_transpose <- function(df, transpose = FALSE) {
  if (!transpose) return(df)
  as_tibble(t(df), rownames = "parameter") |>
    rename_with(~"parameter", 1) |>
    mutate(parameter = as.character(parameter))
}

# ---- UI ----
# Static assets are served from app-local `www/` when launched via shiny::runApp(appDir)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Modern styling */
      body { 
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
        background-color: #f8f9fa;
      }
  
      .well {
        background-color: #ffffff;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      }
      h4 { 
        color: #495057;
        font-weight: 600;
        margin-top: 0;
        padding-bottom: 8px;
        border-bottom: 2px solid #e9ecef;
      }
      .form-group { margin-bottom: 12px; }
      
      /* DataTable styling */
      .dataTable input {
        color: black !important;
        font-weight: bold;
      }
      .dataTables_wrapper { font-size: 13px; }
      
      /* Button improvements */
      .btn { 
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      .btn-sm { padding: 4px 12px; font-size: 12px; }
      .btn-primary { background-color: #0d6efd; border-color: #0d6efd; }
      .btn-primary:hover { background-color: #0b5ed7; }
      .btn-success { background-color: #198754; border-color: #198754; }
      .btn-info { background-color: #0dcaf0; border-color: #0dcaf0; color: #000; }
      .btn-danger { background-color: #dc3545; border-color: #dc3545; }
      
      /* Compact radio buttons */
      .radio-inline { margin-right: 15px; }
      .shiny-input-container:not(.shiny-input-container-inline) { margin-bottom: 10px; }
      
      /* Section cards */
      .section-card {
        background: #fff;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
      .section-title {
        font-size: 14px;
        font-weight: 600;
        color: #495057;
        margin-bottom: 12px;
      }
      
      /* Help text styling */
      .help-block { 
        font-size: 11px; 
        color: #6c757d; 
        margin-top: 4px;
        margin-bottom: 8px;
      }
      
      /* Format info panel */
      .format-info {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        padding: 12px;
        margin-top: 8px;
        font-size: 11px;
      }
      .format-info code {
        background-color: #e9ecef;
        padding: 1px 4px;
        border-radius: 3px;
        font-size: 10px;
      }
      .format-info pre {
        background-color: #e9ecef;
        padding: 8px;
        border-radius: 4px;
        font-size: 10px;
        margin: 8px 0 0 0;
      }
      .format-info ul { 
        padding-left: 18px; 
        margin: 4px 0;
      }
      .format-info li { margin-bottom: 2px; }
      
      /* Info link styling */
      .info-link {
        color: #6c757d;
        font-size: 11px;
        text-decoration: none;
      }
      .info-link:hover { color: #0d6efd; }
      
      /* Button row spacing */
      .btn-row { margin-top: 10px; }
      .btn-row .btn { margin-right: 5px; margin-bottom: 5px; }
      
      /* Main panel tabs */
      .nav-tabs > li > a { 
        border-radius: 6px 6px 0 0;
        font-weight: 500;
      }
      .tab-content { 
        background: #fff;
        border: 1px solid #dee2e6;
        border-top: none;
        border-radius: 0 0 8px 8px;
        padding: 20px;
      }
      
      /* Checkbox and input sizing */
      .checkbox label { font-size: 13px; }
      
      /* Horizontal rule */
      hr { border-color: #e9ecef; margin: 15px 0; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleCondition', function(message) {
        Shiny.setInputValue(message.name, message.value);
      });
      
      // Enable Tab/Shift-Tab navigation in editable DataTables
      $(document).on('keydown', '.dataTable input', function(e) {
        if (e.key === 'Tab') {
          e.preventDefault();
          var $input = $(this);
          var $cell = $input.closest('td');
          var $row = $cell.closest('tr');
          var $table = $row.closest('table');
          var cellIndex = $cell.index();
          var rowIndex = $row.index();
          var $allRows = $table.find('tbody tr');
          var numCols = $row.find('td').length;
          var numRows = $allRows.length;
          
          $input.blur();
          
          setTimeout(function() {
            var newCellIndex, newRowIndex;
            
            if (e.shiftKey) {
              newCellIndex = cellIndex - 1;
              newRowIndex = rowIndex;
              if (newCellIndex < 0) {
                newCellIndex = numCols - 1;
                newRowIndex = rowIndex - 1;
                if (newRowIndex < 0) newRowIndex = numRows - 1;
              }
            } else {
              newCellIndex = cellIndex + 1;
              newRowIndex = rowIndex;
              if (newCellIndex >= numCols) {
                newCellIndex = 0;
                newRowIndex = rowIndex + 1;
                if (newRowIndex >= numRows) newRowIndex = 0;
              }
            }
            
            var $targetRow = $allRows.eq(newRowIndex);
            var $targetCell = $targetRow.find('td').eq(newCellIndex);
            $targetCell.trigger('dblclick');
          }, 50);
        }
      });
    "))
  ),
  
  titlePanel(
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
        div(
          "Sampling from Parametric Literature Models",
          tags$small(style = "color: #6c757d; font-size: 12px; display: block; margin-top: 2px;",
                     "Monte Carlo sampling with parameter uncertainty")
        ),
        img(src = "Pmetrics_logo.png", height = "60px", style = "margin-left: 15px;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # --- Simulation Settings ---
      div(class = "section-card",
          div(class = "section-title", icon("cog"), " Simulation Settings"),
          numericInput("n", "Samples:", value = 1000, min = 1, step = 100),
          actionLink("show_uncommon_settings", "Uncommon options...", class = "info-link"),
          uiOutput("uncommon_settings_ui")
      ),

      # --- Parameters Section ---
      div(class = "section-card",
          div(class = "section-title", icon("table"), " Parameters"),
          radioButtons("param_mode", NULL,
                       choices = c("Manual" = "manual", "Upload CSV" = "upload"),
                       inline = TRUE, selected = "manual"),
          fluidRow(
            column(6, radioButtons("iiv_input_type", "IIV:",
                       choices = c("CV%" = "cv", "ω²" = "omega2"),
                       inline = TRUE, selected = "cv")),
            column(6, radioButtons("rse_input_type", "Uncertainty:",
                       choices = c("RSE%" = "rse", "SE" = "se"),
                       inline = TRUE, selected = "rse"))
          ),
          
          conditionalPanel(
            condition = "input.param_mode == 'manual'",
            DT::DTOutput("param_editor"),
            div(class = "btn-row",
                actionButton("param_add_row", "Add", icon = icon("plus"), class = "btn-sm btn-outline-secondary"),
                actionButton("param_del_row", "Delete", icon = icon("trash"), class = "btn-sm btn-outline-danger"),
                #actionButton("save_params_csv", "Save", icon = icon("download"), class = "btn-sm btn-outline-success"),
                downloadButton("download_params_csv", "Save", icon = icon("download"), class = "btn-sm btn-outline-success")
            ),
            tags$small(
              actionLink("load_defaults", "Load example", style = "color: #6c757d;")
            )
          ),
          conditionalPanel(
            condition = "input.param_mode == 'upload'",
            fileInput("param_file", NULL, accept = ".csv", 
                      placeholder = "Choose CSV file..."),
            actionLink("show_param_format", "Show CSV format", class = "info-link"),
            uiOutput("param_format_info")
          )
      ),

      # --- Correlations Section ---
      div(class = "section-card",
          div(class = "section-title", icon("project-diagram"), " ETA Correlations ", 
              tags$small("(optional)", style = "color: #adb5bd; font-weight: normal;")),
          radioButtons("corr_mode", NULL,
                       choices = c("Manual" = "manual", "Upload CSV" = "upload"),
                       inline = TRUE, selected = "manual"),
          
          conditionalPanel(
            condition = "input.corr_mode == 'manual'",
            fluidRow(
              column(4, selectInput("edge_param_i", NULL, choices = NULL, selectize = FALSE)),
              column(4, selectInput("edge_param_j", NULL, choices = NULL, selectize = FALSE)),
              column(4, numericInput("edge_rho", NULL, value = 0, min = -1, max = 1, step = 0.05))
            ),
            actionButton("edge_add_btn", "Add", icon = icon("plus"), class = "btn-sm btn-info"),
            tags$hr(style = "margin: 10px 0;"),
            DT::DTOutput("edges_editor"),
            div(class = "btn-row",
                actionButton("edges_del_row", "Delete", icon = icon("trash"), class = "btn-sm btn-outline-danger"),
                actionButton("save_corr_csv", "Save", icon = icon("download"), class = "btn-sm btn-outline-success")
            ),
            tags$small(
              actionLink("load_corr_defaults", "Load example", style = "color: #6c757d;")
            )
          ),
          conditionalPanel(
            condition = "input.corr_mode == 'upload'",
            fileInput("corr_file", NULL, accept = ".csv",
                      placeholder = "Choose CSV file..."),
            actionLink("show_corr_format", "Show CSV format", class = "info-link"),
            uiOutput("corr_format_info")
          )
      ),

      # --- Simulate Button ---
      div(style = "text-align: center; margin-top: 10px;",
          actionButton("simulate", "Simulate", icon = icon("play"), 
                       class = "btn-primary btn-lg", style = "width: 100%;")
      )
    ),

    mainPanel(
      width = 8,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = tagList(icon("table"), " Samples"),
          br(),
          DT::DTOutput("sample_table"),
          br(),
          fluidRow(
            column(4, actionButton("save_csv_btn", "Save as CSV", icon = icon("save"), class = "btn-success")),
            column(4, actionButton("copy_df_btn", "Copy as R Data Frame", icon = icon("clipboard"), class = "btn-info")),
            column(4, actionButton("exit_app_btn", "Exit", icon = icon("door-open"), class = "btn-danger"))
          )
        ),
        tabPanel(
          title = tagList(icon("chart-line"), " Histograms"),
          br(),
          fluidRow(
            column(3, numericInput("hist_bins", "Bins:", value = 30, min = 5, max = 200, step = 5))
          ),
          plotOutput("param_histograms", height = "auto")
        ),
        tabPanel(
          title = tagList(icon("stethoscope"), " Diagnostics"),
          br(),
          h5(HTML('Sampled <span style="color:#888; font-style:italic;">(requested)</span> Parameter Summary')),
          DT::DTOutput("sampled_summary"),
          br(),
          h5(HTML('Sampled <span style="color:#888; font-style:italic;">(requested)</span> ETA Correlation Matrix')),
          DT::DTOutput("sampled_corr_matrix")
        )
      )
    )
  )
)


# ---- Server ----
server <- function(input, output, session) {

  params_tbl <- reactiveVal(default_params)
  edges_tbl  <- reactiveVal(default_corr_edges)
  last_saved_path <- reactiveVal(file.path(getwd(), "theta.csv")) # Default path
  

  # Toggle visibility for format info panels
  show_param_format_visible <- reactiveVal(FALSE)
  show_corr_format_visible <- reactiveVal(FALSE)
  show_uncommon_settings <- reactiveVal(FALSE)
  
  observeEvent(input$show_param_format, {
    show_param_format_visible(!show_param_format_visible())
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_corr_format, {
    show_corr_format_visible(!show_corr_format_visible())
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_uncommon_settings, {
    show_uncommon_settings(!show_uncommon_settings())
  }, ignoreInit = TRUE)
  
  # Uncommon settings UI
  output$uncommon_settings_ui <- renderUI({
    req(show_uncommon_settings())
    div(style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid #e9ecef;",
        numericInput("seed", "Seed:", value = -17, min = -20, step = 1),
        checkboxInput("include_internals", "Include theta/eta columns", value = FALSE),
        checkboxInput("transpose_out", "Transpose output", value = FALSE)
    )
  })
  
  # Dynamic CSV format info based on IIV and Uncertainty selections
  output$param_format_info <- renderUI({
    req(show_param_format_visible())
    
    # Build column names based on selections
    iiv_col <- if (input$iiv_input_type == "omega2") "omega2_iiv" else "cv_iiv"
    theta_unc_col <- if (input$rse_input_type == "se") "se_theta" else "rse_theta"
    iiv_unc_col <- if (input$rse_input_type == "se") "se_iiv" else "rse_iiv"
    
    # Build example values
    example_iiv <- if (input$iiv_input_type == "omega2") "0.25" else "52.8"
    example_theta_unc <- if (input$rse_input_type == "se") "0.11" else "19"
    example_iiv_unc <- if (input$rse_input_type == "se") "0.12" else "48"
    
    example_line <- paste0("CL,0.582,", example_theta_unc, ",", example_iiv, ",", example_iiv_unc)
    
    div(class = "format-info",
        tags$strong("Required:"), paste(" param, theta,", theta_unc_col, ",", iiv_col, ",", iiv_unc_col),
        tags$br(),
        tags$strong("Optional:"), " min, max",
        tags$pre(paste0("param,theta,", theta_unc_col, ",", iiv_col, ",", iiv_unc_col, "\n", example_line))
    )
  })
  
  # Correlation CSV format info
  output$corr_format_info <- renderUI({
    req(show_corr_format_visible())
    
    div(class = "format-info",
        tags$strong("Required:"), " i, j, rho",
        tags$pre("i,j,rho\nKm,CL,-0.685")
    )
  })

  # --- PARAM editor (manual mode) ---
  output$param_editor <- DT::renderDT({
    df <- params_tbl()
    # Dynamic column names based on input types
    iiv_col_name <- if (input$iiv_input_type == "omega2") "IIV (ω²)" else "IIV (CV%)"
    theta_unc_name <- if (input$rse_input_type == "se") "Theta SE" else "Theta RSE%"
    iiv_unc_name <- if (input$rse_input_type == "se") "IIV SE" else "IIV RSE%"
    DT::datatable(
      df,
      colnames = c("Name", "Theta TV", theta_unc_name, iiv_col_name, iiv_unc_name, "Min", "Max"),
      selection = "multiple",
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c())), 
      options = list(dom = 'tip', scrollX = TRUE, pageLength = 8)
    )
  })

  observeEvent(input$param_editor_cell_edit, {
    info <- input$param_editor_cell_edit
    df <- params_tbl()
    r <- info$row 
    c <- info$col + 1
    val <- info$value
    num_cols <- c("theta", "rse_theta", "cv_iiv", "rse_iiv", "min", "max")
    if (names(df)[c] %in% num_cols) {
      # Handle "Inf" and "-Inf" strings
      val_str <- toupper(trimws(as.character(val)))
      if (val_str == "INF") {
        val <- Inf
      } else if (val_str == "-INF") {
        val <- -Inf
      } else {
        val <- suppressWarnings(as.numeric(val))
      }
    }
    df[r, c] <- val
    params_tbl(df)
  })

  observeEvent(input$param_add_row, {
    df <- params_tbl()
    new <- tibble::tibble(param = "New", theta = 0, rse_theta = 0, cv_iiv = 0, rse_iiv = 0, min = 0, max = Inf)
    params_tbl(dplyr::bind_rows(df, new))
  })

  observeEvent(input$param_del_row, {
    sel <- input$param_editor_rows_selected
    req(length(sel) > 0)
    df <- params_tbl()
    params_tbl(df[-sel, , drop = FALSE])
  })
  
  observe({
    p <- params_tbl()$param
    req(length(p) > 0)
    updateSelectInput(session, "edge_param_i", choices = p)
    updateSelectInput(session, "edge_param_j", choices = p, selected = if(length(p)>1) p[2] else p[1])
  })

  # --- EDGE editor (manual mode) ---
  output$edges_editor <- DT::renderDT({
    df <- edges_tbl()
    DT::datatable(
      df,
      colnames = c("Parameter i", "Parameter j", "Rho"),
      selection = "multiple",
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      options = list(dom = 't', scrollX = TRUE, pageLength = 6)
    )
  })

  observeEvent(input$edges_editor_cell_edit, {
    info <- input$edges_editor_cell_edit
    df <- edges_tbl()
    r <- info$row
    c <- info$col + 1
    val <- info$value
    if (names(df)[c] == "rho") {
      val <- suppressWarnings(as.numeric(val))
      df[r, c] <- val
      edges_tbl(df)
    }
  })

  observeEvent(input$edge_add_btn, {
    req(input$edge_param_i, input$edge_param_j, input$edge_rho)
    if(input$edge_param_i == input$edge_param_j) {
      showNotification("Parameter I and J cannot be the same.", type = "error")
      return()
    }
    df <- edges_tbl()
    is_match <- (df$i == input$edge_param_i & df$j == input$edge_param_j) |
                (df$i == input$edge_param_j & df$j == input$edge_param_i)
    if(any(is_match)) {
      df <- df[!is_match, ]
    }
    new_row <- tibble::tibble(
      i   = input$edge_param_i,
      j   = input$edge_param_j,
      rho = as.numeric(input$edge_rho)
    )
    edges_tbl(dplyr::bind_rows(df, new_row))
  })

  observeEvent(input$edges_del_row, {
    sel <- input$edges_editor_rows_selected
    req(length(sel) > 0)
    df <- edges_tbl()
    edges_tbl(df[-sel, , drop = FALSE])
  })

  observeEvent(input$load_defaults, { params_tbl(default_params) }, ignoreInit = TRUE)
  
  observeEvent(input$load_corr_defaults, {
    edges_tbl(default_corr_edges)
  }, ignoreInit = TRUE)
  
  # --- SAVE PARAMETERS CSV (downloadHandler) ---
  output$download_params_csv <- downloadHandler(
    filename = function() {
      "parameters.csv"
    },
    content = function(file) {
      readr::write_csv(params_tbl(), file)
    }
  )
  
  # --- SAVE CORRELATIONS CSV (downloadHandler) ---
  output$download_corr_csv <- downloadHandler(
    filename = function() {
      "correlations.csv"
    },
    content = function(file) {
      readr::write_csv(edges_tbl(), file)
    }
  )

  observeEvent(input$param_file, {
    req(input$param_file)
    df <- readr::read_csv(input$param_file$datapath, show_col_types = FALSE)

    # Expected column names based on current settings
    iiv_col <- if (input$iiv_input_type == "omega2") "omega2_iiv" else "cv_iiv"
    theta_unc_col <- if (input$rse_input_type == "se") "se_theta" else "rse_theta"
    iiv_unc_col <- if (input$rse_input_type == "se") "se_iiv" else "rse_iiv"

    need_cols <- c("param", "theta", theta_unc_col, iiv_col, iiv_unc_col)
    miss <- setdiff(need_cols, names(df))
    if (length(miss) > 0) {
      showNotification(
        paste0("Error: Missing columns in parameters CSV: ", paste(miss, collapse=", "),
               "\nExpected: ", paste(need_cols, collapse=", ")), type = "error", duration = 8
      )
      return()
    }

    # Rename to internal standard names
    df <- df |>
      rename(rse_theta = !!theta_unc_col, cv_iiv = !!iiv_col, rse_iiv = !!iiv_unc_col) |>
      mutate(param = as.character(param)) |>
      mutate(across(c(theta, rse_theta, cv_iiv, rse_iiv), as.numeric))

    # Add min/max columns if missing, with defaults
    if (!"min" %in% names(df)) {
      df$min <- 0
    } else {
      df$min <- as.numeric(df$min)
    }
    if (!"max" %in% names(df)) {
      df$max <- Inf
    } else {
      df$max <- as.numeric(df$max)
    }
    df <- df |> arrange(param)
    params_tbl(df)
    showNotification("Parameters updated from uploaded CSV.", type = "message", duration = 4)
    updateRadioButtons(session, "param_mode", selected = "manual")
  }, ignoreInit = TRUE)

  observeEvent(input$corr_file, {
    req(input$corr_file)
    df <- readr::read_csv(input$corr_file$datapath, show_col_types = FALSE)
    need_cols <- c("i","j","rho")
    miss <- setdiff(need_cols, names(df))
    validate(need(length(miss) == 0, paste("Missing columns in correlation edges CSV:", paste(miss, collapse=", "))))
    df <- df |>
      mutate(i = as.character(i),
             j = as.character(j),
             rho = as.numeric(rho))
    edges_tbl(df)
    updateRadioButtons(session, "corr_mode", selected = "manual")
  }, ignoreInit = TRUE)

  # Simulation Logic
  sim_result <- eventReactive(input$simulate, {
    n <- as.integer(input$n)
    validate(need(!is.na(n) && n > 0, "N must be a positive integer"))
    if (!is.null(input$seed) && !is.na(input$seed)) set.seed(as.integer(input$seed))

    par_df <- params_tbl()
    validate(need(nrow(par_df) > 0, "Parameter table is empty."))
    
    # Convert SE to RSE% if needed: RSE% = (SE / theta) * 100
    if (input$rse_input_type == "se") {
      par_df <- par_df |>
        mutate(
          rse_theta = ifelse(theta != 0 & !is.na(rse_theta), (rse_theta / abs(theta)) * 100, 0),
          rse_iiv = ifelse(cv_iiv != 0 & !is.na(rse_iiv), (rse_iiv / cv_iiv) * 100, 0)
        )
    }
    
    # Convert omega² to CV% if needed: CV = sqrt(exp(omega²) - 1) * 100
    if (input$iiv_input_type == "omega2") {
      par_df <- par_df |>
        mutate(cv_iiv = ifelse(cv_iiv > 0, sqrt(exp(cv_iiv) - 1) * 100, 0))
    }

    theta_df <- sample_thetas(n, par_df)
    omega_df <- sample_omegas(n, par_df)

    pname <- par_df$param
    R <- build_correlation(pname, edges_tbl())
    
    # Check if the requested correlation matrix is positive definite
    ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
    if (any(ev <= 0)) {
      min_ev <- min(ev)
      showNotification(
        paste0("Warning: The requested ETA correlation matrix is not positive definite ",
               "(min eigenvalue: ", round(min_ev, 4), "). ",
               "The matrix will be adjusted using nearPD() during sampling."),
        type = "warning",
        duration = 10
      )
    }

    omega_mat <- as.matrix(omega_df[, paste0("omega_", pname), drop = FALSE])
    eta_df <- sample_etas(n, pname, omega_mat, R)

    param_samples <- compose_params(theta_df, eta_df, pname, par_df)

    out <- param_samples
    if (isTRUE(input$include_internals)) {
      out <- bind_cols(param_samples, theta_df, eta_df)
    }
    out <- maybe_transpose(out, transpose = isTRUE(input$transpose_out))

    list(
      samples = out,
      theta   = theta_df,
      omega   = as_tibble(omega_mat) |> set_names(paste0("omega_", pname)),
      params  = par_df,
      edges   = edges_tbl(),
      R       = R
    )
  }, ignoreInit = TRUE)

  # ---- Outputs ----
  output$sample_table <- renderDT({
    req(sim_result())
    df <- sim_result()$samples |>
      mutate(across(where(is.numeric), ~ round(., 3)))
    datatable(df, options = list(scrollX = TRUE, pageLength = 10))
  })

  # --- SAVE BUTTON LOGIC (Replaces downloadHandler) ---
  observeEvent(input$save_csv_btn, {
    req(sim_result())
    
    # 1. Open System Save Dialog via Tcl/Tk
    # This opens a window on the computer running the R code
    save_path <- tcltk::tk_getSaveFile(
      initialdir = getwd(),
      initialfile = "theta.csv",
      title = "Save Simulation Results",
      filetypes = "{{CSV Files} {.csv}} {{All Files} {*}}"
    )
    
    # tk_getSaveFile returns an empty string/character(0) if cancelled
    if (length(save_path) > 0 && nchar(as.character(save_path)) > 0) {
      
      # Ensure extension
      if (!grepl("\\.csv$", save_path, ignore.case = TRUE)) {
        save_path <- paste0(save_path, ".csv")
      }
      
      # Write file
      tryCatch({
        readr::write_csv(sim_result()$samples, save_path)
        
        # Update reactive value for the Exit button
        # Normalize path to use forward slashes for R compatibility
        clean_path <- normalizePath(save_path, winslash = "/", mustWork = FALSE)
        last_saved_path(clean_path)
        
        showNotification(paste("Saved to:", clean_path), type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving file:", e$message), type = "error")
      })
    }
  })

  # --- COPY AS R DATA FRAME BUTTON ---
  observeEvent(input$copy_df_btn, {
    req(sim_result())
    
    df <- sim_result()$samples
    
    # If transposed, we need to handle differently
    if (isTRUE(input$transpose_out)) {
      # Transposed: first column is parameter names, rest are samples
      # Need to convert back to wide format for matrix
      param_names <- df$parameter
      mat_data <- as.matrix(df[, -1])
      mat_data <- t(mat_data)  # Transpose so rows are samples
      colnames(mat_data) <- param_names
    } else {
      # Normal format: rows are samples, columns are parameters
      mat_data <- as.matrix(df)
    }
    
    # Build R code for data frame definition
    n_row <- nrow(mat_data)
    n_col <- ncol(mat_data)
    col_names <- colnames(mat_data)
    
    # Format each column as a vector
    col_strings <- sapply(seq_len(n_col), function(j) {
      col_vals <- mat_data[, j]
      formatted_vals <- paste(format(col_vals, scientific = FALSE, trim = TRUE), collapse = ", ")
      sprintf('  %s = c(%s)', col_names[j], formatted_vals)
    })
    
    # Join columns with comma and newline
    cols_str <- paste(col_strings, collapse = ",\n")
    
    # Build the full R expression for data frame
    r_code <- sprintf(
      'theta <- data.frame(\n%s\n)',
      cols_str
    )
    
    # Copy to clipboard
    tryCatch({
      clipr::write_clip(r_code)
      showNotification(
        paste0("Copied theta data frame (", n_row, " x ", n_col, ") to clipboard. Paste into R to define."),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Error copying to clipboard:", e$message), type = "error")
    })
  })

  # --- EXIT BUTTON LOGIC ---
  observeEvent(input$exit_app_btn, {
    path_str <- last_saved_path()
    
    # Prepare the string
    copy_str <- sprintf('theta <- read.csv("%s")', path_str)
    
    # Copy to clipboard
    tryCatch({
      clipr::write_clip(copy_str)
      message("Copied to clipboard: ", copy_str)
    }, error = function(e) {
      warning("Could not copy to clipboard: ", e$message)
    })
    
    # Stop App
    stopApp()
  })
  
  # Sampled parameter summary table - shows statistics from theta samples with requested values
  output$sampled_summary <- renderDT({
    req(sim_result())
    s <- sim_result()
    
    param_names <- s$params$param
    theta_df <- s$theta  # Raw theta samples (before eta applied)
    omega_df <- s$omega  # Sampled omega values (CV as decimal)
    n_obs <- nrow(theta_df)
    
    # Get the ORIGINAL input values (before any conversions)
    input_params <- params_tbl()
    
    # Calculate statistics from the RAW THETA samples
    theta_means <- sapply(param_names, function(p) {
      mean(theta_df[[paste0("theta_", p)]], na.rm = TRUE)
    })
    theta_sds <- sapply(param_names, function(p) {
      sd(theta_df[[paste0("theta_", p)]], na.rm = TRUE)
    })
    
    # Theta RSE% from samples: (SD/mean)*100
    theta_rse_sampled <- ifelse(theta_means != 0, abs(theta_sds / theta_means) * 100, 0)
    theta_se_sampled <- theta_sds
    
    # Omega/IIV from samples: mean of the sampled omega values (already in CV decimal)
    omega_means <- sapply(param_names, function(p) {
      mean(omega_df[[paste0("omega_", p)]], na.rm = TRUE)
    })
    omega_sds <- sapply(param_names, function(p) {
      sd(omega_df[[paste0("omega_", p)]], na.rm = TRUE)
    })
    
    # Convert omega (CV as decimal) to display format
    if (input$iiv_input_type == "cv") {
      iiv_sampled <- omega_means * 100
      iiv_se_sampled <- omega_sds * 100
    } else {
      iiv_sampled <- log(1 + omega_means^2)
      iiv_se_sampled <- ifelse(omega_means > 0, 
                                2 * omega_means / (1 + omega_means^2) * omega_sds, 
                                0)
    }
    
    # RSE% of IIV: (SE/mean)*100
    iiv_rse_sampled <- ifelse(iiv_sampled != 0, abs(iiv_se_sampled / iiv_sampled) * 100, 0)
    
    # Theta uncertainty in requested format
    if (input$rse_input_type == "rse") {
      theta_unc_sampled <- theta_rse_sampled
    } else {
      theta_unc_sampled <- theta_se_sampled
    }
    
    # IIV uncertainty in requested format
    if (input$rse_input_type == "rse") {
      iiv_unc_sampled <- iiv_rse_sampled
    } else {
      iiv_unc_sampled <- iiv_se_sampled
    }
    
    # Min/max from the FINAL composed samples (theta * exp(eta))
    samples <- s$samples
    if (isTRUE(input$transpose_out)) {
      pnames <- samples$parameter
      mat_data <- as.matrix(samples[, -1])
      mat_data <- t(mat_data)
      colnames(mat_data) <- pnames
      samples <- as_tibble(mat_data)
    }
    
    # Get requested (input) values
    req_theta <- input_params$theta[match(param_names, input_params$param)]
    req_theta_unc <- input_params$rse_theta[match(param_names, input_params$param)]
    req_iiv <- input_params$cv_iiv[match(param_names, input_params$param)]
    req_iiv_unc <- input_params$rse_iiv[match(param_names, input_params$param)]
    req_min <- input_params$min[match(param_names, input_params$param)]
    req_max <- input_params$max[match(param_names, input_params$param)]
    
    # Helper to format cell with sampled and requested values
    format_cell <- function(sampled, requested) {
      sampled_str <- round(sampled, 3)
      requested_str <- if (is.infinite(requested)) {
        ifelse(requested > 0, "Inf", "-Inf")
      } else {
        round(requested, 3)
      }
      paste0('<span style="font-weight:500;">', sampled_str, '</span>',
             '<br><span style="color:#888; font-style:italic; font-size:11px;">', 
             requested_str, '</span>')
    }
    
    # Sampled min/max from the FINAL composed samples
    sampled_min <- sapply(param_names, function(p) min(samples[[p]], na.rm = TRUE))
    sampled_max <- sapply(param_names, function(p) max(samples[[p]], na.rm = TRUE))
    
    # Build display dataframe with HTML formatting
    summary_df <- tibble::tibble(
      param = param_names,
      theta = mapply(format_cell, theta_means, req_theta),
      rse_theta = mapply(format_cell, theta_unc_sampled, req_theta_unc),
      cv_iiv = mapply(format_cell, iiv_sampled, req_iiv),
      rse_cv = mapply(format_cell, iiv_unc_sampled, req_iiv_unc),
      min = mapply(format_cell, sampled_min, req_min),
      max = mapply(format_cell, sampled_max, req_max)
    )
    
    # Dynamic column names based on input types
    iiv_col_name <- if (input$iiv_input_type == "omega2") "IIV (ω²)" else "IIV (CV%)"
    theta_unc_name <- if (input$rse_input_type == "se") "Theta SE" else "Theta RSE%"
    iiv_unc_name <- if (input$rse_input_type == "se") "IIV SE" else "IIV RSE%"
    
    datatable(summary_df, rownames = FALSE, escape = FALSE,
              colnames = c("Name", "Theta TV", theta_unc_name, iiv_col_name, iiv_unc_name, "Min", "Max"),
              options = list(dom = 't', scrollX = TRUE, pageLength = 20))
  })
  
  # Sampled ETA correlation matrix
  output$sampled_corr_matrix <- renderDT({
    req(sim_result())
    s <- sim_result()
    
    param_names <- s$params$param
    
    # Get eta samples - need to compute them from theta and final samples
    theta_df <- s$theta
    samples <- s$samples
    if (isTRUE(input$transpose_out)) {
      pnames <- samples$parameter
      mat_data <- as.matrix(samples[, -1])
      mat_data <- t(mat_data)
      colnames(mat_data) <- pnames
      samples <- as_tibble(mat_data)
    }
    
    # Compute eta = log(final / theta) for each parameter
    # Only for parameters with IIV > 0
    eta_df <- tibble::tibble(.rows = nrow(samples))
    params_with_iiv <- s$params$param[s$params$cv_iiv > 0]
    
    for (p in params_with_iiv) {
      theta_col <- paste0("theta_", p)
      final_vals <- samples[[p]]
      theta_vals <- theta_df[[theta_col]]
      # eta = log(final / theta), but handle zeros/negatives
      eta_vals <- ifelse(final_vals > 0 & theta_vals > 0, 
                         log(final_vals / theta_vals), 
                         0)
      eta_df[[paste0("eta_", p)]] <- eta_vals
    }
    
    if (ncol(eta_df) < 2) {
      # Not enough parameters with IIV for correlation
      return(datatable(
        data.frame(Note = "Need at least 2 parameters with IIV to compute correlation"),
        rownames = FALSE,
        options = list(dom = 't')
      ))
    }
    
    # Calculate correlation matrix of etas
    corr_mat <- cor(eta_df, use = "pairwise.complete.obs")
    
    # Clean up column names (remove "eta_" prefix for display)
    clean_names <- gsub("^eta_", "", colnames(corr_mat))
    colnames(corr_mat) <- clean_names
    rownames(corr_mat) <- clean_names
    
    # Build requested correlation matrix from edges
    req_corr_mat <- diag(1, length(clean_names))
    dimnames(req_corr_mat) <- list(clean_names, clean_names)
    edges <- edges_tbl()
    if (!is.null(edges) && nrow(edges) > 0) {
      for (k in seq_len(nrow(edges))) {
        pi <- as.character(edges$i[k])
        pj <- as.character(edges$j[k])
        rho <- as.numeric(edges$rho[k])
        if (pi %in% clean_names && pj %in% clean_names) {
          req_corr_mat[pi, pj] <- rho
          req_corr_mat[pj, pi] <- rho
        }
      }
    }
    
    # Format cells with sampled and requested values
    format_corr_cell <- function(sampled, requested) {
      paste0('<span style="font-weight:500;">', round(sampled, 3), '</span>',
             '<br><span style="color:#888; font-style:italic; font-size:11px;">', 
             round(requested, 3), '</span>')
    }
    
    # Build formatted matrix
    formatted_mat <- matrix("", nrow = length(clean_names), ncol = length(clean_names))
    for (i in seq_along(clean_names)) {
      for (j in seq_along(clean_names)) {
        formatted_mat[i, j] <- format_corr_cell(
          corr_mat[clean_names[i], clean_names[j]],
          req_corr_mat[clean_names[i], clean_names[j]]
        )
      }
    }
    
    corr_df <- as.data.frame(formatted_mat)
    colnames(corr_df) <- clean_names
    corr_df <- cbind(Parameter = clean_names, corr_df)
    
    datatable(corr_df, rownames = FALSE, escape = FALSE,
              options = list(dom = 't', scrollX = TRUE, pageLength = 20))
  })
  
  # --- HISTOGRAMS TAB ---
  output$param_histograms <- renderPlot({
    req(sim_result())
    
    # Get the samples (handle transposed case)
    df <- sim_result()$samples
    if (isTRUE(input$transpose_out)) {
      # Transposed: first column is parameter names, rest are samples
      param_names <- df$parameter
      mat_data <- as.matrix(df[, -1])
      mat_data <- t(mat_data)
      colnames(mat_data) <- param_names
      df <- as_tibble(mat_data)
    }
    
    # Get parameter names (exclude internal columns if present)
    param_names <- sim_result()$params$param
    df_plot <- df[, param_names, drop = FALSE]
    
    n_params <- length(param_names)
    n_bins <- input$hist_bins %||% 30
    
    # Calculate grid dimensions
    n_cols <- min(3, n_params)
    n_rows <- ceiling(n_params / n_cols)
    
    # Set up multi-panel plot
    par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
    
    for (par in param_names) {
      vals <- df_plot[[par]]
      hist(vals, breaks = n_bins, main = par, xlab = "", col = "steelblue", border = "white")
    }
  }, height = function() {
    req(sim_result())
    n_params <- length(sim_result()$params$param)
    n_cols <- min(3, n_params)
    n_rows <- ceiling(n_params / n_cols)
    n_rows * 250  # 250 pixels per row
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
