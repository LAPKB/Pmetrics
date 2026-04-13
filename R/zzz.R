.onAttach <- function(...) {
  if (interactive()) {
    installedVersion <- packageVersion("Pmetrics")

    # Check Rust installation
    rustcVersion <- tryCatch(
      system("rustc --version", intern = TRUE),
      error = function(e) NA
    )

    cli::cli_div(theme = list(span.red = list(color = "red", "font-weight" = "bold")))
    cli::cli_h2("Welcome to Pmetrics {installedVersion}!")
    ul <- cli::cli_ul()
    cli::cli_li("For {.strong help} and to report {.strong issues}, use {.help PM_help}.")
    cli::cli_li("For {.strong documentation}, use {.help PM_manual}.")
    cli::cli_li("View user {.strong options} with {.help setPMoptions}.")
    cli::cli_li("Model library loaded. View with {.help model_lib}.")
    cli::cli_li("Check for Pmetrics and R updates with {.help check_updates}.")
    if (is.na(rustcVersion) || length(rustcVersion) == 0) {
      cli::cli_li("{.red Warning:} Rust compiler not found. Please install Rust from https://www.rust-lang.org/tools/install")
    } else {
      cli::cli_li("Installed Rust version: {rustcVersion}")
    }
    cli::cli_end(ul)
    pm_maybe_notify_updates()
    
  }
  
  # Set user options for the session
  setPMoptions(launch.app = FALSE)
  
  # Build model library
  # build_model_lib()
}
