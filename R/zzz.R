.onAttach <- function(...) {
  if (interactive()) {
    installedVersion <- packageVersion("Pmetrics")

    cli::cli_h3("Welcome to Pmetrics {installedVersion}!")
    cli::cli_text("For more information or to report issues, visit our GitHub page: https://github.com/LAPKB/Pmetrics")
    cli::cli_text("For documentation, run PM_manual() in R")

    # Check Rust installation
    rustcVersion <- tryCatch(
      system("rustc --version", intern = TRUE),
      error = function(e) NA
    )

    if (is.na(rustcVersion) || length(rustcVersion) == 0) {
      cli::cli_alert_danger("Rust compiler not found. Please install Rust from https://www.rust-lang.org/tools/install")
    } else {
      cli::cli_alert_info("Rust is installed: {rustcVersion}")
    }
  }

  # Set user options for the session
  setPMoptions(launch.app = FALSE)
}
