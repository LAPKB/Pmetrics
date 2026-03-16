.onAttach <- function(...) {
  if (interactive()) {
    installedVersion <- packageVersion("Pmetrics")
    latestPmetrics <- tryCatch(
      package_version(
        jsonlite::fromJSON("https://lapkb.r-universe.dev/api/packages/Pmetrics")$Version
      ),
      error = function(e) NA
    )
    
    # Check Rust installation
    rustcVersion <- tryCatch(
      system("rustc --version", intern = TRUE),
      error = function(e) NA
    )
    
    # Check R version (platform-specific endpoint)
    currentR <- getRversion()
    latestRInfo <- tryCatch(latestR(), error = function(e) NULL)
    latestRVersion <- if (!is.null(latestRInfo) && !is.null(latestRInfo$version)) {
      package_version(latestRInfo$version)
    } else {
      NA
    }
    
    
    cli::cli_div(theme = list(span.red = list(color = "red", "font-weight" = "bold")))
    cli::cli_h2("Welcome to Pmetrics {installedVersion}!")
    ul <- cli::cli_ul()
    cli::cli_li("For {.strong help} and to report {.strong issues}, use {.help PM_help}.")
    cli::cli_li("For {.strong documentation}, use {.help PM_manual}.")
    cli::cli_li("View user {.strong options} with {.help setPMoptions}.")
    cli::cli_li("Model library loaded. View with {.help model_lib}.")
    if (!is.na(latestPmetrics)) {
      if (installedVersion < latestPmetrics) {
        cli::cli_li("{.red Warning:} Your Pmetrics version ({installedVersion}) is older than the latest release ({latestPmetrics}). Update instructions are at https://github.com/LAPKB/Pmetrics.")
      } else {
        cli::cli_li("You are using the latest Pmetrics version: {installedVersion}.")
      }
    }
    if (!is.na(latestRVersion)){
      if(currentR < latestRVersion) {
        cli::cli_li("{.red Warning:} Your R version ({currentR}) is older than the latest release ({latestRVersion}). Use {.help downloadR} to download the latest platform-specific installer.")
      } else {
        cli::cli_li("You are using the latest R version: {currentR}.")
      }
    }
    if (is.na(rustcVersion) || length(rustcVersion) == 0) {
      cli::cli_li("{.red Warning:} Rust compiler not found. Please install Rust from https://www.rust-lang.org/tools/install")
    } else {
      cli::cli_li("Installed Rust version: {rustcVersion}")
    }
    cli::cli_end(ul)
    
  }
  
  # Set user options for the session
  setPMoptions(launch.app = FALSE)
  
  # Build model library
  build_model_lib()
}
