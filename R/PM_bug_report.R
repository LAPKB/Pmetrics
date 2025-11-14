#' @title Get Help and Report Issues
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function displays system information useful for debugging and provides
#' a link to the GitHub repository for reporting bugs or requesting help.
#' @details
#' When you encounter bugs or need help, this function collects relevant system
#' information that will help maintainers diagnose the issue. The information
#' includes OS version, R version, RStudio version (if applicable), package version,
#' Rust and Cargo versions (if available).
#'
#' @param copy Logical. If `TRUE`, copies the system information to clipboard.
#' Default is `FALSE`.
#' @return Invisibly returns a list containing the system information.
#' @examples
#' \dontrun{
#' # Display help information
#' PM_bug_report()
#'
#' # Copy information to clipboard
#' PM_bug_report(copy = TRUE)
#' }
#' @export

PM_bug_report <- function(copy = FALSE) {
  
  # GitHub repository URL
  github_url <- "https://github.com/LAPKB/Pmetrics_rust"
  
  # Collect system information
  sys_info <- list()
  
  # Package version
  sys_info$package_version <- tryCatch(
    as.character(utils::packageVersion("Pmetrics")),
    error = function(e) "Unknown"
  )
  
  # R version
  sys_info$r_version <- paste0(R.version$major, ".", R.version$minor)
  
  # OS information
  sys_info$os <- Sys.info()["sysname"]
  sys_info$os_release <- Sys.info()["release"]
  sys_info$os_version <- Sys.info()["version"]
  
  # System architecture
  sys_info$machine <- Sys.info()["machine"]
  sys_info$platform <- R.version$platform
  sys_info$arch <- .Platform$r_arch
  
  # RStudio version
  sys_info$rstudio_version <- tryCatch({
    if (Sys.getenv("RSTUDIO") == "1") {
      rstudioapi::versionInfo()$version
    } else {
      "Not running in RStudio"
    }
  }, error = function(e) "Not running in RStudio")
  
  # Rust version
  sys_info$rust_version <- tryCatch({
    rust_output <- system("rustc --version", intern = TRUE, ignore.stderr = TRUE)
    if (length(rust_output) > 0) rust_output[1] else "Not found"
  }, error = function(e) "Not found")
  
  # Cargo version
  sys_info$cargo_version <- tryCatch({
    cargo_output <- system("cargo --version", intern = TRUE, ignore.stderr = TRUE)
    if (length(cargo_output) > 0) cargo_output[1] else "Not found"
  }, error = function(e) "Not found")
  
  # Locale
  sys_info$locale <- Sys.getlocale("LC_ALL")
  
  # Create formatted output
  output <- paste0(
    "\n",
    "====================================================================\n",
    "                   PMETRICS HELP & BUG REPORT                      \n",
    "====================================================================\n\n",
    "If you need help or want to report a bug, please visit:\n",
    github_url, "/issues\n\n",
    "Please include the following system information in your report:\n\n",
    "--------------------------------------------------------------------\n",
    "SYSTEM INFORMATION\n",
    "--------------------------------------------------------------------\n",
    "Pmetrics version:  ", sys_info$package_version, "\n",
    "R version:         ", sys_info$r_version, "\n",
    "RStudio version:   ", as.character(sys_info$rstudio_version), "\n",
    "Operating System:  ", sys_info$os, " ", sys_info$os_release, "\n",
    "OS Version:        ", sys_info$os_version, "\n",
    "Platform:          ", sys_info$platform, "\n",
    "Architecture:      ", sys_info$machine, 
    ifelse(sys_info$arch != "", paste0(" (", sys_info$arch, ")"), ""), "\n",
    "Rust version:      ", sys_info$rust_version, "\n",
    "Cargo version:     ", sys_info$cargo_version, "\n",
    "Locale:            ", sys_info$locale, "\n",
    "--------------------------------------------------------------------\n\n",
    "ADDITIONAL RESOURCES:\n",
    "  - Documentation: ", github_url, "\n",
    "  - Issues:        ", github_url, "/issues\n",
    "  - Discussions:   ", github_url, "/discussions\n\n",
    "When reporting an issue, please:\n",
    "  1. Describe what you expected to happen\n",
    "  2. Describe what actually happened\n",
    "  3. Include the system information above\n",
    "  4. Provide a minimal reproducible example if possible\n\n",
    "====================================================================\n"
  )
  
  # Print output
  cat(output)
  
  # Copy to clipboard if requested
  if (copy) {
    tryCatch({
      clipr::write_clip(output)
      message("\nSystem information copied to clipboard!")
    }, error = function(e) {
      warning("Could not copy to clipboard. Install 'clipr' package for this feature.")
    })
  }
  
  # Return invisibly
  invisible(sys_info)
}

