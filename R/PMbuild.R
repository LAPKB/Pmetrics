#' @title Build Pmetrics
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Compile Rust source code used by Pmetrics.
#'
#' @author Michael Neely and Julian Otalvaro
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Compile Rust source code used by Pmetrics.
#'
#' @author Michael Neely and Julian Otalvaro
#' @export



PM_build <- function() {
  
  if (is_rustup_installed()) {
    cli::cli_text("Rust was detected in your system, Fetching dependencies and building base project.")
    template_path <- resolve_template_path()
    dummy_compile(template_path = template_path)
  } else {
    cli::cli_text("Rust was not detected in your system, this can be caused by multiple reasons:")
    ul <- cli::cli_ul()
    cli::cli_li("You have not installed rustup in your system, Follow the installation instructions at https://www.rust-lang.org/tools/install")
    cli::cli_li("You might have rustup installed in your system but your $PATH has not been updated (Windows), try closing and re-opening your R session, and/or Rstudio/Positron.")
    cli::cli_li("If you are using linux/MacOS and this error persists after installing rust, try using this command in your terminal: {.code sudo ln -s ~/.cargo/bin/* /usr/local/sbin}")
    cli::cli_end(ul)
    cli::cli_text("If this error persists, please refer to our discussions website: https://github.com/LAPKB/Pmetrics/discussions.")
  }
}

resolve_template_path <- function() {
  package_path <- system.file(package = "Pmetrics")
  in_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))
  in_development <- Sys.getenv("env") == "Development"
  in_cargo_workspace <- nzchar(package_path) && file.exists(file.path(package_path, "Cargo.toml"))

  if (in_development || in_check || in_cargo_workspace) {
    return(file.path(temporary_path(), "template"))
  }

  package_path
}

is_rustup_installed <- function() {
  flag <- is_cargo_installed()
  # Sometimes R does not find rustup even if it is installed,
  # Fix: create a symlink to any of the folders watched by system("echo $PATH")
  # sudo ln -s ~/.cargo/bin/* /usr/local/sbin
  # for rustup and cargo
  # We cannot do it automatically because it requires elevated permissions
  return(flag)
}
is_rustup_installed <- function() {
  flag <- is_cargo_installed()
  # Sometimes R does not find rustup even if it is installed,
  # Fix: create a symlink to any of the folders watched by system("echo $PATH")
  # sudo ln -s ~/.cargo/bin/* /usr/local/sbin
  # for rustup and cargo
  # We cannot do it automatically because it requires elevated permissions
  return(flag)
}
