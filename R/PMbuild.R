#' @title Build Pmetrics
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Compile Rust source code used by Pmetrics.
#'
#' @author Michael Neely and Julian Otalvaro
#' @export



PMbuild <- function() {
  if (is_rustup_installed()) {
    cat("Rust was detected in your system, Fetching dependencies and building base project.\n")
    template <- dummy_compile()
    setPMoptions(rust_template = template)
  } else {
    cat("\n Rust was not detected in your system, this can be caused by multiple reasons:\n")
    cat("* You have not installed rustup in your system, Follow the installation instructions at https://www.rust-lang.org/tools/install\n")
    cat("* You might have rustup installed in your system but your $PATH has not been updated (Windows), try closing and re-opening your R session, and/or Rstudio.\n")
    cat("* If you are using linux/MacOS and this error persists after installing rust, try using this command in your terminal: sudo ln -s ~/.cargo/bin/* /usr/local/sbin \n")
    cat("\n If this error persists, please refer to our discussions website: https://github.com/LAPKB/Pmetrics/discussions\n")
  }
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
