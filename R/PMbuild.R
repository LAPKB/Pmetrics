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
  cli::cli_inform(c(
    "i" = "{.fn PM_build()} is no longer required with the DSL runtime.",
    " " = "Models are validated directly from {.code PM_model$dsl()} during fit and simulation."
  ))
  invisible(NULL)
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
