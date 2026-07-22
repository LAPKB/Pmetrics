#' @title Build Pmetrics
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Building Pmetrics is no longer required. Models are now written in the
#' pharmsol DSL and compiled just-in-time by the bundled Rust backend, so there
#' is no separate compilation step and no Rust toolchain is needed.
#'
#' This function is retained so that existing scripts continue to run; it simply
#' informs the user that no action is necessary.
#'
#' @author Michael Neely and Julian Otalvaro
#' @return Invisibly returns `NULL`.
#' @export

PM_build <- function() {
  cli::cli_inform(c(
    "i" = "{.fn PM_build} is no longer required.",
    " " = "Models are compiled just-in-time from the pharmsol DSL, so no Rust toolchain or build step is needed."
  ))
  invisible(NULL)
}
