test_that <- function(desc, code) {
  desc <- as.character(desc)
  message(sprintf("[test] %s", desc))
  testthat::test_that(desc, code)
}

cleanup_exa_tmp <- function() {
  exa_tmp <- tryCatch(temporary_path(), error = function(...) NA_character_)

  if (
    is.character(exa_tmp) &&
      length(exa_tmp) == 1 &&
      !is.na(exa_tmp) &&
      nzchar(exa_tmp) &&
      dir.exists(exa_tmp)
  ) {
    unlink(exa_tmp, recursive = TRUE, force = TRUE)
  }

  invisible(NULL)
}

local_exa_tmp_cleanup <- function(env = parent.frame()) {
  withr::defer(cleanup_exa_tmp(), envir = env)
  invisible(NULL)
}

build_library_model <- function(model_name, mode = c("analytical", "ode")) {
  mode <- match.arg(mode)

  lib_entry <- getFromNamespace("get_model_library_entry", "Pmetrics")(model_name)

  eqn_block <- if (mode == "analytical") {
    eval(parse(text = sprintf("function(){ %s }", model_name)))
  } else {
    lib_entry$arg_list$eqn
  }

  args <- list(
    pri = as.list(lib_entry$arg_list$pri),
    eqn = eqn_block,
    out = lib_entry$arg_list$out,
    err = as.list(lib_entry$arg_list$err)
  )

  if ("cov" %in% names(lib_entry$arg_list)) {
    args$cov <- lib_entry$arg_list$cov
  }

  if ("sec" %in% names(lib_entry$arg_list)) {
    args$sec <- lib_entry$arg_list$sec
  }

  if ("lag" %in% names(lib_entry$arg_list)) {
    args$lag <- lib_entry$arg_list$lag
  }

  if ("fa" %in% names(lib_entry$arg_list)) {
    args$fa <- lib_entry$arg_list$fa
  }

  if ("ini" %in% names(lib_entry$arg_list)) {
    args$ini <- lib_entry$arg_list$ini
  }

  do.call(PM_model$new, args)
}

build_example_ode_model <- function(compile = TRUE) {
  PM_model$new(
    pri = list(
      ka = ab(0.1, 1.0),
      ke = ab(0.01, 0.5),
      v = ab(10, 100)
    ),
    eqn = function() {
      dx[1] <- -ka * x[1] + b[1]
      dx[2] <- ka * x[1] - ke * x[2]
    },
    out = function() {
      y[1] <- x[2] / v
    },
    err = list(additive(1, c(0.1, 0, 0, 0))),
    compile = compile
  )
}
