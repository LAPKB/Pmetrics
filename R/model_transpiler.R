# R-to-Rust Transpiler for ODE Function
# Transforms an R ODE function into Rust closures with zero-based indexing,
# parameter/covariate fetching, and arithmetic operations.

# Convert an R expression to Rust code (recursive)
expr_to_rust <- function(expr, params = NULL, covs = NULL) {
  # Base cases: numeric literals as floats, symbols
  if (is.numeric(expr) && length(expr) == 1) {
    val <- expr
    if (val == floor(val)) {
      return(sprintf("%d.0", as.integer(val)))
    } else {
      return(as.character(val))
    }
  }
  if (is.symbol(expr)) {
    return(as.character(expr))
  }

  # Handle indexing: var[idx] with zero-based adjustment
  if (is.call(expr) && as.character(expr[[1]]) == "[") {
    var <- as.character(expr[[2]])
    idx_raw <- expr[[3]]
    if (is.numeric(idx_raw) && length(idx_raw) == 1) {
      idx_val <- as.integer(idx_raw) - 1
      return(sprintf("%s[%d]", var, idx_val))
    } else {
      idx_code <- expr_to_rust(idx_raw, params, covs)
      return(sprintf("%s[%s - 1]", var, idx_code))
    }
  }

  if (!is.call(expr)) stop("Unknown expression type")
  op <- as.character(expr[[1]])
  args <- as.list(expr[-1])
  rust_args <- lapply(args, expr_to_rust, params = params, covs = covs)

  switch(op,
    # Grouping
    "(" = sprintf("(%s)", rust_args[[1]]),
    "{" = {
      # strip explicit block tokens—just emit the inner code
      if (length(rust_args) > 0) rust_args[[1]] else ""
    },

    # Arithmetic
    "+" = if (length(rust_args) == 1) {
      sprintf("+(%s)", rust_args[[1]])
    } else {
      sprintf("%s + %s", rust_args[[1]], rust_args[[2]])
    },
    "-" = if (length(rust_args) == 1) {
      sprintf("-(%s)", rust_args[[1]])
    } else {
      sprintf("(%s) - (%s)", rust_args[[1]], rust_args[[2]])
    },
    "*" = sprintf("%s * %s", rust_args[[1]], rust_args[[2]]),
    "/" = sprintf("%s / %s", rust_args[[1]], rust_args[[2]]),
    "^" = sprintf("(%s).powf(%s)", rust_args[[1]], rust_args[[2]]),

    # Comparison
    "==" = sprintf("%s == %s", rust_args[[1]], rust_args[[2]]),
    "!=" = sprintf("%s != %s", rust_args[[1]], rust_args[[2]]),
    ">=" = sprintf("%s >= %s", rust_args[[1]], rust_args[[2]]),
    "<=" = sprintf("%s <= %s", rust_args[[1]], rust_args[[2]]),
    ">" = sprintf("%s > %s", rust_args[[1]], rust_args[[2]]),
    "<" = sprintf("%s < %s", rust_args[[1]], rust_args[[2]]),



    # Logical
    "&" = sprintf("%s && %s", rust_args[[1]], rust_args[[2]]),
    "|" = sprintf("%s || %s", rust_args[[1]], rust_args[[2]]),
    "!" = sprintf("!(%s)", rust_args[[1]]),

    # Function calls
    "abs" = sprintf("(%s).abs()", rust_args[[1]]),
    "log" = sprintf("(%s).ln()", rust_args[[1]]),
    "exp" = sprintf("(%s).exp()", rust_args[[1]]),

    # Assignment
    "<-" = ,
    "=" = {
      lhs <- args[[1]]
      rhs_code <- rust_args[[2]]

      if (is.symbol(lhs)) {
        # It's a plain variable like `ka = ...`
        sprintf("let %s = %s;", as.character(lhs), rhs_code)
      } else {
        # It’s likely something like dx[1] = ...
        lhs_code <- expr_to_rust(lhs, params, covs)
        sprintf("%s = %s;", lhs_code, rhs_code)
      }
    },

    # If
    "if" = {
      cond <- rust_args[[1]]
      then_code <- rust_args[[2]]
      if (length(rust_args) == 3) {
        else_code <- rust_args[[3]]
        # single-line if/else expression
        sprintf("if %s { %s } else { %s }", cond, then_code, else_code)
      } else {
        # fall back to statement form if no else
        sprintf("if %s { %s }", cond, then_code)
      }
    },

    # For
    "for" = {
      var <- as.character(args[[1]])
      n_sym <- as.character(args[[2]][[3]])
      loop_exprs <- if (is.call(args[[3]]) && as.character(args[[3]][[1]]) == "{") as.list(args[[3]][-1]) else list(args[[3]])
      body <- stmts_to_rust(loop_exprs, params, covs)
      sprintf("for %s in 0..%s as usize {\n%s}\n", var, n_sym, indent(body))
    },
    stop(sprintf("Unsupported operation: %s", op))
  )
}

# Helpers: convert list of statements to Rust and indent blocks
stmts_to_rust <- function(exprs, params = NULL, covs = NULL) {
  lines <- vapply(exprs, expr_to_rust, character(1), params = params, covs = covs)
  tolower(paste(lines, collapse = "\n")) # rust is case sensitive, make everything lowercase
}

indent <- function(text, spaces = 4) {
  prefix <- strrep(" ", spaces)
  paste0(prefix, gsub("\n", paste0("\n", prefix), text))
}

# Transpile an R ODE function to Rust closure
transpile_ode_eqn <- function(fun, params, covs, sec) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
  header <- sprintf(
    "|x, p, t, dx, rateiv, cov| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s); %s",
    paste(covs, collapse = ", "),
    paste(params, collapse = ", "),
    paste(sec, collapse = ", ")
  )
  body_rust <- stmts_to_rust(exprs, params, covs) %>%
    stringr::str_replace_all("\\((b|bolus)\\[\\d+\\]\\)", "") %>%
    stringr::str_replace_all("r\\[", "rateiv\\[")
  sprintf("%s\n%s\n }", header, indent(body_rust, spaces = 4))
}

transpile_analytic_eqn <- function(fun, params, covs) {
  if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") {
    found <- get_found_model(fun)
    if (length(found) == 1) { # NA
      cli::cli_abort(c(
        "x" = "No ODE and no library model templates found.",
        "i" = "EQN block must contain ODEs or a single library model template name."
      ))
    }
    tem <- found$name
    eqns <- as.list(body(fun)[-1])
    if (length(eqns) > 0) {
      eqns_char <- map_chr(eqns, deparse)
      # check for ODE, which should not be present
      if (any(stringr::str_detect(eqns_char, regex("dx\\[\\d+\\]", ignore_case = FALSE)))) {
        cli::cli_abort(c(
          "x" = "You appear to have included both a model library template and ODE.",
          "i" = "EQN block must contain ODEs or a single library model template name, not both."
        ))
      }
      exprs <- eqns[-which(eqns_char == found$name)]
    } else {
      exprs <- NULL
    }
  } else {
    list(body(fun))
  }

  # map model name from R to rust
  rust_tem <- dplyr::case_when(
    tem == "one_comp_iv" ~ "one_compartment",
    tem == "one_comp_iv_cl" ~ "", # TBD in rust
    tem == "two_comp_iv" ~ "two_compartments",
    tem == "two_comp_iv_cl" ~ "", # TBD in rust
    tem == "two_comp_bolus" ~ "one_compartment_with_absorption",
    tem == "two_comp_bolus_cl" ~ "", # TBD in rust
    tem == "three_comp_iv" ~ "three_compartments", # TBD in R
    tem == "three_comp_iv_cl" ~ "", # TBD in rust
    tem == "three_comp_bolus" ~ "two_compartments_with_absorption",
    tem == "three_comp_bolus_cl" ~ "", # TBD in rust
    tem == "four_comp_bolus" ~ "three_compartments_with_absorption", # TBD in R
    tem == "four_comp_bolus_cl" ~ "" # TBD in rust
  )

  header <- sprintf(
    " %s,\n |p, t, cov| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(&p, %s);",
    rust_tem,
    paste(covs, collapse = ", "),
    paste(params, collapse = ", ")
  )
  body_rust <- stmts_to_rust(exprs)
  # remap parameters
  req_par <- get(tem)$parameters %>%
    tolower() %>%
    purrr::imap_chr(\(x, y){
      sprintf("p[%i] = %s;", y - 1, x)
    }) %>%
    paste(collapse = "\n")

  sprintf("%s\n%s\n%s\n }", header, indent(body_rust, spaces = 4), indent(req_par, spaces = 4))
}

transpile_sec <- function(fun) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
  body_rust <- stmts_to_rust(exprs)
  sprintf("%s\n", indent(body_rust, spaces = 4))
}

transpile_fa <- function(fun, params, covs, sec) {
  "|_p| fa! {}"
}


transpile_lag <- function(fun, params, covs, sec) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") {
    as.list(body(fun)[-1])
  } else {
    list(body(fun))
  }

  find_max_idx <- function(expr) {
    if (is.call(expr) && as.character(expr[[1]]) == "[" &&
      as.character(expr[[2]]) == "lag" &&
      is.numeric(expr[[3]])) {
      return(as.integer(expr[[3]]))
    }
    if (is.call(expr)) {
      return(max(sapply(as.list(expr), find_max_idx), 0L))
    }
    0L
  }
  max_lag <- max(sapply(exprs, find_max_idx), 0L)
  # If no lag[] found, we still need at least size 1
  arr_size <- if (max_lag > 0) max_lag else 1L

  header <- sprintf(
    "|p| {\n  fetch_params!(p, %s);\n  let mut lag: [f64; %d] = [0.0; %d];\n%s",
    paste(params, collapse = ", "),
    arr_size, arr_size,
    if (length(sec)) paste0("  ", paste(sec, collapse = "\n  "), "\n") else ""
  )

  body_lines <- stmts_to_rust(exprs, params = params, covs = covs)

  slots <- seq_len(arr_size) - 1L
  slot_args <- paste0(slots, "=> lag[", slots, "]", collapse = ", ")
  footer <- sprintf("  lag!{%s}}", slot_args)

  paste0(header, indent(body_lines, 2), "\n", footer)
}





transpile_ini <- function(fun, params, covs, sec) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
  header <- sprintf(
    "|p, t, cov, _x| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s); %s",
    paste(covs, collapse = ", "),
    paste(params, collapse = ", "),
    paste(sec, collapse = ", ")
  )
  body_rust <- stmts_to_rust(exprs, params, covs)
  sprintf("%s\n%s\n }", header, indent(body_rust, spaces = 4))
}

transpile_out <- function(fun, params, covs, sec) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
  header <- sprintf(
    "|x, p, t, cov, y| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s);\n%s",
    paste(covs, collapse = ", "),
    paste(params, collapse = ", "),
    paste(sec, collapse = ", ")
  )
  body_rust <- stmts_to_rust(exprs, params, covs)
  sprintf("%s\n%s\n }", header, indent(body_rust, spaces = 4))
}

empty_sec <- function() {
  "|_p, _t, _cov| { }"
}
empty_fa <- function() {
  "|_p| fa! {}"
}
empty_lag <- function() {
  "|_p| lag! {}"
}
empty_ini <- function() {
  "|_p, _t, _cov, _x| { }"
}

empty_out <- function() {
  "|_x, _p, _t, _cov, _y| { }"
}

# ------------------ Error Transpilation Support ------------------
# Validate and transpile an R 'error' function into a real R list of evaluated calls

# collect_error_entries <- function(fun) {
#   body_expr <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") {
#     as.list(body(fun)[-1])
#   } else {
#     list(body(fun))
#   }
#
#   entries <- list()
#   expected_idx <- 1L
#
#   for (expr in body_expr) {
#     if (!is.call(expr) || !(as.character(expr[[1]]) %in% c("<-", "="))) {
#       cli::cli_abort(c(
#         "x" = "Invalid syntax in {.fn err}",
#         "i" = "Only assingments allowed, but found {.code {deparse(expr)}}"))
#     }
#     lhs <- expr[[2]]
#     rhs <- expr[[3]]
#
#     if (!is.call(lhs) || as.character(lhs[[1]]) != "[" || as.character(lhs[[2]]) != "e") {
#       cli::cli_abort(c(
#         "x" = "Invalid LHS in {.fn err}",
#         "i" = "Expected {.code e[index]}, got {.code {deparse(lhs)}}"))
#     }
#     idx_raw <- lhs[[3]]
#     if (!is.numeric(idx_raw) || length(idx_raw) != 1) {
#       cli::cli_abort(c(
#         "x" = "Invalid index in {.fn err}",
#         "i" = "Must be a single numeric literal, got  {.code {deparse(idx_raw)}}"))
#     }
#     idx <- as.integer(idx_raw)
#     if (idx != expected_idx) {
#       cli::cli_abort(c(
#         "x" = "Error indices must start at 1 and increment by 1.",
#         "i" = "Expected index {.val {expected_idx}}, got  {.val {idx}}"))
#
#     }
#     expected_idx <- expected_idx + 1L
#
#     if (!is.call(rhs) || !(as.character(rhs[[1]]) %in% c("proportional", "additive"))) {
#       cli::cli_abort(c(
#         "x" = "Invalid RHS in {.fn err}",
#         "i" = "Only {.fn proportional} or {.fn additive} calls allowed, but found {.code {deparse(rhs)}}"))
#     }
#
#     n_args <- length(rhs) - 1
#     if (!(n_args %in% c(2, 3))) {
#       cli::cli_abort(c(
#         "x" = "Invalid syntax in {.fn err}",
#         "i" = "{.code {as.character(rhs[[1]])}} must have 2 or 3 arguments, but got {.code {n_args}}"))
#     }
#
#     if (n_args == 2) {
#       rhs <- as.call(c(as.list(rhs), list(fixed = FALSE)))
#     }
#
#     entries[[as.character(idx)]] <- rhs
#   }
#   entries
# }
#
# # Transpile an R error function
# transpile_error <- function(fun) {
#   entries <- collect_error_entries(fun)
#   result <- list()
#   for (i in 1:length(entries)) {
#     # evaluate each proportional/additive call
#     result[[i]] <- eval(entries[[i]])
#   }
#   # ensure names are correct ("1","2",...)
#   names(result) <- 1:length(result)
#   result
# }

get_assignments <- function(fn, assign) {
  count_assignments <- function(expr) {
    if (is.call(expr)) {
      if (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("="))) {
        lhs <- expr[[2]]
        # Check if lhs is an indexing call (i.e., assign[...] <- ...)
        if (is.call(lhs) && identical(lhs[[1]], as.name("["))) {
          target_name <- as.character(lhs[[2]])
          if (tolower(target_name) == tolower(assign)) {
            return(1 + count_assignments(expr[[3]]))
          }
        }
        return(count_assignments(expr[[2]]) + count_assignments(expr[[3]]))
      } else {
        return(sum(sapply(expr, count_assignments)))
      }
    }
    return(0)
  }

  body_expr <- body(fn)
  count_assignments(body_expr)
}
