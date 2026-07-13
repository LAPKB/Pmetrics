# R-to-DSL emitter for Pmetrics models (pharmsol DSL).
#
# ---------------------------------------------------------------------------
# Overview
# ---------------------------------------------------------------------------
# Historically, Pmetrics translated model blocks into Rust source code that was
# compiled with `cargo` into a shared library. As of the pharmsol DSL backend,
# models are instead emitted as a small text description in the pharmsol DSL and
# JIT-compiled at run time inside the `pm_rs` Rust library. This removes the need
# for a Rust toolchain on the user's machine.
#
# This file converts the R model definition (stored in `model$arg_list`) into the
# pharmsol DSL "authoring shorthand". The DSL expression grammar is very close to
# R: mathematical functions use call syntax (`exp(x)`, `ln(x)`, `sqrt(x)`), the
# `^` operator is supported for powers, and `&&`/`||` are used for logical AND/OR.
#
# Naming conventions used by the emitter:
#   * States   `x[i]`            -> `x{i}`          (declared in `states = ...`)
#   * Outputs  `Y[i]` / `y[i]`   -> `outeq_{i-1}`   (declared in `outputs = ...`)
#   * Inputs   `b[j]`/`bolus[j]` -> `bolus(input_{j-1}) -> x{k}`   (route)
#              `rateiv[j]`/`r[j]`-> `infusion(input_{j-1}) -> x{k}`(route)
#   * Params / covariates keep their (lower-cased) names.
#
# Infusions and boluses are declared as routes into the compartment in which they
# appear in the derivative equations; the corresponding `rateiv[j]`/`b[j]` terms
# are stripped from the derivative because the DSL runtime injects them
# automatically from the route declaration.
# ---------------------------------------------------------------------------

# The set of DSL intrinsic functions and how R names map to them. R names that
# are not present here (e.g. hyperbolic or inverse-trigonometric functions) are
# not currently expressible in the DSL and trigger an informative error.
dsl_function_map <- function() {
  c(
    abs = "abs", exp = "exp", sqrt = "sqrt",
    ln = "ln", log = "ln", log10 = "log10", log2 = "log2",
    sin = "sin", cos = "cos", tan = "tan",
    floor = "floor", ceiling = "ceil", round = "round",
    max = "max", min = "min"
  )
}

# Convert a pure R expression (no assignments or blocks) to a DSL expression.
expr_to_dsl <- function(expr) {
  # Numeric literals: emit integers with a trailing `.0` so the DSL treats them
  # as floating point, matching the Rust transpiler's behaviour.
  if (is.numeric(expr) && length(expr) == 1) {
    val <- expr
    if (is.finite(val) && val == floor(val)) {
      return(sprintf("%d.0", as.integer(val)))
    }
    return(as.character(val))
  }
  if (is.symbol(expr)) {
    return(tolower(as.character(expr)))
  }
  if (!is.call(expr)) {
    cli::cli_abort("Unable to convert model expression to DSL: unsupported token.")
  }

  op <- as.character(expr[[1]])

  # Indexing: x[i] -> x{i}. Only literal, positive integer indices are allowed.
  if (op == "[") {
    var <- tolower(as.character(expr[[2]]))
    idx_raw <- expr[[3]]
    if (!(is.numeric(idx_raw) && length(idx_raw) == 1)) {
      cli::cli_abort(c(
        "x" = "Dynamic (non-literal) indices are not supported in the DSL backend.",
        "i" = "Use literal indices such as {.code x[1]}."
      ))
    }
    idx <- as.integer(idx_raw)
    if (var %in% c("b", "bolus", "rateiv", "r")) {
      cli::cli_abort(c(
        "x" = "Bolus/infusion inputs may only be used as standalone additive terms in derivative equations.",
        "i" = "Write, for example, {.code dx[1] = -ke * x[1] + rateiv[1]}, not inside a product."
      ))
    }
    return(sprintf("%s%d", var, idx))
  }

  args <- as.list(expr[-1])
  a <- lapply(args, expr_to_dsl)

  fmap <- dsl_function_map()

  out <- switch(op,
    "(" = sprintf("(%s)", a[[1]]),
    "+" = if (length(a) == 1) sprintf("+(%s)", a[[1]]) else sprintf("%s + %s", a[[1]], a[[2]]),
    "-" = if (length(a) == 1) sprintf("-(%s)", a[[1]]) else sprintf("(%s) - (%s)", a[[1]], a[[2]]),
    "*" = sprintf("%s * %s", a[[1]], a[[2]]),
    "/" = sprintf("%s / %s", a[[1]], a[[2]]),
    "^" = sprintf("(%s)^(%s)", a[[1]], a[[2]]),
    "==" = sprintf("%s == %s", a[[1]], a[[2]]),
    "!=" = sprintf("%s != %s", a[[1]], a[[2]]),
    ">=" = sprintf("%s >= %s", a[[1]], a[[2]]),
    "<=" = sprintf("%s <= %s", a[[1]], a[[2]]),
    ">" = sprintf("%s > %s", a[[1]], a[[2]]),
    "<" = sprintf("%s < %s", a[[1]], a[[2]]),
    "&" = sprintf("%s && %s", a[[1]], a[[2]]),
    "&&" = sprintf("%s && %s", a[[1]], a[[2]]),
    "|" = sprintf("%s || %s", a[[1]], a[[2]]),
    "||" = sprintf("%s || %s", a[[1]], a[[2]]),
    "!" = sprintf("!(%s)", a[[1]]),
    "if" = {
      cond <- a[[1]]
      then_code <- a[[2]]
      if (length(a) == 3) {
        sprintf("if %s { %s } else { %s }", cond, then_code, a[[3]])
      } else {
        cli::cli_abort(c(
          "x" = "Conditional expressions in the DSL must include an `else` branch.",
          "i" = "Write {.code if (cond) a else b}."
        ))
      }
    },
    {
      # Function call: look up in the DSL intrinsic map.
      if (!is.null(fmap[[op]])) {
        sprintf("%s(%s)", fmap[[op]], paste(unlist(a), collapse = ", "))
      } else {
        cli::cli_abort(c(
          "x" = "Unsupported function {.val {op}} for the DSL backend.",
          "i" = "Supported functions are: {.val {sort(unique(unname(dsl_function_map())))}}."
        ))
      }
    }
  )
  out
}

# Return the top-level statements of a model block function body.
dsl_body_stmts <- function(fun) {
  b <- body(fun)
  if (is.call(b) && as.character(b[[1]]) == "{") {
    as.list(b[-1])
  } else {
    list(b)
  }
}

# Is `expr` an assignment (`<-` or `=`)?
dsl_is_assign <- function(expr) {
  is.call(expr) && (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("=")))
}

# Flatten an expression into signed additive terms, i.e. a list of
# `list(sign = +/-1, expr = <expr>)`.
dsl_flatten_add <- function(expr, sign = 1) {
  if (is.call(expr) && identical(expr[[1]], as.name("+"))) {
    if (length(expr) == 3) {
      return(c(dsl_flatten_add(expr[[2]], sign), dsl_flatten_add(expr[[3]], sign)))
    }
    return(dsl_flatten_add(expr[[2]], sign)) # unary +
  }
  if (is.call(expr) && identical(expr[[1]], as.name("-"))) {
    if (length(expr) == 3) {
      return(c(dsl_flatten_add(expr[[2]], sign), dsl_flatten_add(expr[[3]], -sign)))
    }
    return(dsl_flatten_add(expr[[2]], -sign)) # unary -
  }
  list(list(sign = sign, expr = expr))
}

# If `expr` is a route reference (`b[j]`, `bolus[j]`, `rateiv[j]`, `r[j]`),
# return `list(kind = "bolus"|"infusion", input = j)`, else NULL.
dsl_route_of <- function(expr) {
  if (is.call(expr) && as.character(expr[[1]]) == "[") {
    v <- tolower(as.character(expr[[2]]))
    idx <- expr[[3]]
    if (v %in% c("b", "bolus", "rateiv", "r") && is.numeric(idx) && length(idx) == 1) {
      kind <- if (v %in% c("b", "bolus")) "bolus" else "infusion"
      return(list(kind = kind, input = as.integer(idx)))
    }
  }
  NULL
}

# Build a DSL expression string from a list of signed terms.
dsl_join_terms <- function(terms) {
  if (length(terms) == 0) {
    return("0.0")
  }
  pieces <- character(0)
  for (i in seq_along(terms)) {
    t <- terms[[i]]
    es <- expr_to_dsl(t$expr)
    if (i == 1) {
      pieces <- if (t$sign < 0) sprintf("-(%s)", es) else es
    } else {
      pieces <- paste0(pieces, if (t$sign < 0) " - " else " + ", sprintf("(%s)", es))
    }
  }
  pieces
}

# Convert a derivative RHS into (routes, stripped DSL expression) for the given
# destination compartment index `comp`.
dsl_extract_routes <- function(rhs, comp) {
  terms <- dsl_flatten_add(rhs)
  routes <- list()
  kept <- list()
  for (t in terms) {
    route <- dsl_route_of(t$expr)
    if (!is.null(route)) {
      if (t$sign < 0) {
        cli::cli_abort(c(
          "x" = "Bolus/infusion inputs must be added (not subtracted) in derivative equations.",
          "i" = "Write {.code dx[{comp}] = ... + rateiv[{route$input}]}."
        ))
      }
      route$comp <- comp
      routes[[length(routes) + 1]] <- route
    } else {
      kept[[length(kept) + 1]] <- t
    }
  }
  list(routes = routes, expr = dsl_join_terms(kept))
}

# Emit the ODE equation block: returns routes, derived assignments, and dx lines.
dsl_eqn_block <- function(fun) {
  exprs <- dsl_body_stmts(fun)
  routes <- list()
  derived <- character(0)
  dx_lines <- character(0)

  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort(c(
        "x" = "Only assignments are supported in the equation block for the DSL backend.",
        "i" = "Control-flow constructs such as {.code for} loops are not supported."
      ))
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "dx") {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in equation block.")
      }
      res <- dsl_extract_routes(rhs, idx)
      routes <- c(routes, res$routes)
      dx_lines <- c(dx_lines, sprintf("dx(x%d) = %s", idx, res$expr))
    } else {
      # Scalar (secondary/derived) assignment.
      name <- tolower(as.character(lhs))
      derived <- c(derived, sprintf("%s = %s", name, expr_to_dsl(rhs)))
    }
  }

  list(routes = routes, derived = derived, dx = dx_lines)
}

# Emit the output block: returns derived assignments and out() lines.
dsl_out_block <- function(fun) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  out_lines <- character(0)

  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the output block for the DSL backend.")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "y") {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in output block.")
      }
      # Pmetrics data uses 1-based OUTEQ labels, so output `y[i]` maps to the
      # DSL output label `outeq_{i}` (numeric label resolves to index i).
      out_lines <- c(out_lines, sprintf("out(outeq_%d) = %s", idx, expr_to_dsl(rhs)))
    } else {
      name <- tolower(as.character(lhs))
      derived <- c(derived, sprintf("%s = %s", name, expr_to_dsl(rhs)))
    }
  }

  list(derived = derived, out = out_lines)
}

# Emit `derive` assignments from a secondary-equation block.
dsl_sec_block <- function(fun) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the secondary block for the DSL backend.")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (!is.symbol(lhs)) {
      cli::cli_abort("Secondary equations must assign to scalar variables.")
    }
    derived <- c(derived, sprintf("%s = %s", tolower(as.character(lhs)), expr_to_dsl(rhs)))
  }
  derived
}

# Emit route-property modifiers (`lag(...)` / `fa(...)`) from a lag/fa block.
# `target` is the DSL property name ("lag" or "fa"); the R block assigns to
# `lag[j]` / `fa[j]` where `j` is the 1-based input index.
dsl_route_property_block <- function(fun, target) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  lines <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the {target} block for the DSL backend.")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != target) {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in {target} block.")
      }
      lines <- c(lines, sprintf("%s(input_%d) = %s", target, idx, expr_to_dsl(rhs)))
    } else {
      derived <- c(derived, sprintf("%s = %s", tolower(as.character(lhs)), expr_to_dsl(rhs)))
    }
  }
  list(derived = derived, lines = lines)
}

# Emit `init(...)` statements from an initial-conditions block.
dsl_ini_block <- function(fun) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  lines <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the initial-conditions block for the DSL backend.")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "x") {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in initial-conditions block.")
      }
      lines <- c(lines, sprintf("init(x%d) = %s", idx, expr_to_dsl(rhs)))
    } else {
      derived <- c(derived, sprintf("%s = %s", tolower(as.character(lhs)), expr_to_dsl(rhs)))
    }
  }
  list(derived = derived, lines = lines)
}

# Deduplicate routes, keeping declaration order, and detect conflicts (an input
# used as both bolus and infusion, or targeting different compartments).
dsl_finalize_routes <- function(routes) {
  by_input <- list()
  for (r in routes) {
    key <- as.character(r$input)
    if (is.null(by_input[[key]])) {
      by_input[[key]] <- r
    } else {
      prev <- by_input[[key]]
      if (prev$comp != r$comp) {
        cli::cli_abort(c(
          "x" = "Input {r$input} is directed to more than one compartment.",
          "i" = "Each drug input may target a single compartment in the DSL backend."
        ))
      }
      if (prev$kind != r$kind) {
        cli::cli_abort(c(
          "x" = "Input {r$input} is used as both a bolus and an infusion.",
          "i" = "The DSL backend requires each drug input to be either a bolus or an infusion, not both."
        ))
      }
    }
  }
  # Preserve ascending input order for deterministic output.
  inputs <- sort(as.integer(names(by_input)))
  lapply(inputs, function(i) by_input[[as.character(i)]])
}

# Map Pmetrics analytical library template names to DSL analytical structures.
dsl_analytical_structure <- function(tem) {
  dplyr::case_when(
    tem == "one_comp_iv" ~ "one_compartment",
    tem == "one_comp_iv_cl" ~ "one_compartment_cl",
    tem == "one_comp_bolus" ~ "one_compartment_with_absorption",
    tem == "one_comp_bolus_cl" ~ "one_compartment_cl_with_absorption",
    tem == "two_comp_iv" ~ "two_compartments",
    tem == "two_comp_iv_cl" ~ "two_compartments_cl",
    tem == "two_comp_bolus" ~ "two_compartments_with_absorption",
    tem == "two_comp_bolus_cl" ~ "two_compartments_cl_with_absorption",
    tem == "three_comp_iv" ~ "three_compartments",
    tem == "three_comp_iv_cl" ~ "three_compartments_cl",
    tem == "three_comp_bolus" ~ "three_compartments_with_absorption",
    tem == "three_comp_bolus_cl" ~ "three_compartments_cl_with_absorption",
    .default = NA_character_
  )
}

# Assemble the full pharmsol DSL text for a PM_model object.
model_to_dsl <- function(model) {
  arg_list <- model$arg_list
  model_list <- model$model_list
  if (is.null(arg_list) || is.null(model_list)) {
    cli::cli_abort("Model is not fully defined; cannot generate DSL source.")
  }

  type <- model_list$type
  parameters <- tolower(names(arg_list$pri))
  covariate_names <- tolower(names(arg_list$cov))

  header <- character(0)
  header <- c(header, sprintf("name = %s", if (is.null(model_list$name)) "user" else model_list$name))
  header <- c(header, sprintf("kind = %s", if (type == "Analytical") "analytical" else "ode"))
  header <- c(header, sprintf("params = %s", paste(parameters, collapse = ", ")))

  if (length(covariate_names) > 0) {
    cov_decls <- vapply(seq_along(covariate_names), function(i) {
      # arg_list$cov holds 1 for linear interpolation, 0 for carry-forward.
      if (isTRUE(unname(arg_list$cov[i]) == 0)) {
        paste0(covariate_names[i], "@carry_forward")
      } else {
        covariate_names[i]
      }
    }, character(1))
    header <- c(header, sprintf("covariates = %s", paste(cov_decls, collapse = ", ")))
  }

  # Derived (secondary) equations shared across blocks.
  derived <- character(0)
  if (!is.null(arg_list$sec)) {
    derived <- c(derived, dsl_sec_block(arg_list$sec))
  }

  if (type == "Analytical") {
    return(dsl_analytical(model, header, derived, parameters))
  }

  # ---- ODE model ----
  eqn <- dsl_eqn_block(arg_list$eqn)
  derived <- c(derived, eqn$derived)

  out <- dsl_out_block(arg_list$out)
  derived <- c(derived, out$derived)

  init_lines <- character(0)
  if (!is.null(arg_list$ini)) {
    ini <- dsl_ini_block(arg_list$ini)
    derived <- c(derived, ini$derived)
    init_lines <- ini$lines
  }

  lag_lines <- character(0)
  if (!is.null(arg_list$lag)) {
    lag <- dsl_route_property_block(arg_list$lag, "lag")
    derived <- c(derived, lag$derived)
    lag_lines <- lag$lines
  }

  fa_lines <- character(0)
  if (!is.null(arg_list$fa)) {
    fa <- dsl_route_property_block(arg_list$fa, "fa")
    derived <- c(derived, fa$derived)
    fa_lines <- fa$lines
  }

  # Number of states and outputs.
  n_states <- max(
    get_max_assignment_index(arg_list$eqn, "dx"),
    get_max_index(arg_list$eqn, "x"),
    if (!is.null(arg_list$ini)) get_max_index(arg_list$ini, "x") else 0L,
    get_max_index(arg_list$out, "x")
  )
  n_out <- get_max_assignment_index(arg_list$out, "y")

  states <- paste0("x", seq_len(n_states))
  outputs <- paste0("outeq_", seq_len(n_out))

  routes <- dsl_finalize_routes(eqn$routes)
  # Pmetrics data uses 1-based INPUT labels, so drug input `j` maps to the DSL
  # route label `input_{j}` (numeric label resolves to index j).
  route_lines <- vapply(routes, function(r) {
    sprintf("%s(input_%d) -> x%d", r$kind, r$input, r$comp)
  }, character(1))

  # Assemble the DSL text in an order that respects definite assignment:
  # declarations, routes, derived values, route properties, initial conditions,
  # derivatives, and finally outputs.
  lines <- c(
    header,
    sprintf("states = %s", paste(states, collapse = ", ")),
    sprintf("outputs = %s", paste(outputs, collapse = ", ")),
    "",
    route_lines,
    if (length(route_lines) > 0) "" else NULL,
    derived,
    if (length(derived) > 0) "" else NULL,
    lag_lines,
    fa_lines,
    if (length(lag_lines) > 0 || length(fa_lines) > 0) "" else NULL,
    init_lines,
    if (length(init_lines) > 0) "" else NULL,
    eqn$dx,
    "",
    out$out
  )

  paste(lines, collapse = "\n")
}

# Assemble DSL text for an analytical (library-structure) model.
dsl_analytical <- function(model, header, derived, parameters) {
  arg_list <- model$arg_list
  template <- model$arg_list$tem
  if (is.null(template)) {
    template <- attr(model, "model_template")
  }
  tem_name <- model$model_list$name
  structure <- dsl_analytical_structure(tem_name)
  if (is.na(structure)) {
    cli::cli_abort(c(
      "x" = "Analytical model template {.val {tem_name}} is not supported by the DSL backend.",
      "i" = "See {.fn model_lib} for supported templates."
    ))
  }

  out <- dsl_out_block(arg_list$out)
  derived <- c(derived, out$derived)

  # Determine the number of compartments the structure requires.
  n_states <- dsl_analytical_state_count(structure)
  states <- paste0("x", seq_len(n_states))
  n_out <- get_max_assignment_index(arg_list$out, "y")
  outputs <- paste0("outeq_", seq_len(n_out))

  lines <- c(
    header,
    sprintf("structure = %s", structure),
    sprintf("states = %s", paste(states, collapse = ", ")),
    sprintf("outputs = %s", paste(outputs, collapse = ", ")),
    "",
    derived,
    if (length(derived) > 0) "" else NULL,
    out$out
  )

  paste(lines, collapse = "\n")
}

# Number of states (compartments) for a DSL analytical structure.
dsl_analytical_state_count <- function(structure) {
  dplyr::case_when(
    stringr::str_starts(structure, "one_compartment") ~ 1L + as.integer(stringr::str_detect(structure, "absorption")),
    stringr::str_starts(structure, "two_compartments") ~ 2L + as.integer(stringr::str_detect(structure, "absorption")),
    stringr::str_starts(structure, "three_compartments") ~ 3L + as.integer(stringr::str_detect(structure, "absorption")),
    .default = NA_integer_
  )
}
