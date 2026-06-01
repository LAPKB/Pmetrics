statement_exprs <- function(fun_or_expr) {
  if (is.null(fun_or_expr)) {
    return(list())
  }

  expr <- if (is.function(fun_or_expr)) body(fun_or_expr) else fun_or_expr
  if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    return(as.list(expr[-1]))
  }

  list(expr)
}


reserved_name_conflicts <- function(blocks) {
  reserved <- c("t", "x", "dx", "p", "b", "bolus", "r", "rateiv", "cov", "y")

  block_lines <- function(block) {
    if (is.null(block)) {
      return(character(0))
    }

    if (is.function(block)) {
      return(tolower(func_to_char(block)))
    }

    if (is.environment(block)) {
      return(character(0))
    }

    if (is.list(block)) {
      block_names <- names(block)
      block_names <- if (is.null(block_names)) character(0) else tolower(block_names)
      return(c(block_names, unlist(lapply(block, block_lines), use.names = FALSE)))
    }

    if (!is.atomic(block) && !is.character(block)) {
      return(character(0))
    }

    tolower(as.character(block))
  }

  conflicts <- unique(unlist(lapply(blocks, function(block) {
    lines <- block_lines(block)
    if (length(lines) == 0) {
      return(character(0))
    }

    unlist(lapply(reserved, function(name) {
      has_match <- name %in% lines || any(stringr::str_detect(lines, paste0("^", name, "\\s+[=<]")))
      if (has_match) {
        return(name)
      }
      NA_character_
    }), use.names = FALSE)
  }), use.names = FALSE))

  conflicts[!is.na(conflicts)]
}


expr_text <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = "\n")
}


index_vector_size <- function(max_index) {
  if (max_index <= 0L) {
    return(1L)
  }

  max_index + 1L
}


get_max_index <- function(fn_or_expr, targets) {
  targets <- tolower(targets)

  walk_expr <- function(expr) {
    if (is.call(expr) && identical(expr[[1]], as.name("["))) {
      target_name <- tolower(as.character(expr[[2]]))
      if (target_name %in% targets) {
        idx <- expr[[3]]
        if (is.numeric(idx) && length(idx) == 1) {
          return(as.integer(idx))
        }
      }
    }

    if (is.call(expr)) {
      return(max(vapply(as.list(expr), walk_expr, integer(1)), 0L))
    }

    0L
  }
  expr <- if (is.function(fn_or_expr)) body(fn_or_expr) else fn_or_expr
  walk_expr(expr)
}


collect_index_refs <- function(fn_or_expr, targets) {
  targets <- tolower(targets)
  found <- integer(0)

  walk_expr <- function(expr) {
    if (!is.call(expr)) {
      return(invisible(NULL))
    }

    if (identical(expr[[1]], as.name("["))) {
      target_name <- tolower(as.character(expr[[2]]))
      idx <- expr[[3]]
      if (target_name %in% targets && is.numeric(idx) && length(idx) == 1) {
        found <<- c(found, as.integer(idx))
      }
    }

    lapply(as.list(expr), walk_expr)
    invisible(NULL)
  }

  expr <- if (is.function(fn_or_expr)) body(fn_or_expr) else fn_or_expr
  walk_expr(expr)
  sort(unique(found))
}


get_assignment_indices <- function(fn_or_expr, targets) {
  targets <- tolower(targets)
  found <- integer(0)

  walk_expr <- function(expr) {
    if (!is.call(expr)) {
      return(invisible(NULL))
    }

    if (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("="))) {
      lhs <- expr[[2]]
      if (is.call(lhs) && identical(lhs[[1]], as.name("["))) {
        target_name <- tolower(as.character(lhs[[2]]))
        idx <- lhs[[3]]
        if (target_name %in% targets && is.numeric(idx) && length(idx) == 1) {
          found <<- c(found, as.integer(idx))
        }
      }
    }

    lapply(as.list(expr), walk_expr)
    invisible(NULL)
  }

  expr <- if (is.function(fn_or_expr)) body(fn_or_expr) else fn_or_expr
  walk_expr(expr)
  sort(unique(found))
}


get_assignments <- function(fn_or_expr, targets) {
  length(get_assignment_indices(fn_or_expr, targets))
}


get_max_assignment_index <- function(fn_or_expr, targets) {
  found <- get_assignment_indices(fn_or_expr, targets)
  if (length(found) == 0) {
    return(0L)
  }

  max(found)
}


has_nonliteral_index <- function(fn_or_expr, targets) {
  targets <- tolower(targets)

  walk_expr <- function(expr) {
    if (is.call(expr) && identical(expr[[1]], as.name("["))) {
      target_name <- tolower(as.character(expr[[2]]))
      if (target_name %in% targets) {
        idx <- expr[[3]]
        return(!(is.numeric(idx) && length(idx) == 1))
      }
    }

    if (is.call(expr)) {
      return(any(vapply(as.list(expr), walk_expr, logical(1))))
    }

    FALSE
  }

  expr <- if (is.function(fn_or_expr)) body(fn_or_expr) else fn_or_expr
  walk_expr(expr)
}


collect_assigned_names_from_exprs <- function(exprs) {
  found <- character(0)

  walk_expr <- function(expr) {
    if (!is.call(expr)) {
      return(invisible(NULL))
    }

    op <- as.character(expr[[1]])
    if (op %in% c("<-", "=")) {
      lhs <- expr[[2]]
      if (is.symbol(lhs)) {
        found <<- c(found, tolower(as.character(lhs)))
      }
    }

    lapply(as.list(expr), walk_expr)
    invisible(NULL)
  }

  lapply(exprs, walk_expr)
  unique(found)
}


collect_assigned_names <- function(fun_or_expr) {
  collect_assigned_names_from_exprs(statement_exprs(fun_or_expr))
}


normalize_ode_solver <- function(solver) {
  if (!is.character(solver) || length(solver) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg solver} must be a single character value.",
      "i" = "Supported values are 'BDF', 'TRBDF2', 'ESDIRK34', and 'TSIT45'."
    ))
  }

  switch(tolower(solver),
    bdf = "BDF",
    trbdf2 = "TRBDF2",
    esdirk34 = "ESDIRK34",
    tsit45 = "TSIT45",
    cli::cli_abort(c(
      "x" = "Unsupported {.arg solver} value: {.val {solver}}.",
      "i" = "Supported values are 'BDF', 'TRBDF2', 'ESDIRK34', and 'TSIT45'."
    ))
  )
}


dsl_covariate_declarations <- function(covariates) {
  if (is.null(covariates) || length(covariates) == 0) {
    return(character(0))
  }

  purrr::imap_chr(covariates, \(interpolation, name) {
    annotation <- if (identical(interpolation, 0)) "@locf" else "@linear"
    paste0(tolower(name), annotation)
  })
}


dsl_analytical_spec <- function(template_name) {
  switch(tolower(template_name),
    one_comp_iv = list(
      structure = "one_compartment",
      states = c("central"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "central"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    one_comp_iv_cl = list(
      structure = "one_compartment_cl",
      states = c("central"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "central"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    one_comp_bolus = list(
      structure = "one_compartment_with_absorption",
      states = c("gut", "central"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "gut"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    one_comp_bolus_cl = list(
      structure = "one_compartment_cl_with_absorption",
      states = c("gut", "central"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "gut"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    two_comp_iv = list(
      structure = "two_compartments",
      params = c("ke", "kcp", "kpc", "v"),
      symbols = c(k12 = "kcp", k21 = "kpc"),
      states = c("central", "peripheral"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "central"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    two_comp_iv_cl = list(
      structure = "two_compartments_cl",
      params = c("cl", "q", "vc", "vp"),
      symbols = c(v1 = "vc", v2 = "vp"),
      states = c("central", "peripheral"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "central"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    two_comp_bolus = list(
      structure = "two_compartments_with_absorption",
      params = c("ke", "ka", "kcp", "kpc", "v"),
      symbols = c(k23 = "kcp", k32 = "kpc"),
      states = c("gut", "central", "peripheral"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "gut"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    two_comp_bolus_cl = list(
      structure = "two_compartments_cl_with_absorption",
      params = c("ka", "cl", "q", "vc", "vp"),
      symbols = c(v2 = "vc", v3 = "vp"),
      states = c("gut", "central", "peripheral"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "gut"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    three_comp_iv = list(
      structure = "three_compartments",
      params = c("k10", "k12", "k13", "k21", "k31", "v"),
      symbols = c(ke = "k10"),
      states = c("central", "peripheral_1", "peripheral_2"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "central"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    three_comp_iv_cl = list(
      structure = "three_compartments_cl",
      params = c("cl", "q2", "q3", "vc", "v2", "v3"),
      symbols = c(v1 = "vc"),
      states = c("central", "peripheral_1", "peripheral_2"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "central"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    three_comp_bolus = list(
      structure = "three_compartments_with_absorption",
      params = c("ka", "k10", "k12", "k13", "k21", "k31", "v"),
      symbols = c(ke = "k10", k23 = "k12", k24 = "k13", k32 = "k21", k42 = "k31"),
      states = c("gut", "central", "peripheral_1", "peripheral_2"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "gut"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    three_comp_bolus_cl = list(
      structure = "three_compartments_cl_with_absorption",
      params = c("ka", "cl", "q2", "q3", "vc", "v2", "v3"),
      symbols = c(q3 = "q2", q4 = "q3", v2 = "vc", v3 = "v2", v4 = "v3"),
      states = c("gut", "central", "peripheral_1", "peripheral_2"),
      routes = list(
        list(kind = "bolus", input = 1L, destination = "gut"),
        list(kind = "infusion", input = 1L, destination = "central")
      )
    ),
    cli::cli_abort(c(
      "x" = "Unsupported analytical model template: {.val {template_name}}."
    ))
  )
}


is_template_statement <- function(expr, template_name) {
  expr_name <- tolower(expr_text(expr))
  if (expr_name == tolower(template_name)) {
    return(TRUE)
  }

  alt_names <- alt_mod_lib_names() |>
    dplyr::filter(primary == template_name) |>
    dplyr::pull(alt) |>
    tolower()

  expr_name %in% alt_names
}


collect_route_usage <- function(fun_or_expr, state_names) {
  usage <- list()
  exprs <- if (is.list(fun_or_expr) && !is.function(fun_or_expr)) {
    fun_or_expr
  } else {
    statement_exprs(fun_or_expr)
  }

  register_usage <- function(kind, input_index, destination) {
    key <- paste(kind, input_index, sep = ":")
    existing <- usage[[key]]
    if (!is.null(existing) && !identical(existing$destination, destination)) {
      cli::cli_abort(c(
        "x" = "Input {.val {input_index}} is routed to multiple destinations in the same model.",
        "i" = "Each bolus or infusion input must target exactly one state in the DSL lowering."
      ))
    }

    usage[[key]] <<- list(kind = kind, input = as.integer(input_index), destination = destination)
  }

  for (expr in exprs) {
    if (!is.call(expr) || !as.character(expr[[1]]) %in% c("<-", "=")) {
      next
    }

    lhs <- expr[[2]]
    if (!is.call(lhs) || !identical(lhs[[1]], as.name("["))) {
      next
    }

    target <- tolower(as.character(lhs[[2]]))
    idx <- lhs[[3]]
    if (target != "dx" || !is.numeric(idx) || length(idx) != 1) {
      next
    }

    destination <- unname(state_names[[as.character(as.integer(idx))]])
    rhs <- expr[[3]]

    for (input_index in collect_index_refs(rhs, c("b", "bolus"))) {
      register_usage("bolus", input_index, destination)
    }

    for (input_index in collect_index_refs(rhs, c("r", "rateiv"))) {
      register_usage("infusion", input_index, destination)
    }
  }

  usage <- unname(usage)
  if (length(usage) == 0) {
    return(list())
  }

  usage[order(
    vapply(usage, \(item) item$input, integer(1)),
    match(vapply(usage, \(item) item$kind, character(1)), c("bolus", "infusion"))
  )]
}


lookup_index_name <- function(mapping, index, label) {
  key <- as.character(as.integer(index))
  if (!key %in% names(mapping)) {
    cli::cli_abort(c(
      "x" = "Unknown {label} index {.val {index}} in model DSL lowering."
    ))
  }

  unname(mapping[[key]])
}


route_mapping_key <- function(kind, index) {
  paste(tolower(kind), as.integer(index), sep = ":")
}


build_route_name_map <- function(routes) {
  if (length(routes) == 0) {
    return(setNames(character(0), character(0)))
  }

  route_inputs <- vapply(routes, \(route) as.character(as.integer(route$input)), character(1))
  input_counts <- table(route_inputs)
  route_names <- vapply(routes, \(route) {
    input <- as.integer(route$input)
    input_key <- as.character(input)
    if (input_counts[[input_key]] > 1L) {
      sprintf("input_%s_%s", input, route$kind)
    } else {
      sprintf("input_%s", input)
    }
  }, character(1))

  names(route_names) <- vapply(routes, \(route) route_mapping_key(route$kind, route$input), character(1))
  route_names
}


lookup_route_name <- function(mapping, index, kind) {
  key <- route_mapping_key(kind, index)
  if (!key %in% names(mapping)) {
    cli::cli_abort(c(
      "x" = "Unknown {kind} input index {.val {index}} in model DSL lowering."
    ))
  }

  unname(mapping[[key]])
}


lookup_symbol_name <- function(mappings, symbol_name) {
  symbol_name <- tolower(symbol_name)
  symbol_map <- mappings$symbols %||% character(0)

  if (symbol_name %in% names(symbol_map)) {
    return(unname(symbol_map[[symbol_name]]))
  }

  symbol_name
}


rename_declared_names <- function(names, symbol_map) {
  if (length(names) == 0 || length(symbol_map) == 0) {
    return(names)
  }

  unique(vapply(names, function(name) {
    name <- tolower(name)
    if (name %in% names(symbol_map)) {
      return(unname(symbol_map[[name]]))
    }

    name
  }, character(1)))
}


pm_model_route_mapping <- function(model) {
  kind <- tolower(model$model_list$type)

  routes <- if (kind == "ode") {
    state_names <- setNames(
      paste0("state_", seq_len(model$model_list$n_eqn)),
      as.character(seq_len(model$model_list$n_eqn))
    )
    collect_route_usage(statement_exprs(model$arg_list$eqn), state_names)
  } else if (kind == "analytical") {
    dsl_analytical_spec(model$model_list$name)$routes
  } else {
    cli::cli_abort(c("x" = "Unsupported model kind for route lowering: {.val {model$model_list$type}}."))
  }

  list(routes = routes, names = build_route_name_map(routes))
}


write_runtime_data_matrix <- function(data, file_path) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  os_type <- getOS()
  eol <- c("\r\n", "\n", "\r\n")[os_type]
  output <- file(file_path, "w")
  on.exit(close(output), add = TRUE)

  writeLines(toupper(names(data)[-ncol(data)]), output, sep = getPMoptions("sep"))
  writeLines(toupper(names(data)[ncol(data)]), output)
  utils::write.table(
    data,
    output,
    row.names = FALSE,
    na = ".",
    quote = FALSE,
    sep = getPMoptions("sep"),
    dec = getPMoptions("dec"),
    col.names = FALSE,
    eol = eol
  )

  invisible(file_path)
}


rewrite_runtime_route_labels <- function(file_path, model) {
  route_mapping <- pm_model_route_mapping(model)$names
  if (length(route_mapping) == 0 || !file.exists(file_path)) {
    return(invisible(file_path))
  }

  runtime_data <- PMreadMatrix(file_path, quiet = TRUE)

  if (!all(c("evid", "dur", "input") %in% names(runtime_data))) {
    return(invisible(file_path))
  }

  evid <- suppressWarnings(as.numeric(runtime_data$evid))
  dur <- suppressWarnings(as.numeric(runtime_data$dur))
  input_labels <- as.character(runtime_data$input)
  dose_rows <- !is.na(evid) & evid != 0 & !is.na(input_labels)

  for (row in which(dose_rows)) {
    input_index <- suppressWarnings(as.integer(as.numeric(input_labels[[row]])))
    if (is.na(input_index)) {
      next
    }

    route_kind <- if (!is.na(dur[[row]]) && dur[[row]] > 0) "infusion" else "bolus"
    route_key <- route_mapping_key(route_kind, input_index)
    if (route_key %in% names(route_mapping)) {
      runtime_data$input[[row]] <- route_mapping[[route_key]]
    }
  }

  write_runtime_data_matrix(runtime_data, file_path)
  invisible(file_path)
}


replace_route_refs <- function(expr) {
  if (!is.call(expr)) {
    return(expr)
  }

  if (identical(expr[[1]], as.name("["))) {
    target <- tolower(as.character(expr[[2]]))
    if (target %in% c("b", "bolus", "r", "rateiv")) {
      return(0)
    }
  }

  as.call(lapply(as.list(expr), replace_route_refs))
}


format_dsl_number <- function(value) {
  format(value, scientific = FALSE, trim = TRUE, digits = 15)
}


new_pm_dsl <- function(source) {
  structure(as.character(source), class = c("pm_dsl", "character"))
}


#' @method print pm_dsl
#' @keywords internal
#' @noRd
#' @export
print.pm_dsl <- function(x, ...) {
  writeLines(unclass(x))
  invisible(x)
}


is_numeric_literal <- function(expr, value = NULL) {
  if (!is.numeric(expr) || length(expr) != 1) {
    return(FALSE)
  }

  if (is.null(value)) {
    return(TRUE)
  }

  isTRUE(all.equal(as.numeric(expr), as.numeric(value)))
}


is_zero_literal <- function(expr) {
  is_numeric_literal(expr, 0)
}


is_one_literal <- function(expr) {
  is_numeric_literal(expr, 1)
}


simplify_dsl_expr <- function(expr) {
  if (!is.call(expr)) {
    return(expr)
  }

  op <- as.character(expr[[1]])
  args <- lapply(as.list(expr[-1]), simplify_dsl_expr)

  if (op == "(") {
    return(args[[1]])
  }

  if (op == "+") {
    if (length(args) == 1) {
      return(args[[1]])
    }

    lhs <- args[[1]]
    rhs <- args[[2]]
    if (is_zero_literal(lhs)) {
      return(rhs)
    }
    if (is_zero_literal(rhs)) {
      return(lhs)
    }

    return(as.call(list(as.name("+"), lhs, rhs)))
  }

  if (op == "-") {
    if (length(args) == 1) {
      value <- args[[1]]
      if (is_zero_literal(value)) {
        return(0)
      }

      return(as.call(list(as.name("-"), value)))
    }

    lhs <- args[[1]]
    rhs <- args[[2]]
    if (is_zero_literal(rhs)) {
      return(lhs)
    }
    if (is_zero_literal(lhs)) {
      return(as.call(list(as.name("-"), rhs)))
    }

    return(as.call(list(as.name("-"), lhs, rhs)))
  }

  if (op == "*") {
    lhs <- args[[1]]
    rhs <- args[[2]]
    if (is_zero_literal(lhs) || is_zero_literal(rhs)) {
      return(0)
    }
    if (is_one_literal(lhs)) {
      return(rhs)
    }
    if (is_one_literal(rhs)) {
      return(lhs)
    }

    return(as.call(list(as.name("*"), lhs, rhs)))
  }

  if (op == "/") {
    lhs <- args[[1]]
    rhs <- args[[2]]
    if (is_zero_literal(lhs)) {
      return(0)
    }
    if (is_one_literal(rhs)) {
      return(lhs)
    }

    return(as.call(list(as.name("/"), lhs, rhs)))
  }

  if (op == "^") {
    lhs <- args[[1]]
    rhs <- args[[2]]
    if (is_one_literal(rhs)) {
      return(lhs)
    }
    if (is_zero_literal(rhs)) {
      return(1)
    }

    return(as.call(list(as.name("^"), lhs, rhs)))
  }

  as.call(c(expr[[1]], args))
}


dsl_operator_precedence <- function(expr) {
  if (!is.call(expr)) {
    return(Inf)
  }

  op <- as.character(expr[[1]])
  switch(op,
    "if" = 1L,
    "ifelse" = 1L,
    "|" = 2L,
    "||" = 2L,
    "&" = 3L,
    "&&" = 3L,
    "==" = 4L,
    "!=" = 4L,
    ">=" = 4L,
    "<=" = 4L,
    ">" = 4L,
    "<" = 4L,
    "+" = if (length(expr) == 2) 7L else 5L,
    "-" = if (length(expr) == 2) 7L else 5L,
    "*" = 6L,
    "/" = 6L,
    "!" = 7L,
    "^" = 8L,
    "(" = 9L,
    Inf
  )
}


needs_dsl_parentheses <- function(expr, parent_precedence, side = "left", parent_op = NULL) {
  if (!is.call(expr)) {
    return(FALSE)
  }

  child_precedence <- dsl_operator_precedence(expr)
  if (is.infinite(child_precedence)) {
    return(FALSE)
  }

  if (child_precedence < parent_precedence) {
    return(TRUE)
  }

  if (child_precedence > parent_precedence) {
    return(FALSE)
  }

  if (side == "right" && parent_op %in% c("-", "/", "^")) {
    return(TRUE)
  }

  if (side == "left" && identical(parent_op, "^")) {
    return(TRUE)
  }

  FALSE
}


render_dsl_child <- function(expr, mappings, parent_precedence, side = "left", parent_op = NULL) {
  expr <- simplify_dsl_expr(expr)
  text <- expr_to_dsl(expr, mappings)

  if (needs_dsl_parentheses(expr, parent_precedence, side = side, parent_op = parent_op)) {
    return(sprintf("(%s)", text))
  }

  text
}


expr_to_dsl <- function(expr, mappings) {
  expr <- simplify_dsl_expr(expr)

  if (is.null(expr)) {
    return("")
  }

  if (is.logical(expr) && length(expr) == 1) {
    return(tolower(as.character(expr)))
  }

  if (is.numeric(expr) && length(expr) == 1) {
    return(format_dsl_number(expr))
  }

  if (is.symbol(expr)) {
    return(lookup_symbol_name(mappings, as.character(expr)))
  }

  if (!is.call(expr)) {
    cli::cli_abort(c(
      "x" = "Unsupported expression in DSL lowering.",
      "i" = expr_text(expr)
    ))
  }

  if (identical(expr[[1]], as.name("["))) {
    target <- tolower(as.character(expr[[2]]))
    index <- expr[[3]]

    if (!is.numeric(index) || length(index) != 1) {
      cli::cli_abort(c(
        "x" = "Dynamic indices are not supported in DSL lowering.",
        "i" = expr_text(expr)
      ))
    }

    index <- as.integer(index)
    return(switch(target,
      x = lookup_index_name(mappings$states, index, "state"),
      y = lookup_index_name(mappings$outputs, index, "output"),
      lag = sprintf("lag(%s)", lookup_route_name(mappings$routes, index, "bolus")),
      fa = sprintf("fa(%s)", lookup_route_name(mappings$routes, index, "bolus")),
      r = sprintf("rate(%s)", lookup_route_name(mappings$routes, index, "infusion")),
      rateiv = sprintf("rate(%s)", lookup_route_name(mappings$routes, index, "infusion")),
      b = cli::cli_abort(c("x" = "Bolus inputs must be lowered as routes, not inline expressions.")),
      bolus = cli::cli_abort(c("x" = "Bolus inputs must be lowered as routes, not inline expressions.")),
      sprintf("%s[%s]", target, index)
    ))
  }

  op <- as.character(expr[[1]])
  args <- as.list(expr[-1])

  switch(op,
    "(" = expr_to_dsl(args[[1]], mappings),
    "+" = if (length(args) == 1) {
      expr_to_dsl(args[[1]], mappings)
    } else {
      precedence <- 5L
      sprintf(
        "%s + %s",
        render_dsl_child(args[[1]], mappings, precedence, side = "left", parent_op = op),
        render_dsl_child(args[[2]], mappings, precedence, side = "right", parent_op = op)
      )
    },
    "-" = if (length(args) == 1) {
      precedence <- 7L
      sprintf("-%s", render_dsl_child(args[[1]], mappings, precedence, side = "right", parent_op = op))
    } else {
      precedence <- 5L
      sprintf(
        "%s - %s",
        render_dsl_child(args[[1]], mappings, precedence, side = "left", parent_op = op),
        render_dsl_child(args[[2]], mappings, precedence, side = "right", parent_op = op)
      )
    },
    "*" = {
      precedence <- 6L
      sprintf(
        "%s * %s",
        render_dsl_child(args[[1]], mappings, precedence, side = "left", parent_op = op),
        render_dsl_child(args[[2]], mappings, precedence, side = "right", parent_op = op)
      )
    },
    "/" = {
      precedence <- 6L
      sprintf(
        "%s / %s",
        render_dsl_child(args[[1]], mappings, precedence, side = "left", parent_op = op),
        render_dsl_child(args[[2]], mappings, precedence, side = "right", parent_op = op)
      )
    },
    "^" = sprintf(
      "pow(%s, %s)",
      render_dsl_child(args[[1]], mappings, 8L, side = "left", parent_op = op),
      render_dsl_child(args[[2]], mappings, 8L, side = "right", parent_op = op)
    ),
    "==" = sprintf("%s == %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "!=" = sprintf("%s != %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    ">=" = sprintf("%s >= %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "<=" = sprintf("%s <= %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    ">" = sprintf("%s > %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "<" = sprintf("%s < %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "&" = sprintf("%s && %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "&&" = sprintf("%s && %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "|" = sprintf("%s || %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "||" = sprintf("%s || %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "!" = sprintf("!%s", render_dsl_child(args[[1]], mappings, 7L, side = "right", parent_op = op)),
    "abs" = sprintf("abs(%s)", expr_to_dsl(args[[1]], mappings)),
    "ln" = sprintf("log(%s)", expr_to_dsl(args[[1]], mappings)),
    "log" = sprintf("log(%s)", expr_to_dsl(args[[1]], mappings)),
    "log10" = sprintf("log10(%s)", expr_to_dsl(args[[1]], mappings)),
    "log2" = sprintf("log2(%s)", expr_to_dsl(args[[1]], mappings)),
    "exp" = sprintf("exp(%s)", expr_to_dsl(args[[1]], mappings)),
    "sqrt" = sprintf("sqrt(%s)", expr_to_dsl(args[[1]], mappings)),
    "sin" = sprintf("sin(%s)", expr_to_dsl(args[[1]], mappings)),
    "cos" = sprintf("cos(%s)", expr_to_dsl(args[[1]], mappings)),
    "tan" = sprintf("tan(%s)", expr_to_dsl(args[[1]], mappings)),
    "asin" = sprintf("asin(%s)", expr_to_dsl(args[[1]], mappings)),
    "acos" = sprintf("acos(%s)", expr_to_dsl(args[[1]], mappings)),
    "atan" = sprintf("atan(%s)", expr_to_dsl(args[[1]], mappings)),
    "atan2" = sprintf("atan2(%s, %s)", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings)),
    "sinh" = sprintf("sinh(%s)", expr_to_dsl(args[[1]], mappings)),
    "cosh" = sprintf("cosh(%s)", expr_to_dsl(args[[1]], mappings)),
    "tanh" = sprintf("tanh(%s)", expr_to_dsl(args[[1]], mappings)),
    "asinh" = sprintf("asinh(%s)", expr_to_dsl(args[[1]], mappings)),
    "acosh" = sprintf("acosh(%s)", expr_to_dsl(args[[1]], mappings)),
    "atanh" = sprintf("atanh(%s)", expr_to_dsl(args[[1]], mappings)),
    "floor" = sprintf("floor(%s)", expr_to_dsl(args[[1]], mappings)),
    "ceiling" = sprintf("ceil(%s)", expr_to_dsl(args[[1]], mappings)),
    "round" = sprintf("round(%s)", expr_to_dsl(args[[1]], mappings)),
    "trunc" = sprintf("trunc(%s)", expr_to_dsl(args[[1]], mappings)),
    "if" = {
      else_branch <- if (length(args) >= 3) expr_to_dsl(args[[3]], mappings) else "0"
      sprintf("if (%s) %s else %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings), else_branch)
    },
    "ifelse" = sprintf("if (%s) %s else %s", expr_to_dsl(args[[1]], mappings), expr_to_dsl(args[[2]], mappings), expr_to_dsl(args[[3]], mappings)),
    sprintf("%s(%s)", tolower(op), paste(vapply(args, expr_to_dsl, character(1), mappings = mappings), collapse = ", "))
  )
}


indent_lines <- function(lines, spaces = 2) {
  if (length(lines) == 0) {
    return(character(0))
  }

  prefix <- strrep(" ", spaces)
  paste0(prefix, lines)
}


translate_statement_block <- function(expr, context, mappings) {
  if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    return(unlist(lapply(as.list(expr[-1]), translate_statement, context = context, mappings = mappings), use.names = FALSE))
  }

  translate_statement(expr, context = context, mappings = mappings)
}


translate_statement <- function(expr, context, mappings) {
  if (is.symbol(expr)) {
    return(character(0))
  }

  if (!is.call(expr)) {
    cli::cli_abort(c(
      "x" = "Unsupported statement in DSL lowering.",
      "i" = expr_text(expr)
    ))
  }

  op <- as.character(expr[[1]])

  if (op == "{") {
    return(translate_statement_block(expr, context = context, mappings = mappings))
  }

  if (op == "if") {
    condition <- expr_to_dsl(expr[[2]], mappings)
    then_lines <- translate_statement_block(expr[[3]], context = context, mappings = mappings)
    else_lines <- if (length(expr) >= 4) {
      translate_statement_block(expr[[4]], context = context, mappings = mappings)
    } else {
      character(0)
    }

    return(c(
      sprintf("if (%s) {", condition),
      indent_lines(then_lines),
      if (length(else_lines) > 0) {
        c("} else {", indent_lines(else_lines), "}")
      } else {
        "}"
      }
    ))
  }

  if (op == "for") {
    cli::cli_abort(c(
      "x" = "For loops are not supported in DSL lowering.",
      "i" = expr_text(expr)
    ))
  }

  if (!op %in% c("<-", "=")) {
    cli::cli_abort(c(
      "x" = "Unsupported statement in DSL lowering.",
      "i" = expr_text(expr)
    ))
  }

  lhs <- expr[[2]]
  rhs <- expr[[3]]

  if (is.symbol(lhs)) {
    return(sprintf("%s = %s", lookup_symbol_name(mappings, as.character(lhs)), expr_to_dsl(rhs, mappings)))
  }

  if (!is.call(lhs) || !identical(lhs[[1]], as.name("["))) {
    cli::cli_abort(c(
      "x" = "Unsupported assignment target in DSL lowering.",
      "i" = expr_text(expr)
    ))
  }

  target <- tolower(as.character(lhs[[2]]))
  index <- lhs[[3]]
  if (!is.numeric(index) || length(index) != 1) {
    cli::cli_abort(c(
      "x" = "Dynamic indices are not supported in DSL lowering.",
      "i" = expr_text(expr)
    ))
  }

  index <- as.integer(index)
  switch(context,
    ode = {
      if (target != "dx") {
        cli::cli_abort(c("x" = "Only {.code dx[i]} assignments are allowed in the ODE block after DSL lowering."))
      }

      return(sprintf(
        "dx(%s) = %s",
        lookup_index_name(mappings$states, index, "state"),
        expr_to_dsl(replace_route_refs(rhs), mappings)
      ))
    },
    derive = cli::cli_abort(c("x" = "Indexed assignments are not allowed in the derived block after DSL lowering.")),
    ini = {
      if (target != "x") {
        cli::cli_abort(c("x" = "Only {.code x[i]} assignments are allowed in the initial conditions block after DSL lowering."))
      }

      return(sprintf(
        "init(%s) = %s",
        lookup_index_name(mappings$states, index, "state"),
        expr_to_dsl(rhs, mappings)
      ))
    },
    lag = {
      if (target != "lag") {
        cli::cli_abort(c("x" = "Only {.code lag[i]} assignments are allowed in the lag block after DSL lowering."))
      }

      return(sprintf(
        "lag(%s) = %s",
        lookup_route_name(mappings$routes, index, "bolus"),
        expr_to_dsl(rhs, mappings)
      ))
    },
    fa = {
      if (target != "fa") {
        cli::cli_abort(c("x" = "Only {.code fa[i]} assignments are allowed in the bioavailability block after DSL lowering."))
      }

      return(sprintf(
        "fa(%s) = %s",
        lookup_route_name(mappings$routes, index, "bolus"),
        expr_to_dsl(rhs, mappings)
      ))
    },
    out = {
      if (target != "y") {
        cli::cli_abort(c("x" = "Only {.code y[i]} assignments are allowed in the output block after DSL lowering."))
      }

      return(sprintf(
        "out(%s) = %s ~ continuous()",
        lookup_index_name(mappings$outputs, index, "output"),
        expr_to_dsl(rhs, mappings)
      ))
    },
    cli::cli_abort(c("x" = "Unknown DSL lowering context: {.val {context}}."))
  )
}


translate_statements <- function(fun_or_exprs, context, mappings) {
  exprs <- if (is.list(fun_or_exprs) && !is.function(fun_or_exprs)) {
    fun_or_exprs
  } else {
    statement_exprs(fun_or_exprs)
  }

  lines <- unlist(lapply(exprs, translate_statement, context = context, mappings = mappings), use.names = FALSE)
  lines[nzchar(lines)]
}


build_route_declarations <- function(route_usage, route_names) {
  if (length(route_usage) == 0) {
    return(character(0))
  }

  purrr::map_chr(route_usage, \(route) {
    sprintf(
      "%s(%s) -> %s",
      route$kind,
      lookup_route_name(route_names, route$input, route$kind),
      route$destination
    )
  })
}


model_derived_names <- function(arg_list, eqn_exprs, excluded_names) {
  derived <- unique(c(
    collect_assigned_names(arg_list$sec),
    collect_assigned_names_from_exprs(eqn_exprs),
    collect_assigned_names(arg_list$ini),
    collect_assigned_names(arg_list$lag),
    collect_assigned_names(arg_list$fa),
    collect_assigned_names(arg_list$out)
  ))

  derived[!derived %in% excluded_names]
}


pm_model_to_dsl_source <- function(model) {
  if (!inherits(model, "PM_model")) {
    cli::cli_abort(c("x" = "DSL lowering requires a {.cls PM_model} object."))
  }

  if (is.null(model$model_list)) {
    cli::cli_abort(c("x" = "Model metadata is empty.", "i" = "Please create a valid {.cls PM_model} first."))
  }

  kind <- tolower(model$model_list$type)
  params <- tolower(model$model_list$parameters)
  covariate_names <- tolower(names(model$arg_list$cov %||% list()))
  covariate_declarations <- dsl_covariate_declarations(model$arg_list$cov)
  output_indices <- get_assignment_indices(model$arg_list$out, "y")
  output_names <- setNames(paste0("outeq_", output_indices), as.character(output_indices))

  if (length(output_names) == 0) {
    cli::cli_abort(c("x" = "Model DSL lowering requires at least one output equation."))
  }

  if (kind == "ode") {
    state_names <- setNames(
      paste0("state_", seq_len(model$model_list$n_eqn)),
      as.character(seq_len(model$model_list$n_eqn))
    )
    eqn_exprs <- statement_exprs(model$arg_list$eqn)
    route_info <- pm_model_route_mapping(model)
    route_usage <- route_info$routes
    route_names <- route_info$names
    excluded_names <- c(params, covariate_names, unname(state_names), unname(output_names))
    derived_names <- model_derived_names(model$arg_list, eqn_exprs, excluded_names)
    mappings <- list(states = state_names, outputs = output_names, routes = route_names)

    lines <- c(
      "name = user",
      "kind = ode",
      sprintf("params = %s", paste(params, collapse = ", ")),
      if (length(covariate_declarations) > 0) sprintf("covariates = %s", paste(covariate_declarations, collapse = ", ")),
      if (length(derived_names) > 0) sprintf("derived = %s", paste(derived_names, collapse = ", ")),
      sprintf("states = %s", paste(unname(state_names), collapse = ", ")),
      sprintf("outputs = %s", paste(unname(output_names), collapse = ", ")),
      build_route_declarations(route_usage, route_names),
      translate_statements(model$arg_list$sec, "derive", mappings),
      translate_statements(model$arg_list$ini, "ini", mappings),
      translate_statements(model$arg_list$lag, "lag", mappings),
      translate_statements(model$arg_list$fa, "fa", mappings),
      translate_statements(eqn_exprs, "ode", mappings),
      translate_statements(model$arg_list$out, "out", mappings)
    )
  } else if (kind == "analytical") {
    spec <- dsl_analytical_spec(model$model_list$name)
    state_names <- setNames(spec$states, as.character(seq_along(spec$states)))
    route_info <- pm_model_route_mapping(model)
    route_names <- route_info$names
    eqn_exprs <- statement_exprs(model$arg_list$eqn)
    eqn_exprs <- purrr::discard(eqn_exprs, \(expr) is_template_statement(expr, model$model_list$name))
    runtime_params <- tolower(spec$params %||% params)
    symbol_map <- spec$symbols %||% character(0)
    if (length(symbol_map) > 0) {
      symbol_map <- stats::setNames(tolower(unname(symbol_map)), tolower(names(symbol_map)))
    }
    excluded_names <- c(runtime_params, covariate_names, unname(state_names), unname(output_names))
    derived_names <- model_derived_names(model$arg_list, eqn_exprs, excluded_names)
    derived_names <- rename_declared_names(derived_names, symbol_map)
    mappings <- list(states = state_names, outputs = output_names, routes = route_names, symbols = symbol_map)

    lines <- c(
      sprintf("name = %s", tolower(model$model_list$name)),
      "kind = analytical",
      sprintf("params = %s", paste(runtime_params, collapse = ", ")),
      if (length(covariate_declarations) > 0) sprintf("covariates = %s", paste(covariate_declarations, collapse = ", ")),
      if (length(derived_names) > 0) sprintf("derived = %s", paste(derived_names, collapse = ", ")),
      sprintf("states = %s", paste(unname(state_names), collapse = ", ")),
      sprintf("outputs = %s", paste(unname(output_names), collapse = ", ")),
      build_route_declarations(spec$routes, route_names),
      translate_statements(model$arg_list$sec, "derive", mappings),
      translate_statements(eqn_exprs, "derive", mappings),
      sprintf("structure = %s", spec$structure),
      translate_statements(model$arg_list$ini, "ini", mappings),
      translate_statements(model$arg_list$lag, "lag", mappings),
      translate_statements(model$arg_list$fa, "fa", mappings),
      translate_statements(model$arg_list$out, "out", mappings)
    )
  } else {
    cli::cli_abort(c("x" = "Unsupported model type for DSL lowering: {.val {model$model_list$type}}."))
  }

  paste(lines[nzchar(lines)], collapse = "\n")
}
