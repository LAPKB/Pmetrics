# Export of a Pmetrics run to a BestDose model file (.bdm)
#
# `PM_result$export()` is the user facing entry point. Everything that can be
# read from the run (model, final support points, error model, covariate data,
# dosing data, fit statistics) is exported automatically. Only the drug and the
# model name are required; anything that cannot be retrieved is given a neutral
# default and can be overridden through the `...` of `$export()`.

# ==================================================================____
# Expression helpers
# ==================================================================____

# statements of a function body, as a list of expressions
bd_statements <- function(fn) {
  if (is.null(fn)) {
    return(list())
  }
  b <- body(fn)
  if (is.call(b) && identical(b[[1]], as.name("{"))) as.list(b)[-1] else list(b)
}

bd_is_assign <- function(e) {
  is.call(e) && identical(e[[1]], as.name("="))
}

# BestDose notation: lower case symbols, `<-` written as `=`, and the indexed
# state/input/output vectors in upper case (X[], B[], R[], Y[]); `dx[]`, `lag[]`
# and `fa[]` stay lower case.
bd_norm <- function(e) {
  if (is.symbol(e)) {
    s <- as.character(e)
    return(if (s %in% c("T", "F")) e else as.name(tolower(s)))
  }
  if (!is.call(e)) {
    return(e)
  }
  parts <- as.list(e)
  if (identical(parts[[1]], as.name("<-"))) {
    parts[[1]] <- as.name("=")
  }
  if (identical(parts[[1]], as.name("[")) && length(parts) >= 2 && is.symbol(parts[[2]])) {
    head <- tolower(as.character(parts[[2]]))
    parts[[2]] <- as.name(switch(head,
      x = "X",
      b = ,
      bolus = "B",
      r = ,
      rateiv = "R",
      y = "Y",
      head
    ))
    parts[-(1:2)] <- lapply(parts[-(1:2)], bd_norm)
  } else {
    parts <- lapply(parts, bd_norm)
  }
  as.call(parts)
}

# literal indices used with a vector, e.g. bd_index_refs(e, "X") -> c(1L, 2L)
bd_index_refs <- function(e, head) {
  if (!is.call(e)) {
    return(integer())
  }
  parts <- as.list(e)
  hit <- integer()
  if (identical(parts[[1]], as.name("[")) && length(parts) >= 3 &&
    identical(parts[[2]], as.name(head)) && is.numeric(parts[[3]])) {
    hit <- as.integer(parts[[3]])
  }
  as.integer(c(hit, unlist(lapply(parts, bd_index_refs, head = head))))
}

# one model statement: normalized expression, text, and what it assigns to
bd_item <- function(e) {
  n <- bd_norm(e)
  name <- NA_character_
  idx <- NA_integer_
  rhs <- n
  if (bd_is_assign(n)) {
    rhs <- n[[3]]
    lhs <- n[[2]]
    if (is.symbol(lhs)) {
      name <- as.character(lhs)
    } else if (is.call(lhs) && identical(lhs[[1]], as.name("[")) &&
      is.symbol(lhs[[2]]) && is.numeric(lhs[[3]])) {
      name <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
    }
  }
  list(
    expr = n, rhs = rhs, name = name, idx = idx,
    text = gsub("\\s*/\\s*", " / ", paste(trimws(deparse(n, width.cutoff = 500L)), collapse = " "))
  )
}

# named list of statement text, the way BestDose names its model lines
bd_named <- function(items) {
  if (!length(items)) {
    return(NULL)
  }
  keys <- vapply(seq_along(items), function(k) {
    it <- items[[k]]
    if (!is.na(it$idx)) {
      paste0(it$name, it$idx)
    } else if (!is.na(it$name)) {
      it$name
    } else {
      paste0("expr", k)
    }
  }, character(1))
  stats::setNames(lapply(items, `[[`, "text"), make.unique(keys))
}

# variables and states an expression depends on, following the assignments in `defs`
bd_deps <- function(expr, defs) {
  seen <- character()
  states <- integer()
  queue <- list(expr)
  while (length(queue)) {
    e <- queue[[1]]
    queue <- queue[-1]
    states <- c(states, bd_index_refs(e, "X"))
    for (v in all.vars(e)) {
      if (!v %in% seen) {
        seen <- c(seen, v)
        if (!is.null(defs[[v]])) {
          queue <- c(queue, list(defs[[v]]))
        }
      }
    }
  }
  list(vars = seen, states = unique(states))
}


# ==================================================================____
# Model blocks
# ==================================================================____

#' Split a PM model into BestDose blocks
#'
#' Library (analytical) templates are replaced by their ordinary differential
#' equations, because BestDose models are ODE based. Assignments written in the
#' `eqn` block that are not derivatives are moved to the secondary block.
#' @param a The `arg_list` of a [PM_model]
#' @return A list of statement lists (`sec`, `dx`, `ini`, `fa`, `lag`, `out`)
#' and `defs`, the right hand side of every scalar assignment.
#' @noRd
bd_parse_model <- function(a) {
  eqn <- bd_statements(a$eqn)
  lag <- bd_statements(a$lag)
  fa <- bd_statements(a$fa)

  tmpl <- get_found_model(a$eqn)
  if (is.list(tmpl)) {
    txt <- vapply(eqn, function(s) paste(deparse(s, width.cutoff = 500L), collapse = "\n"), character(1))
    eqn <- c(bd_statements(tmpl$arg_list$eqn), eqn[!txt %in% c(tmpl$name, tmpl$alt_names)])
    if (is.null(a$lag)) lag <- bd_statements(tmpl$arg_list$lag)
    if (is.null(a$fa)) fa <- bd_statements(tmpl$arg_list$fa)
  }

  eqn <- lapply(eqn, bd_item)
  is_dx <- vapply(eqn, function(i) isTRUE(i$name == "dx") && !is.na(i$idx), logical(1))
  by_idx <- function(items) items[order(vapply(items, `[[`, integer(1), "idx"))]

  p <- list(
    sec = c(lapply(bd_statements(a$sec), bd_item), eqn[!is_dx]),
    dx = by_idx(eqn[is_dx]),
    ini = lapply(bd_statements(a$ini), bd_item),
    fa = lapply(fa, bd_item),
    lag = lapply(lag, bd_item),
    out = lapply(bd_statements(a$out), bd_item)
  )
  p$out_y <- by_idx(p$out[vapply(p$out, function(i) isTRUE(i$name == "y") && !is.na(i$idx), logical(1))])

  if (length(p$dx) == 0) {
    cli::cli_abort(c("x" = "No differential equations ({.code dx[i]}) were found in the model."))
  }
  if (length(p$out_y) == 0) {
    cli::cli_abort(c("x" = "No output equations ({.code y[i]}) were found in the model."))
  }

  all_items <- c(p$sec, p$dx, p$ini, p$fa, p$lag, p$out)
  scalar <- Filter(function(i) !is.na(i$name) && is.na(i$idx), all_items)
  p$defs <- stats::setNames(lapply(scalar, `[[`, "rhs"), vapply(scalar, `[[`, character(1), "name"))
  p
}

#' Compartment structure inferred from the equations
#'
#' The PK part of the model is every compartment the first output depends on
#' (directly or through other compartments). Remaining compartments, and
#' outputs that depend on them, are pharmacodynamic.
#' @noRd
bd_structure <- function(p, primary) {
  states_of <- function(item) bd_deps(item$rhs, p$defs)$states
  dx <- stats::setNames(p$dx, vapply(p$dx, function(i) as.character(i$idx), character(1)))

  out_states <- lapply(p$out_y, states_of)
  all_comp <- as.integer(names(dx))
  seed <- out_states[[1]]
  if (length(seed) == 0) {
    seed <- all_comp[1]
  }
  central <- seed[1]

  pk <- seed
  repeat {
    new <- setdiff(unique(unlist(lapply(as.character(pk), function(cc) {
      if (is.null(dx[[cc]])) integer() else states_of(dx[[cc]])
    }))), pk)
    if (length(new) == 0) break
    pk <- c(pk, new)
  }
  pk <- sort(pk)

  # peripheral compartments exchange drug reversibly with the central one
  peripheral <- Filter(function(cc) {
    !is.null(dx[[as.character(cc)]]) && !is.null(dx[[as.character(central)]]) &&
      central %in% states_of(dx[[as.character(cc)]]) &&
      cc %in% states_of(dx[[as.character(central)]])
  }, setdiff(pk, central))
  peripheral <- as.integer(unlist(peripheral))

  is_pd <- vapply(seq_along(out_states), function(k) {
    k > 1 && length(out_states[[k]]) > 0 && any(!out_states[[k]] %in% pk)
  }, logical(1))

  out_names <- vapply(seq_along(out_states), function(k) {
    s <- out_states[[k]]
    if (k == 1) {
      "central"
    } else if (length(s) == 1 && s %in% peripheral) {
      "peripheral"
    } else {
      paste0("output", p$out_y[[k]]$idx)
    }
  }, character(1))

  # parameters used only by the PD part of the model
  vars_of <- function(items) {
    unique(unlist(lapply(items, function(i) bd_deps(i$rhs, p$defs)$vars)))
  }
  in_pk <- function(i) is.na(i$idx) || i$idx %in% pk
  pk_vars <- vars_of(c(
    Filter(in_pk, p$dx), Filter(in_pk, p$ini), p$lag, p$fa,
    p$out_y[!is_pd]
  ))
  pd_vars <- vars_of(c(
    Filter(Negate(in_pk), p$dx), Filter(Negate(in_pk), p$ini),
    p$out_y[is_pd]
  ))
  pd_parameters <- as.character(setdiff(intersect(primary, pd_vars), pk_vars))

  list(
    pk = pk, central = central, peripheral = peripheral,
    out_idx = vapply(p$out_y, `[[`, integer(1), "idx"),
    out_names = make.unique(out_names), is_pd = is_pd,
    pd_parameters = pd_parameters
  )
}

#' Routes of administration from the dosing data and the model inputs
#' @noRd
bd_routes <- function(p, data, central) {
  feeds <- do.call(rbind, lapply(p$dx, function(it) {
    b <- bd_index_refs(it$rhs, "B")
    r <- bd_index_refs(it$rhs, "R")
    data.frame(
      comp = rep(it$idx, length(b) + length(r)),
      kind = c(rep("B", length(b)), rep("R", length(r))),
      input = c(b, r)
    )
  }))

  doses <- NULL
  if (!is.null(data) && all(c("evid", "dur") %in% names(data))) {
    doses <- data[!is.na(data$evid) & data$evid == 1, , drop = FALSE]
    if (nrow(doses) == 0) doses <- NULL
  }

  if (is.null(doses)) {
    inputs <- sort(unique(feeds$input))
    has_inf <- stats::setNames(vapply(inputs, function(j) any(feeds$kind == "R" & feeds$input == j), logical(1)), inputs)
    has_bol <- stats::setNames(vapply(inputs, function(j) any(feeds$kind == "B" & feeds$input == j), logical(1)), inputs)
  } else {
    inp <- if ("input" %in% names(doses)) doses$input else rep(1, nrow(doses))
    inp[is.na(inp)] <- 1
    inputs <- sort(unique(inp))
    has_inf <- stats::setNames(vapply(inputs, function(j) any(doses$dur[inp == j] > 0, na.rm = TRUE), logical(1)), inputs)
    has_bol <- stats::setNames(vapply(inputs, function(j) any(is.na(doses$dur[inp == j]) | doses$dur[inp == j] == 0), logical(1)), inputs)
  }

  routes <- data.frame(route = character(), compartment = integer())
  for (j in inputs) {
    jj <- as.character(j)
    if (isTRUE(has_inf[[jj]])) {
      routes <- rbind(routes, data.frame(route = "IV", compartment = as.integer(j)))
    }
    if (isTRUE(has_bol[[jj]])) {
      targets <- feeds$comp[feeds$kind == "B" & feeds$input == j]
      route <- if (length(targets) == 0 || all(targets == central)) "IV" else "PO"
      routes <- rbind(routes, data.frame(route = route, compartment = as.integer(j)))
    }
  }
  routes <- unique(routes)
  if (nrow(routes) == 0) {
    routes <- data.frame(route = "IV", compartment = 1L)
  }
  rownames(routes) <- NULL
  routes
}


# ==================================================================____
# Primary parameters, covariates, error model
# ==================================================================____

bd_primary <- function(pri) {
  lapply(stats::setNames(pri, tolower(names(pri))), function(x) {
    list(type = "ab", min = x$min, max = x$max)
  })
}

# label, unit and description for covariate names that are common in PK
bd_cov_info <- function(name) {
  known <- list(
    wt = c("Weight", "kg", "Patient weight in kilograms"),
    weight = c("Weight", "kg", "Patient weight in kilograms"),
    bw = c("Body weight", "kg", "Patient body weight in kilograms"),
    lbw = c("Lean body weight", "kg", "Patient lean body weight in kilograms"),
    ibw = c("Ideal body weight", "kg", "Patient ideal body weight in kilograms"),
    age = c("Age", "years", "Patient age in years"),
    ht = c("Height", "cm", "Patient height in centimeters"),
    height = c("Height", "cm", "Patient height in centimeters"),
    bsa = c("Body surface area", "m2", "Body surface area in square meters"),
    bmi = c("Body mass index", "kg/m2", "Body mass index"),
    crcl = c("Creatinine clearance", "mL/min", "Creatinine clearance in mL/min"),
    egfr = c("eGFR", "mL/min/1.73m2", "Estimated glomerular filtration rate"),
    scr = c("Serum creatinine", "mg/dL", "Serum creatinine in mg/dL"),
    cr = c("Serum creatinine", "mg/dL", "Serum creatinine in mg/dL"),
    alb = c("Albumin", "g/dL", "Serum albumin in g/dL"),
    albumin = c("Albumin", "g/dL", "Serum albumin in g/dL")
  )
  if (!is.null(known[[name]])) {
    return(known[[name]])
  }
  label <- paste0(toupper(substring(name, 1, 1)), substring(name, 2))
  c(label, "", paste0("Covariate ", name))
}

bd_cov_range <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(list(min = 0, max = 1, default = 0))
  }
  rng <- range(x)
  step <- if (diff(rng) >= 10) 1 else 0.1
  list(
    min = round(floor(rng[1] / step) * step, 6),
    max = round(ceiling(rng[2] / step) * step, 6),
    default = round(stats::median(x), 2)
  )
}

# returns the covariate block of the model and of the description
bd_covariates <- function(cov, data) {
  if (length(cov) == 0) {
    return(list(model = NULL, description = NULL))
  }
  nm <- tolower(names(cov))
  model <- stats::setNames(lapply(cov, function(v) {
    list(interp = if (isTRUE(as.numeric(v) == 1)) "linear" else "none")
  }), nm)

  info <- lapply(nm, bd_cov_info)
  data_names <- if (is.null(data)) character() else tolower(names(data))
  values <- stats::setNames(lapply(nm, function(n) {
    col <- which(data_names == n)
    list(bd_cov_range(if (length(col)) data[[col[1]]] else NULL))
  }), nm)

  list(
    model = model,
    description = list(
      number = length(nm),
      names = I(nm),
      label = I(vapply(info, `[`, character(1), 1)),
      units = I(vapply(info, `[`, character(1), 2)),
      types = I(rep("numeric", length(nm))),
      value = values,
      description = stats::setNames(lapply(info, `[`, 3), nm)
    )
  )
}

# Error models in output order. The first one is described at the top level and
# the others carry their own type and value, as in the BestDose files. The error
# value (gamma/lambda) is the one estimated by the run when available.
bd_error <- function(err, out_idx, cycle) {
  if (length(err) == 0) {
    cli::cli_abort(c("x" = "The model has no error model."))
  }
  labels <- pm_err_labels(err)
  eq <- suppressWarnings(as.integer(sub("^outeq_", "", labels)))
  ord <- if (anyNA(eq)) seq_along(err) else order(eq)
  err <- err[ord]
  eq <- eq[ord]

  one <- function(e, outeq) {
    value <- e$initial
    if (!isTRUE(e$fixed) && !is.null(cycle) && all(c("outeq", "value", "cycle") %in% names(cycle))) {
      est <- cycle[cycle$outeq == outeq, , drop = FALSE]
      if (nrow(est) > 0) {
        value <- est$value[which.max(est$cycle)]
      }
    }
    cf <- c(e$coeff, rep(0, 4))[1:4] + 0 # + 0 turns -0 into 0
    list(
      type = e$type, initial_value = value,
      coefficient = list(c0 = cf[1], c1 = cf[2], c2 = cf[3], c3 = cf[4])
    )
  }
  items <- lapply(seq_along(err), function(k) one(err[[k]], if (anyNA(eq)) k else eq[k]))

  coefficient <- lapply(seq_along(items), function(k) {
    if (k == 1) {
      items[[k]]$coefficient
    } else {
      c(items[[k]]$coefficient, list(type = items[[k]]$type, initial_value = items[[k]]$initial_value))
    }
  })
  list(
    type = items[[1]]$type,
    initial_value = items[[1]]$initial_value,
    coefficient = coefficient
  )
}


# ==================================================================____
# Assembly
# ==================================================================____

#' @title Create a BestDose model list from a PM_result
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Builds the content of a BestDose model file (.bdm) from the model, final
#' population support points, error model, data and fit statistics of a run.
#' @param PM_result A [PM_result] object
#' @param drug_name Name of the drug
#' @param model_name Name of the model
#' @param ... Named elements that replace or complete the `description` block,
#' e.g. `reference = "Author, Journal, 2024"`.
#' @return A list with `description`, `model` and `support_point`, ready to be
#' written as JSON.
#' @noRd
createBDmodel <- function(PM_result, drug_name = "Drug", model_name = drug_name, ...) {
  mod <- PM_result$model
  a <- mod$arg_list
  if (is.null(a) || !is.function(a$eqn) || !is.function(a$out) || is.null(a$pri)) {
    cli::cli_abort(c(
      "x" = "The model of this run cannot be exported.",
      "i" = "Only models defined with {.code pri}, {.code eqn}, {.code out} and {.code err} blocks are supported."
    ))
  }
  if (is.null(PM_result$final$popPoints)) {
    cli::cli_abort(c("x" = "The run has no final population support points."))
  }

  data <- PM_result$data$data
  cycle <- PM_result$cycle$data
  primary <- tolower(names(a$pri))

  p <- bd_parse_model(a)
  s <- bd_structure(p, primary)
  cov <- bd_covariates(a$cov, data)

  # symbols that are neither parameters, covariates nor defined in the model
  known <- c(primary, tolower(names(a$cov)), names(p$defs), "X", "B", "R", "Y", "dx", "lag", "fa")
  used <- unique(unlist(lapply(c(p$sec, p$dx, p$ini, p$fa, p$lag, p$out), function(i) all.vars(i$rhs))))
  undefined <- setdiff(used, known)
  if (length(undefined) > 0) {
    cli::cli_warn(c(
      "!" = "The model uses {.val {undefined}}, which {?is/are} not a primary parameter, covariate or secondary variable.",
      "i" = "The exported BestDose model will not run until {?it is/they are} defined."
    ))
  }

  # support points, in parameter order
  sp <- as.data.frame(PM_result$final$popPoints)
  names(sp) <- tolower(names(sp))
  missing_cols <- setdiff(c(primary, "prob"), names(sp))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c("x" = "Support points are missing {.val {missing_cols}}."))
  }
  sp <- sp[, c(primary, "prob"), drop = FALSE]

  outputs <- lapply(seq_along(s$out_idx), function(k) {
    o <- list(name = s$out_names[k], equation = s$out_idx[k])
    if (s$is_pd[k]) {
      o$pd <- list(label = paste("Output", s$out_idx[k]), unit = "")
    }
    o
  })

  fit_text <- NULL
  neg2ll <- tryCatch(utils::tail(cycle$objective$neg2ll, 1), error = function(e) NULL)
  ncyc <- tryCatch(max(cycle$objective$cycle), error = function(e) NULL)
  if (length(neg2ll) == 1 && length(ncyc) == 1 && is.finite(neg2ll)) {
    fit_text <- sprintf(" Fit: %d cycles, -2LL = %.1f.", as.integer(ncyc), neg2ll)
  }
  nsub <- PM_result$final$nsub
  description_text <- sprintf(
    "%s model exported from a Pmetrics run with %s subject(s) and %d support points.%s Exported with Pmetrics %s on %s.",
    drug_name,
    if (is.null(nsub)) "an unknown number of" else nsub,
    nrow(sp), if (is.null(fit_text)) "" else fit_text,
    as.character(utils::packageVersion("Pmetrics")), format(Sys.Date())
  )

  description <- list(
    drug = drug_name,
    route = bd_routes(p, data, s$central),
    name = model_name,
    compartment = list(
      number = length(s$pk),
      central = s$central - 1L,
      peripheral = if (length(s$peripheral)) I(s$peripheral - 1L) else NULL,
      outputs = outputs
    ),
    version = 1,
    description = description_text,
    reference = "Exported from a Pmetrics run",
    reference_url = "",
    covariates = cov$description,
    native = FALSE,
    default_model = FALSE,
    pd_parameters = I(s$pd_parameters)
  )
  overrides <- list(...)
  if (length(overrides) > 0) {
    description <- utils::modifyList(description, overrides)
  }

  model <- list(
    primary = bd_primary(a$pri),
    covariates = cov$model,
    secondary = bd_named(p$sec),
    initial_conditions = bd_named(p$ini),
    fa = bd_named(p$fa),
    lag = bd_named(p$lag),
    equation = bd_named(p$dx),
    out = bd_named(p$out_y),
    error = bd_error(a$err, s$out_idx, cycle$gamlam)
  )

  list(description = description, model = model, support_point = sp)
}

bd_export <- function(run, path = ".", drug, model, ...) {
  if (missing(drug) || !is.character(drug) || length(drug) != 1 || !nzchar(drug)) {
    cli::cli_abort(c("x" = "{.arg drug} must be a single non-empty character string."))
  }
  if (missing(model) || !is.character(model) || length(model) != 1 || !nzchar(model)) {
    cli::cli_abort(c("x" = "{.arg model} must be a single non-empty character string."))
  }
  model <- sub("\\.bdm$", "", model)

  bd <- createBDmodel(run, drug_name = drug, model_name = model, ...)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  file <- file.path(path, paste0(model, ".bdm"))
  jsonlite::write_json(bd, file, auto_unbox = TRUE, pretty = TRUE, digits = 10, null = "null", na = "null")

  needs_review <- character()
  if (!is.null(bd$description$covariates) && any(bd$description$covariates$units == "")) {
    needs_review <- c(needs_review, "covariate units, labels and descriptions")
  }
  if (any(vapply(bd$description$compartment$outputs, function(o) !is.null(o$pd), logical(1)))) {
    needs_review <- c(needs_review, "PD output labels and units")
  }
  cli::cli_inform(c(
    "v" = "BestDose model written to {.file {file}}",
    "i" = "{length(bd$description$compartment$outputs)} output(s), {nrow(bd$support_point)} support points, {length(bd$model$primary)} parameter(s).",
    if (length(needs_review) > 0) c("!" = "Not available in a Pmetrics run, please review: {needs_review}. They can be supplied through {.code ...}.")
  ))
  invisible(file)
}
