# Export of a Pmetrics run to a BestDose model file (.bdm)
#
# `PM_result$export()` is the user facing entry point. Everything that can be
# read from the run (model, final support points, error model, covariate data,
# dosing data, fit statistics) is exported automatically. Only the drug and the
# model name are required; anything that cannot be retrieved is given a neutral
# default and can be overridden through `outputs` and `...` of `$export()`.
#
# The exported model is built to pass the BestDose model checklist
# (dev/bestdose_example/model_checklist.md). What cannot be repaired
# automatically is reported by `bd_check()` after the file is written.

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
# and `fa[]` stay lower case. `ren` renames symbols (covariates), old -> new.
bd_norm <- function(e, ren = NULL) {
  if (is.symbol(e)) {
    s <- as.character(e)
    if (s %in% c("T", "F")) {
      return(e)
    }
    s <- tolower(s)
    if (!is.null(ren) && s %in% names(ren)) s <- ren[[s]]
    return(as.name(s))
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
    parts[-(1:2)] <- lapply(parts[-(1:2)], bd_norm, ren = ren)
  } else {
    parts <- lapply(parts, bd_norm, ren = ren)
  }
  as.call(parts)
}

bd_text <- function(e) {
  gsub("\\s*/\\s*", " / ", paste(trimws(deparse(e, width.cutoff = 500L)), collapse = " "))
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
bd_item <- function(e, ren = NULL) {
  n <- bd_norm(e, ren)
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
  list(expr = n, rhs = rhs, name = name, idx = idx, text = bd_text(n))
}

# the same statement with a new right hand side
bd_set_rhs <- function(it, rhs) {
  it$expr[[3]] <- rhs
  it$rhs <- rhs
  it$text <- bd_text(it$expr)
  it
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

# removes the standalone additive terms B[j] / R[j] listed in `drop` (e.g. "R1")
# from a sum, e.g. `R[1] + ka * X[1] - ke * X[2]` -> `ka * X[1] - ke * X[2]`
bd_drop_terms <- function(e, drop) {
  is_term <- function(x) {
    is.call(x) && identical(x[[1]], as.name("[")) && length(x) >= 3 && is.symbol(x[[2]]) &&
      is.numeric(x[[3]]) && paste0(as.character(x[[2]]), x[[3]]) %in% drop
  }
  if (is_term(e)) {
    return(NULL)
  }
  if (is.call(e) && (identical(e[[1]], as.name("+")) || identical(e[[1]], as.name("-")))) {
    if (length(e) == 3) {
      l <- bd_drop_terms(e[[2]], drop)
      r <- bd_drop_terms(e[[3]], drop)
      if (is.null(l) && is.null(r)) {
        return(NULL)
      }
      if (is.null(r)) {
        return(l)
      }
      if (is.null(l)) {
        return(if (identical(e[[1]], as.name("-"))) call("-", r) else r)
      }
      e[[2]] <- l
      e[[3]] <- r
      return(e)
    }
    if (length(e) == 2) {
      a <- bd_drop_terms(e[[2]], drop)
      if (is.null(a)) {
        return(NULL)
      }
      e[[2]] <- a
      return(e)
    }
  }
  e
}


# ==================================================================____
# Reserved covariate names (English, French, Spanish, Norwegian)
# ==================================================================____

# BestDose reserved covariate names and the other names they go by. Keys are
# ASCII, lower case, with `_` as separator (see `bd_ascii_key()`).
bd_cov_keywords <- list(
  weight = c(
    "wt", "tbw", "bw", "body_weight", "bodyweight", # en
    "poids", "pds", "poids_corporel", "poids_total", # fr
    "peso", "peso_corporal", "peso_total", # es
    "vekt", "kroppsvekt" # no
  ),
  crcl = c(
    "gfr", "egfr", "ccr", "clcr", "cl_cr", "clcreat", "creatinine_clearance", "cockcroft", "cockcroft_gault", # en
    "dfg", "edfg", "clairance_creatinine", "clairance_de_la_creatinine", # fr
    "tfg", "etfg", "aclaramiento_creatinina", "filtrado_glomerular", # es
    "kreatininclearance", "kreatinin_clearance", "krea_clearance" # no
  ),
  creat = c(
    "creatinine", "scr", "screat", "serum_creatinine", "cr", # en
    "creatininemie", "creatinine_serique", # fr
    "creatinina", "crea", "creatinina_serica", # es
    "kreatinin", "kreat", "krea", "serumkreatinin", "s_kreatinin" # no
  ),
  height = c(
    "ht", "hgt", "size", # en
    "taille", "hauteur", # fr
    "talla", "estatura", "altura", # es
    "hoyde", "kroppshoyde", "lengde" # no
  ),
  bmi = c(
    "imc", "body_mass_index", # en, fr
    "indice_masse_corporelle", "indice_de_masse_corporelle", # fr
    "indice_masa_corporal", "indice_de_masa_corporal", # es
    "kmi", "kroppsmasseindeks" # no
  ),
  bsa = c(
    "sc", "body_surface_area", # en
    "surface_corporelle", # fr
    "superficie_corporal", "asc", "area_superficie_corporal", # es
    "kroppsoverflate", "overflate", "kof" # no
  ),
  sex = c(
    "sexe", "gender", # fr, en
    "sexo", "genero", # es
    "kjonn", "kjoenn" # no
  ),
  age = c(
    "edad", # es
    "alder" # no
  )
)

# lower case ASCII key of a name: accents removed, anything else than letters and
# digits becomes `_`
bd_ascii_key <- function(x) {
  x <- tolower(x)
  x <- chartr(
    "æøåéèêëàâäîïôöùûüçñáíóú",
    "aoaeeeeaaaiioouuucnaiou", x
  )
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

# reserved name for a covariate name, or its cleaned name if it has none
bd_cov_canonical <- function(name) {
  key <- bd_ascii_key(name)
  for (canon in names(bd_cov_keywords)) {
    if (key == canon || key %in% bd_cov_keywords[[canon]]) {
      return(canon)
    }
  }
  if (nzchar(key)) key else "cov"
}

# old -> new names for the covariates of the model (all lower case)
bd_cov_rename <- function(cov_names, taken) {
  old <- tolower(cov_names)
  new <- vapply(old, bd_cov_canonical, character(1))
  for (k in seq_along(new)) {
    if (new[k] %in% taken || new[k] %in% new[-k]) { # would clash: keep a cleaned version of the name
      new[k] <- bd_ascii_key(old[k])
    }
  }
  stats::setNames(make.unique(new, sep = "_"), old)
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
#' @param ren Named vector renaming covariates, old -> new
#' @return A list of statement lists (`sec`, `dx`, `ini`, `fa`, `lag`, `out`)
#' and `defs`, the right hand side of every scalar assignment.
#' @noRd
bd_parse_model <- function(a, ren = NULL) {
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

  eqn <- lapply(eqn, bd_item, ren = ren)
  is_dx <- vapply(eqn, function(i) isTRUE(i$name == "dx") && !is.na(i$idx), logical(1))
  by_idx <- function(items) items[order(vapply(items, `[[`, integer(1), "idx"))]

  p <- list(
    sec = c(lapply(bd_statements(a$sec), bd_item, ren = ren), eqn[!is_dx]),
    dx = by_idx(eqn[is_dx]),
    ini = lapply(bd_statements(a$ini), bd_item, ren = ren),
    fa = lapply(fa, bd_item, ren = ren),
    lag = lapply(lag, bd_item, ren = ren),
    out = lapply(bd_statements(a$out), bd_item, ren = ren)
  )
  p$out_y <- by_idx(p$out[vapply(p$out, function(i) isTRUE(i$name == "y") && !is.na(i$idx), logical(1))])

  if (length(p$dx) == 0) {
    cli::cli_abort(c("x" = "No differential equations ({.code dx[i]}) were found in the model."))
  }
  if (length(p$out_y) == 0) {
    cli::cli_abort(c("x" = "No output equations ({.code y[i]}) were found in the model."))
  }
  p$defs <- bd_defs(p)
  p
}

# right hand side of every scalar assignment, by variable name
bd_defs <- function(p) {
  all_items <- c(p$sec, p$dx, p$ini, p$fa, p$lag, p$out)
  scalar <- Filter(function(i) !is.na(i$name) && is.na(i$idx), all_items)
  stats::setNames(lapply(scalar, `[[`, "rhs"), vapply(scalar, `[[`, character(1), "name"))
}

# BestDose concentration outputs: X[N] divided by a parameter
bd_is_conc <- function(text) {
  grepl("X\\[[1-6]\\]\\s*/\\s*(\\(|[A-Za-z])", text)
}

#' Compartment structure inferred from the equations
#'
#' The PK part of the model is every compartment the first output depends on
#' (directly or through other compartments). Remaining compartments, and
#' outputs that depend on them or that are not a concentration, are
#' pharmacodynamic.
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
  depot <- setdiff(pk, c(central, peripheral)) # absorption compartments

  is_pd <- vapply(seq_along(out_states), function(k) {
    k > 1 && (any(!out_states[[k]] %in% pk) || !bd_is_conc(p$out_y[[k]]$text))
  }, logical(1))

  out_names <- vapply(seq_along(out_states), function(k) {
    s <- out_states[[k]]
    if (k == 1) {
      "central"
    } else if (!is_pd[k] && length(s) == 1 && s %in% peripheral) {
      "peripheral"
    } else if (is_pd[k]) {
      paste0("pd", p$out_y[[k]]$idx)
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
    pk = pk, central = central, peripheral = peripheral, depot = depot,
    out_idx = vapply(p$out_y, `[[`, integer(1), "idx"),
    out_names = make.unique(out_names), is_pd = is_pd,
    pd_parameters = pd_parameters
  )
}

# Compartment block. BestDose numbers the compartments from 1: the central one,
# then peripheral ones, then absorption compartments; `number` is the count of
# PK compartments and every one of them is named.
bd_compartment <- function(s, outputs) {
  idx <- 0L
  nxt <- function() {
    idx <<- idx + 1L
    idx
  }
  comp <- list(number = length(s$pk), central = nxt())
  named <- function(prefix, n) {
    if (n == 0) {
      return(NULL)
    }
    v <- lapply(seq_len(n), function(i) nxt())
    stats::setNames(v, if (n == 1) prefix else paste0(prefix, seq_len(n)))
  }
  c(comp, named("peripheral", length(s$peripheral)), named("depot", length(s$depot)), list(outputs = outputs))
}

#' Routes of administration from the dosing data and the model inputs
#'
#' @return A list with `routes` (data frame with `route` and `compartment`, the
#' input number), and the inputs for which the infusion (`R`) and bolus (`B`)
#' terms are used by the data. Both are `NULL` when the data hold no dose.
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
  feed_b <- function(j) feeds$comp[feeds$kind == "B" & feeds$input == j]

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
  use_r <- use_b <- integer()
  for (j in inputs) {
    jj <- as.character(j)
    targets <- feed_b(j)
    if (isTRUE(has_inf[[jj]])) {
      routes <- rbind(routes, data.frame(route = "IV", compartment = as.integer(j)))
      use_r <- c(use_r, j)
    }
    if (isTRUE(has_bol[[jj]])) {
      route <- if (length(targets) == 0 || all(targets == central)) "IV" else "PO"
      routes <- rbind(routes, data.frame(route = route, compartment = as.integer(j)))
      if (length(targets) == 0) use_r <- c(use_r, j) else use_b <- c(use_b, j)
    }
  }
  routes <- unique(routes)
  if (nrow(routes) == 0) {
    routes <- data.frame(route = "IV", compartment = 1L)
  }
  rownames(routes) <- NULL
  list(
    routes = routes,
    inputs = unique(inputs),
    use_r = if (is.null(doses)) NULL else unique(use_r),
    use_b = if (is.null(doses)) NULL else unique(use_b)
  )
}

# Drops the input terms (library models have both B[j] and R[j]) that the
# dosing data never use, so every input in the equations has a matching route.
bd_prune_inputs <- function(p, r) {
  if (is.null(r$use_r) && is.null(r$use_b)) {
    return(p)
  }
  unused <- function(head, used) {
    j <- setdiff(r$inputs, used)
    if (length(j)) paste0(head, j)
  }
  drop <- c(unused("R", r$use_r), unused("B", r$use_b))
  if (length(drop) == 0) {
    return(p)
  }
  p$dx <- lapply(p$dx, function(it) {
    new <- bd_drop_terms(it$rhs, drop)
    bd_set_rhs(it, if (is.null(new)) 0 else new)
  })
  p$defs <- bd_defs(p)
  p
}


# ==================================================================____
# Primary parameters, covariates, error model
# ==================================================================____

bd_primary <- function(pri) {
  lapply(stats::setNames(pri, tolower(names(pri))), function(x) {
    list(type = "ab", min = x$min, max = x$max)
  })
}

# label, unit and description for the reserved and some common covariate names
bd_cov_info <- function(name) {
  known <- list(
    weight = c("Weight", "kg", "Patient weight in kilograms"),
    lbw = c("Lean body weight", "kg", "Patient lean body weight in kilograms"),
    ibw = c("Ideal body weight", "kg", "Patient ideal body weight in kilograms"),
    age = c("Age", "years", "Patient age in years"),
    height = c("Height", "cm", "Patient height in centimeters"),
    bsa = c("Body surface area", "m2", "Body surface area in square meters"),
    bmi = c("Body mass index", "kg/m2", "Body mass index"),
    crcl = c("Creatinine clearance", "mL/min", "Creatinine clearance in mL/min"),
    creat = c("Serum creatinine", "mg/dL", "Serum creatinine in mg/dL"),
    albumin = c("Albumin", "g/dL", "Serum albumin in g/dL"),
    sex = c("Sex", "", "Patient sex")
  )
  if (!is.null(known[[name]])) {
    return(known[[name]])
  }
  label <- paste0(toupper(substring(name, 1, 1)), substring(name, 2))
  c(label, "", paste0("Covariate ", name))
}

# observed range of a numeric covariate; always min < max with the default inside
bd_cov_range <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(list(min = 0, max = 1, default = 0))
  }
  if (all(x %in% c(0, 1))) {
    return(list(min = 0, max = 1, default = round(stats::median(x))))
  }
  rng <- range(x)
  step <- if (diff(rng) >= 10) 1 else 0.1
  lo <- round(floor(rng[1] / step) * step, 6)
  hi <- round(ceiling(rng[2] / step) * step, 6)
  if (lo >= hi) { # constant covariate
    w <- if (lo == 0) 1 else abs(lo) / 2
    lo <- lo - w
    hi <- hi + w
  }
  list(min = lo, max = hi, default = round(stats::median(x), 2))
}

# `sex` is categorical with the lower case levels BestDose requires. Codes 1/2
# mean male/female, otherwise 1/0 do.
bd_sex_values <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  female <- if (any(x == 2)) 2 else 0
  mode <- if (length(x) == 0) 1 else as.numeric(names(which.max(table(x))))
  data.frame(
    cat_value = c("male", "female"),
    num_value = c(1, female),
    default = c(mode != female, mode == female)
  )
}

# Covariate blocks of the model and of the description. `cov` is the model's
# covariate list, `ren` maps its names to the BestDose names, `keep` the names
# (new) that the equations actually use.
bd_covariates <- function(cov, data, ren, keep) {
  old <- tolower(names(cov))
  sel <- unname(ren[old]) %in% keep
  if (!any(sel)) {
    return(list(model = NULL, description = NULL))
  }
  cov <- cov[sel]
  old <- old[sel]
  nm <- unname(ren[old])

  is_cat <- nm == "sex"
  model <- stats::setNames(lapply(seq_along(cov), function(k) {
    list(interp = if (!is_cat[k] && isTRUE(as.numeric(cov[[k]]) == 1)) "linear" else "none")
  }), nm)

  info <- lapply(nm, bd_cov_info)
  data_names <- if (is.null(data)) character() else tolower(names(data))
  column <- function(k) {
    col <- which(data_names == old[k])
    if (length(col)) data[[col[1]]] else NULL
  }
  values <- stats::setNames(lapply(seq_along(nm), function(k) {
    if (is_cat[k]) bd_sex_values(column(k)) else list(bd_cov_range(column(k)))
  }), nm)

  list(
    model = model,
    description = list(
      number = length(nm),
      names = I(nm),
      label = I(vapply(info, `[`, character(1), 1)),
      units = I(vapply(info, `[`, character(1), 2)),
      types = I(ifelse(is_cat, "categorical", "numeric")),
      value = values,
      description = stats::setNames(lapply(info, `[`, 3), nm)
    )
  )
}

# Error models in output order. The first one is described at the top level and
# the others carry their own type and value, as in the BestDose files. The error
# value (gamma/lambda) is the one estimated by the run when available.
bd_error <- function(err, cycle) {
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
      type = if (e$type == "additive") "additional" else e$type, # BestDose spelling
      initial_value = value,
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
#' @param outputs Optional named list, named by output equation number, to
#' give outputs a `name` and, for PD outputs, a `label` and `unit`, e.g.
#' `list("2" = list(name = "platelets", label = "Platelets", unit = "10^9/L"))`.
#' @param ... Named elements that replace or complete the `description` block,
#' e.g. `reference = "Author, Journal, 2024"`.
#' @return A list with `description`, `model` and `support_point`, ready to be
#' written as JSON.
#' @noRd
createBDmodel <- function(PM_result, drug_name = "Drug", model_name = drug_name, outputs = NULL, ...) {
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

  # covariates get the BestDose reserved names (e.g. poids, peso, vekt -> weight)
  ren <- if (length(a$cov)) bd_cov_rename(names(a$cov), primary) else NULL
  renamed <- ren[names(ren) != unname(ren)]

  p <- bd_parse_model(a, ren)
  s <- bd_structure(p, primary)

  # covariates the equations never use are not exported
  used <- unique(unlist(lapply(c(p$sec, p$dx, p$ini, p$fa, p$lag, p$out), function(i) all.vars(i$rhs))))
  cov <- bd_covariates(a$cov, data, ren, keep = intersect(unname(ren), used))
  dropped <- setdiff(names(ren), names(ren)[unname(ren) %in% used])

  r <- bd_routes(p, data, s$central)
  p <- bd_prune_inputs(p, r)

  # support points, in parameter order
  sp <- as.data.frame(PM_result$final$popPoints)
  names(sp) <- tolower(names(sp))
  missing_cols <- setdiff(c(primary, "prob"), names(sp))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c("x" = "Support points are missing {.val {missing_cols}}."))
  }
  sp <- sp[, c(primary, "prob"), drop = FALSE]

  out_list <- lapply(seq_along(s$out_idx), function(k) {
    o <- list(name = s$out_names[k], equation = s$out_idx[k])
    if (s$is_pd[k]) {
      o$pd <- list(label = paste("Output", s$out_idx[k]), unit = "")
    }
    given <- outputs[[as.character(s$out_idx[k])]]
    if (!is.null(given$name)) o$name <- given$name
    if (s$is_pd[k]) {
      if (!is.null(given$label)) o$pd$label <- given$label
      if (!is.null(given$unit)) o$pd$unit <- given$unit
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
    route = r$routes,
    name = model_name,
    compartment = bd_compartment(s, out_list),
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
    error = bd_error(a$err, cycle$gamlam)
  )

  bd <- list(description = description, model = model, support_point = sp)
  attr(bd, "notes") <- list(renamed = renamed, dropped = dropped)
  bd
}


# ==================================================================____
# Checklist
# ==================================================================____

# Checks an exported model against the BestDose model checklist
# (dev/bestdose_example/model_checklist.md) and returns what is still wrong,
# as a list of `errors` and `warnings`. Everything that could be repaired
# automatically has been before this point.
bd_check <- function(bd) {
  errors <- character()
  warnings <- character()
  err <- function(...) errors <<- c(errors, paste0(...))
  warn <- function(...) warnings <<- c(warnings, paste0(...))

  d <- bd$description
  m <- bd$model
  prim <- names(m$primary)
  sec <- unlist(m$secondary)
  eqn <- unlist(m$equation)
  outs <- unlist(m$out)
  fa <- unlist(m$fa)
  lag <- unlist(m$lag)
  ini <- unlist(m$initial_conditions)
  covs <- names(m$covariates)

  parse_eq <- function(txt) {
    e <- tryCatch(parse(text = txt)[[1]], error = function(e) NULL)
    if (is.null(e)) {
      return(list(lhs = NULL, vars = character(), ok = FALSE))
    }
    if (bd_is_assign(e)) list(lhs = e[[2]], vars = all.vars(e[[3]]), ok = TRUE) else list(lhs = NULL, vars = all.vars(e), ok = FALSE)
  }
  sec_vars <- lapply(sec, function(x) parse_eq(x)$vars)
  fixed <- c("X", "B", "R", "Y", "dx", "lag", "fa", "t", "time")

  # description
  if (!nzchar(d$name) || !nzchar(d$drug)) err("name and drug must not be empty")
  if (!nzchar(d$description)) warn("the model description is empty")
  if (!nzchar(d$reference)) warn("the reference is empty")

  # compartments
  comp <- d$compartment
  named <- setdiff(names(comp), c("number", "outputs"))
  idx <- unlist(comp[named])
  if (!is.numeric(comp$number) || comp$number < 1 || comp$number > 10) err("compartment$number must be between 1 and 10")
  if (!"central" %in% named) err("the compartment block must have a central compartment")
  if (length(named) > 6) err("more than 6 named compartments")
  if (length(named) != comp$number) err("the number of named compartments (", length(named), ") differs from compartment$number (", comp$number, ")")
  if (anyDuplicated(idx) || any(idx < 1 | idx > 6) || any(idx > comp$number)) err("compartment indices must be unique, between 1 and 6 and not exceed compartment$number")
  if (comp$number > length(eqn)) err("compartment$number exceeds the number of state equations")
  if (comp$number < length(eqn)) warn("compartment$number (", comp$number, ") is lower than the number of state equations (", length(eqn), "), as expected for a PK/PD model")

  # outputs
  onames <- vapply(comp$outputs, function(o) o$name, character(1))
  oeq <- vapply(comp$outputs, function(o) as.integer(o$equation), integer(1))
  if (anyDuplicated(tolower(onames))) err("output names must be unique, whatever their case")
  if (anyDuplicated(oeq)) err("output equation numbers must be unique")
  if (any(oeq < 1 | oeq > 6)) err("output equations must be between 1 and 6")
  if (any(tolower(onames) %in% c("concentration", covs))) err("an output is named 'concentration' or like a covariate")
  if (length(outs) > 6) err("more than 6 outputs")
  if (length(outs) != length(oeq)) err("there must be one output equation per declared output")
  for (o in comp$outputs) {
    key <- paste0("y", o$equation)
    if (!key %in% names(outs)) err("output ", o$name, " has no equation ", key)
    if (is.null(o$pd)) {
      if (key %in% names(outs) && !bd_is_conc(outs[[key]])) err("concentration output ", key, " is not X[N] divided by a parameter")
    } else {
      if (identical(o$name, "central")) err("the central output cannot be a PD effect")
      if (!nzchar(o$pd$label)) err("PD output ", o$name, " has no label")
      if (!nzchar(o$pd$unit)) warn("PD output ", o$name, " has no unit")
      if (tolower(o$name) == "effect") warn("PD output name 'effect' is generic")
    }
  }

  # routes
  rt <- d$route
  if (anyDuplicated(rt$compartment)) warn("several routes use the same input number")
  used_r <- unique(unlist(regmatches(eqn, gregexpr("(?<=R\\[)[0-9]+(?=\\])", eqn, perl = TRUE))))
  used_b <- unique(unlist(regmatches(eqn, gregexpr("(?<=B\\[)[0-9]+(?=\\])", eqn, perl = TRUE))))
  if (length(c(used_r, used_b)) == 0) err("no dose input (R[N] or B[N]) in the equations")
  for (j in setdiff(c(used_r, used_b), as.character(rt$compartment))) err("input ", j, " is used by the equations but has no route")
  for (j in setdiff(as.character(rt$compartment), c(used_r, used_b))) warn("route with input ", j, " is not used by the equations")
  for (j in used_r) {
    if (!any(rt$route[rt$compartment == as.integer(j)] %in% c("IV", "SC"))) err("R[", j, "] is used by a route that is not IV or SC")
  }

  # covariates
  cv <- d$covariates
  if (!is.null(cv)) {
    if (any(grepl("[^a-zA-Z0-9_]", cv$names))) err("covariate names may only have letters, numbers and _")
    for (k in seq_along(cv$names)) {
      v <- cv$value[[cv$names[k]]]
      if (is.null(v)) {
        err("covariate ", cv$names[k], " has no values")
        next
      }
      if (cv$types[k] == "numeric") {
        if (!(v[[1]]$min < v[[1]]$max)) err("covariate ", cv$names[k], ": min must be lower than max")
        if (v[[1]]$default < v[[1]]$min || v[[1]]$default > v[[1]]$max) err("covariate ", cv$names[k], ": default is outside min/max")
      } else {
        if (sum(v$default) != 1) err("covariate ", cv$names[k], ": exactly one default category is required")
        if (!identical(m$covariates[[cv$names[k]]]$interp, "none")) err("categorical covariate ", cv$names[k], " must use no interpolation")
      }
    }
  }

  # primary parameters
  if (length(prim) < 2) err("at least two primary parameters are required")
  for (q in prim) if (!(m$primary[[q]]$min < m$primary[[q]]$max)) err("primary parameter ", q, ": min must be lower than max")

  # secondary parameters
  if (any(names(sec) %in% prim)) err("secondary parameter(s) share a name with a primary one: ", paste(intersect(names(sec), prim), collapse = ", "))
  reach <- function(v, seen = character()) { # variables reached through secondary parameters
    for (x in setdiff(v, seen)) {
      seen <- c(seen, x)
      if (x %in% names(sec)) seen <- union(seen, reach(sec_vars[[x]], seen))
    }
    seen
  }
  for (nm in names(sec)) {
    if (!grepl("=", sec[[nm]], fixed = TRUE)) err("secondary ", nm, " is not an equation")
    v <- sec_vars[[nm]]
    if (length(v) > 0 && !any(prim %in% reach(v))) err("secondary ", nm, " uses no primary parameter")
  }
  in_sec <- unique(unlist(lapply(sec_vars, reach)))
  for (cv_name in setdiff(covs, in_sec)) warn("covariate ", cv_name, " is not used by any secondary equation")

  # every symbol is defined, every parameter used
  all_txt <- c(sec, eqn, outs, fa, lag, ini)
  all_v <- unique(unlist(lapply(all_txt, function(x) parse_eq(x)$vars)))
  unknown <- setdiff(all_v, c(prim, names(sec), covs, fixed))
  if (length(unknown) > 0) err("undefined variable(s): ", paste(unknown, collapse = ", "))
  after_sec <- unique(unlist(lapply(c(eqn, outs, fa, lag, ini), function(x) parse_eq(x)$vars)))
  expected <- c(names(sec), setdiff(prim, in_sec))
  for (q in setdiff(expected, reach(after_sec))) err("parameter ", q, " is not used by the state, output, fa or lag equations")

  # structure of the equations
  if (length(eqn) > 10) err("more than 10 differential equations")
  for (nm in names(eqn)) if (!grepl(paste0("^dx\\[", sub("^dx", "", nm), "\\]\\s*="), eqn[[nm]])) err("equation ", nm, " is not written dx[N] = ...")
  for (nm in names(outs)) if (!grepl("^Y\\[[1-6]\\]\\s*=", outs[[nm]])) err("output ", nm, " is not written Y[N] = ...")
  for (nm in names(ini)) if (!grepl(paste0("^X\\[", sub("^x", "", nm), "\\]\\s*="), ini[[nm]])) err("initial condition ", nm, " is not written X[N] = ...")
  for (nm in names(fa)) if (!grepl("^fa\\[[0-9]+\\]\\s*=", fa[[nm]], ignore.case = TRUE)) err("fa ", nm, " is not written fa[N] = ...")
  for (nm in names(lag)) if (!grepl("^lag\\[[0-9]+\\]\\s*=", lag[[nm]], ignore.case = TRUE)) err("lag ", nm, " is not written lag[N] = ...")

  # error model
  er <- m$error
  if (!er$type %in% c("additional", "proportional")) err("error type must be additional or proportional")
  if (!(er$initial_value > 0)) err("the error initial_value must be greater than zero")
  if (length(er$coefficient) > length(outs)) err("more error rows than outputs")
  for (k in seq_along(er$coefficient)) {
    cf <- unlist(er$coefficient[[k]][c("c0", "c1", "c2", "c3")])
    if (all(cf == 0)) warn("error row ", k, " has only zero coefficients")
    ev <- if (k == 1) er$initial_value else er$coefficient[[k]]$initial_value
    if (!(ev > 0)) err("error row ", k, ": initial_value must be greater than zero")
  }
  n_pd <- sum(vapply(comp$outputs, function(o) !is.null(o$pd), logical(1)))
  if (n_pd > 0 && length(er$coefficient) < length(outs)) warn("a PD effect has no error row of its own")

  # PD
  if (any(!d$pd_parameters %in% prim)) err("a PD parameter is not a primary parameter")
  if (n_pd > 0 && length(d$pd_parameters) == 0) warn("PD effect output(s) but no PD parameter")
  if (n_pd == 0 && length(d$pd_parameters) > 0) warn("PD parameter(s) but no PD effect output")

  # support points
  sp <- bd$support_point
  if (abs(sum(sp$prob) - 1) > 0.01) warn("support point probabilities sum to ", round(sum(sp$prob), 3))
  if (!all(prim %in% names(sp))) err("support points do not cover every primary parameter")

  list(errors = errors, warnings = warnings)
}

# Asks one question in the console. Enter keeps the default (shown in brackets).
bd_ask <- function(question, default = "", required = FALSE, .read = readline) {
  repeat {
    shown <- if (nzchar(default)) paste0(question, " [", default, "]: ") else paste0(question, ": ")
    answer <- trimws(.read(shown))
    if (nzchar(answer)) {
      return(answer)
    }
    if (nzchar(default) || !required) {
      return(default)
    }
    cat("An answer is required.\n")
  }
}

# Asks for what a run cannot provide and patches the exported model. Every
# question has a default, so Enter keeps what was derived from the run.
bd_interview <- function(bd, ask = bd_ask) {
  cat("\nBestDose export - press Enter to keep the value in brackets.\n")
  bd$description$description <- ask("Short model description", bd$description$description)
  bd$description$reference <- ask("Reference", bd$description$reference)

  cv <- bd$description$covariates
  if (!is.null(cv)) {
    for (k in seq_along(cv$names)) {
      if (cv$names[k] == "sex") next
      cat("\nCovariate '", cv$names[k], "'\n", sep = "")
      cv$label[k] <- ask("  Label", cv$label[k])
      cv$units[k] <- ask("  Unit", cv$units[k])
    }
    cv$label <- I(as.character(cv$label))
    cv$units <- I(as.character(cv$units))
    bd$description$covariates <- cv
  }

  outs <- bd$description$compartment$outputs
  for (k in seq_along(outs)) {
    if (is.null(outs[[k]]$pd)) next
    cat("\nOutput ", outs[[k]]$equation, " is a pharmacodynamic (PD) effect\n", sep = "")
    outs[[k]]$name <- ask("  Name (what is measured, e.g. platelets)", outs[[k]]$name)
    outs[[k]]$pd$label <- ask("  Label", outs[[k]]$pd$label)
    outs[[k]]$pd$unit <- ask("  Unit", outs[[k]]$pd$unit)
  }
  bd$description$compartment$outputs <- outs
  cat("\n")
  bd
}

bd_export <- function(run, path = ".", drug, model, outputs = NULL, ..., ask = interactive()) {
  if (missing(drug) || is.null(drug)) {
    drug <- if (ask) bd_ask("Drug name", required = TRUE) else NULL
  }
  if (!is.character(drug) || length(drug) != 1 || !nzchar(drug)) {
    cli::cli_abort(c("x" = "{.arg drug} must be a single non-empty character string."))
  }
  if (missing(model) || is.null(model)) {
    model <- if (ask) bd_ask("Model name (also the file name)", paste0(tolower(gsub("[^A-Za-z0-9]+", "_", drug)), "_model"), required = TRUE) else NULL
  }
  if (!is.character(model) || length(model) != 1 || !nzchar(model)) {
    cli::cli_abort(c("x" = "{.arg model} must be a single non-empty character string."))
  }
  model <- sub("\\.bdm$", "", model)

  bd <- createBDmodel(run, drug_name = drug, model_name = model, outputs = outputs, ...)
  notes <- attr(bd, "notes")
  attr(bd, "notes") <- NULL
  if (ask) {
    bd <- bd_interview(bd)
  }
  check <- bd_check(bd)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  file <- file.path(path, paste0(model, ".bdm"))
  jsonlite::write_json(bd, file, auto_unbox = TRUE, pretty = TRUE, digits = 10, null = "null", na = "null")

  cli::cli_inform(c(
    "v" = "BestDose model written to {.file {file}}",
    "i" = "{length(bd$description$compartment$outputs)} output(s), {nrow(bd$support_point)} support points, {length(bd$model$primary)} parameter(s)."
  ))
  if (length(notes$renamed) > 0) {
    cli::cli_inform(c("i" = "Covariates renamed to BestDose reserved names: {paste0(names(notes$renamed), ' -> ', notes$renamed, collapse = ', ')}."))
  }
  if (length(notes$dropped) > 0) {
    cli::cli_inform(c("i" = "Covariates not used by the equations were not exported: {notes$dropped}."))
  }
  cv <- bd$description$covariates
  if ("sex" %in% cv$names) {
    other <- bd$description$covariates$value$sex$num_value[2]
    cli::cli_inform(c("i" = "{.code sex} was exported as categorical with 1 = male and {other} = female. Check that this is your coding."))
  }
  review <- character()
  if (!is.null(cv) && any(cv$units[cv$names != "sex"] == "")) review <- c(review, "covariate units, labels and descriptions")
  if (any(vapply(bd$description$compartment$outputs, function(o) !is.null(o$pd), logical(1)))) {
    review <- c(review, "PD output names, labels and units (see the outputs argument)")
  }
  if (length(review) > 0 && !ask) {
    cli::cli_inform(c("!" = "Not available in a Pmetrics run, please review: {review}."))
  }
  if (length(check$errors) > 0) {
    cli::cli_warn(c("x" = "The model will not pass the BestDose checks:", stats::setNames(check$errors, rep("*", length(check$errors)))))
  }
  if (length(check$warnings) > 0) {
    cli::cli_inform(c("!" = "BestDose check warnings:", stats::setNames(check$warnings, rep("*", length(check$warnings)))))
  }
  invisible(file)
}
