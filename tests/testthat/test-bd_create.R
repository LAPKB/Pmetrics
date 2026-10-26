testthat::skip_on_cran()

# Unit tests for the small functions behind PM_result$export() (R/BD_create.R).
# The end to end behavior is tested in test-export_bestdose.R.

internal <- function(name) getFromNamespace(name, "Pmetrics")
txt <- function(e) paste(deparse(e), collapse = "")

# the model blocks of a list of functions, parsed the way the export does
parse_blocks <- function(...) internal("bd_parse_model")(list(...))

# ---------------------------------------------------------------------------
# expression helpers
# ---------------------------------------------------------------------------

test_that("bd_statements reads function bodies", {
  st <- internal("bd_statements")
  expect_equal(st(NULL), list())
  expect_length(st(function() {
    a <- 1
    b <- 2
  }), 2)
  expect_length(st(function() a + 1), 1) # body without braces
})

test_that("bd_norm writes BestDose notation", {
  norm <- internal("bd_norm")
  expect_equal(txt(norm(quote(Ke <- CL / V))), "ke = cl/v")
  expect_equal(txt(norm(quote(dX[1] <- rateiv[1] - x[1]))), "dx[1] = R[1] - X[1]")
  expect_equal(txt(norm(quote(bolus[2] + b[1] + y[1]))), "B[2] + B[1] + Y[1]")
  expect_equal(txt(norm(quote(lag[1]))), "lag[1]")
  expect_equal(txt(norm(quote(if (T) F else 1))), "if (T) F else 1") # T and F are kept
  expect_equal(txt(norm(quote(wt * x[1]), c(wt = "weight"))), "weight * X[1]")
  expect_equal(txt(norm(quote(f(a, digits = 2)))), "f(a, digits = 2)") # named arguments
  expect_equal(norm(3), 3)
})

test_that("bd_item describes a statement", {
  item <- internal("bd_item")
  a <- item(quote(Cl <- Cl0 * 2))
  expect_equal(c(a$name, a$text), c("cl", "cl = cl0 * 2"))
  expect_true(is.na(a$idx))

  b <- item(quote(dX[3] <- -k * X[3]))
  expect_equal(c(b$name, b$idx), c("dx", 3))

  c <- item(quote(if (a) b else c))
  expect_true(is.na(c$name))

  d <- internal("bd_set_rhs")(a, quote(7))
  expect_equal(d$text, "cl = 7")
})

test_that("bd_text puts spaces around divisions", {
  expect_equal(internal("bd_text")(quote(a/b)), "a / b")
})

test_that("bd_index_refs finds literal indices", {
  refs <- internal("bd_index_refs")
  expect_equal(refs(quote(X[1] + 2 * X[3] - B[1]), "X"), c(1L, 3L))
  expect_equal(refs(quote(a), "X"), integer())
  expect_equal(refs(quote(X[i]), "X"), integer()) # not literal
})

test_that("bd_named keys statements by what they assign", {
  item <- internal("bd_item")
  named <- internal("bd_named")
  expect_null(named(list()))
  out <- named(list(item(quote(dx[1] <- 1)), item(quote(k <- 2)), item(quote(if (a) b)), item(quote(k <- 3))))
  expect_equal(names(out), c("dx1", "k", "expr3", "k.1"))
})

test_that("bd_deps follows assignments", {
  deps <- internal("bd_deps")
  d <- deps(quote(a * X[2]), list(a = quote(b + X[1]), b = quote(c)))
  expect_setequal(d$vars, c("a", "X", "b", "c"))
  expect_setequal(d$states, c(1L, 2L))
})

# ---------------------------------------------------------------------------
# reserved covariate names
# ---------------------------------------------------------------------------

test_that("bd_ascii_key removes accents and symbols", {
  key <- internal("bd_ascii_key")
  expect_equal(key("Kjønn"), "kjonn")
  expect_equal(key("Taille (cm)"), "taille_cm")
  expect_equal(key("  Àge "), "age")
  expect_equal(key("a.b-c"), "a_b_c")
})

test_that("every reserved name and its aliases are recognised", {
  canon <- internal("bd_cov_canonical")
  keywords <- internal("bd_cov_keywords")
  for (reserved in names(keywords)) {
    expect_equal(canon(reserved), reserved)
    for (alias in keywords[[reserved]]) {
      expect_equal(canon(alias), reserved, info = alias)
    }
  }
  expect_equal(canon("Poids Corporel"), "weight")
  expect_equal(canon("%%"), "cov") # nothing usable left
  expect_equal(canon("Albumin"), "albumin")
})

test_that("covariate information is known for reserved names and made up for others", {
  info <- internal("bd_cov_info")
  expect_equal(info("weight")[2], "kg")
  expect_equal(info("crcl")[2], "mL/min")
  expect_equal(info("sex")[2], "")
  expect_equal(info("zzz"), c("Zzz", "", "Covariate zzz"))
})

# ---------------------------------------------------------------------------
# covariate values
# ---------------------------------------------------------------------------

test_that("bd_cov_range handles every kind of covariate", {
  rng <- internal("bd_cov_range")
  expect_equal(rng(c("a", "b")), list(min = 0, max = 1, default = 0)) # no number at all
  expect_equal(unlist(rng(c(0, 1, 1, 1))), c(min = 0, max = 1, default = 1))
  expect_equal(unlist(rng(c(20, 80, 35))), c(min = 20, max = 80, default = 35))
  expect_equal(unlist(rng(c(1.52, 1.81))), c(min = 1.5, max = 1.9, default = 1.67))

  for (v in list(c(5, 5), c(0, 0, 0), c(-3, -3), c(2, 2, 2))) {
    r <- rng(v)
    expect_lt(r$min, r$max)
    expect_true(r$default >= r$min && r$default <= r$max)
  }
  expect_equal(unlist(rng(c(5, 5))[c("min", "max")]), c(min = 2.5, max = 7.5))
})

test_that("bd_sex_values gives exactly one default", {
  sx <- internal("bd_sex_values")
  for (codes in list(c(1, 0), c(1, 2), c(2, 2, 1), NULL, "a")) {
    v <- sx(codes)
    expect_equal(v$cat_value, c("male", "female"))
    expect_equal(sum(v$default), 1)
  }
})

# ---------------------------------------------------------------------------
# error model
# ---------------------------------------------------------------------------

test_that("bd_error refuses a model without error model", {
  expect_error(internal("bd_error")(list(), NULL), "no error model")
})

# ---------------------------------------------------------------------------
# model blocks and structure
# ---------------------------------------------------------------------------

two_comp <- function() {
  parse_blocks(
    eqn = function() {
      dx[1] <- -(k10 + k12) * x[1] + k21 * x[2] + rateiv[1]
      dx[2] <- k12 * x[1] - k21 * x[2]
    },
    out = function() {
      y[1] <- x[1] / v1
      y[2] <- x[2] / v2
    }
  )
}

test_that("bd_parse_model splits blocks and keeps local assignments as secondary", {
  p <- parse_blocks(
    sec = function() {
      v <- v0 * wt
    },
    eqn = function() {
      k <- cl / v
      dx[1] <- rateiv[1] - k * x[1]
    },
    out = function() {
      y[1] <- x[1] / v
    }
  )
  expect_equal(vapply(p$sec, function(i) i$name, ""), c("v", "k"))
  expect_equal(vapply(p$dx, function(i) i$idx, 1L), 1L)
  expect_setequal(names(p$defs), c("v", "k"))
})

test_that("bd_parse_model expands library templates", {
  p <- parse_blocks(eqn = function() {
    one_comp_iv_cl
  }, out = function() {
    y[1] <- x[1] / v
  })
  expect_equal(vapply(p$sec, function(i) i$text, ""), "ke = cl / v")
  expect_equal(p$dx[[1]]$text, "dx[1] = R[1] - ke * X[1]")
})

test_that("bd_structure names the compartments", {
  p <- two_comp()
  s <- internal("bd_structure")(p, c("k10", "k12", "k21", "v1", "v2"))
  expect_equal(s$central, 1)
  expect_equal(s$peripheral, 2L)
  expect_length(s$depot, 0)
  expect_equal(s$out_names, c("central", "peripheral")) # a second concentration, not PD
  expect_false(any(s$is_pd))
  expect_length(s$pd_parameters, 0)
})

test_that("bd_structure names a second concentration that is not peripheral", {
  p <- parse_blocks(
    eqn = function() {
      dx[1] <- rateiv[1] - k * x[1]
    },
    out = function() {
      y[1] <- x[1] / v
      y[2] <- x[1] / v * 1000
    }
  )
  s <- internal("bd_structure")(p, c("k", "v"))
  expect_equal(s$out_names, c("central", "output2"))
})

test_that("bd_structure falls back to the first compartment when the output uses no state", {
  p <- parse_blocks(
    eqn = function() {
      dx[1] <- rateiv[1] - k * x[1]
    },
    out = function() {
      y[1] <- c0
    }
  )
  s <- internal("bd_structure")(p, "k")
  expect_equal(s$central, 1)
})

test_that("bd_compartment numbers central, peripheral then absorption compartments", {
  comp <- internal("bd_compartment")
  s <- list(pk = 1:4, peripheral = c(2L, 3L), depot = 4L)
  out <- comp(s, list())
  expect_equal(unlist(out[c("number", "central", "peripheral1", "peripheral2", "depot")]), c(
    number = 4, central = 1, peripheral1 = 2, peripheral2 = 3, depot = 4
  ))
  one <- comp(list(pk = 1L, peripheral = integer(), depot = integer()), list())
  expect_equal(names(one), c("number", "central", "outputs"))
})

# ---------------------------------------------------------------------------
# routes
# ---------------------------------------------------------------------------

test_that("bd_routes defaults to IV when the model has no input", {
  p <- parse_blocks(eqn = function() {
    dx[1] <- -k * x[1]
  }, out = function() {
    y[1] <- x[1] / v
  })
  r <- internal("bd_routes")(p, NULL, 1)
  expect_equal(r$routes, data.frame(route = "IV", compartment = 1L))
})

test_that("bd_routes reads doses without an input column or with missing inputs", {
  p <- parse_blocks(eqn = function() {
    dx[1] <- b[1] - k * x[1]
  }, out = function() {
    y[1] <- x[1] / v
  })
  dat <- data.frame(evid = c(1, 1, 0), dur = c(0, NA, NA))
  r <- internal("bd_routes")(p, dat, 1)
  expect_equal(r$routes$route, "IV") # bolus straight into the central compartment
  expect_equal(r$use_b, 1)
})

test_that("bd_prune_inputs leaves the model alone without dosing information", {
  p <- two_comp()
  expect_identical(internal("bd_prune_inputs")(p, list(inputs = 1, use_r = NULL, use_b = NULL)), p)
})

test_that("bd_prune_inputs keeps a model whose inputs are all used", {
  p <- two_comp()
  expect_identical(internal("bd_prune_inputs")(p, list(inputs = 1, use_r = 1, use_b = 1)), p)
})

test_that("bd_prune_inputs removes the unused inputs", {
  p <- parse_blocks(eqn = function() {
    dx[1] <- b[1] + rateiv[1] - k * x[1]
  }, out = function() {
    y[1] <- x[1] / v
  })
  out <- internal("bd_prune_inputs")(p, list(inputs = 1, use_r = 1, use_b = integer()))
  expect_equal(out$dx[[1]]$text, "dx[1] = R[1] - k * X[1]")
  empty <- internal("bd_prune_inputs")(p, list(inputs = 1, use_r = integer(), use_b = integer()))
  expect_equal(empty$dx[[1]]$text, "dx[1] = -(k * X[1])")
})

test_that("bd_drop_terms handles every branch", {
  drop <- internal("bd_drop_terms")
  expect_null(drop(quote(B[1] + R[1]), c("B1", "R1")))
  expect_equal(txt(drop(quote(-a), "R1")), "-a")
  expect_equal(txt(drop(quote(-(R[1] + a)), "R1")), "-(R[1] + a)") # inside parentheses is left alone
  expect_null(drop(quote(-R[1]), "R1"))
})

# ---------------------------------------------------------------------------
# covariate blocks
# ---------------------------------------------------------------------------

test_that("bd_covariates returns nothing when no covariate is used", {
  r <- internal("bd_covariates")(list(wt = 1), data.frame(wt = 70), c(wt = "weight"), keep = character())
  expect_null(r$model)
  expect_null(r$description)
})

test_that("bd_covariates describes numeric and categorical covariates", {
  dat <- data.frame(wt = c(60, 90), sexe = c(1, 2), other = c(1, 2))
  ren <- c(wt = "weight", sexe = "sex", other = "other")
  r <- internal("bd_covariates")(list(wt = 1, sexe = 1, other = 0), dat, ren, keep = c("weight", "sex"))

  expect_equal(as.character(r$description$names), c("weight", "sex"))
  expect_equal(as.character(r$description$types), c("numeric", "categorical"))
  expect_equal(r$model$sex$interp, "none") # categorical is never interpolated
  expect_equal(r$model$weight$interp, "linear")
  expect_equal(r$description$value$weight[[1]]$min, 60)
  expect_equal(r$description$value$sex$cat_value, c("male", "female"))
})

test_that("bd_covariates copes with a covariate missing from the data", {
  r <- internal("bd_covariates")(list(wt = 1), NULL, c(wt = "weight"), keep = "weight")
  expect_equal(r$description$value$weight[[1]]$min, 0)
  expect_equal(r$description$value$weight[[1]]$max, 1)
})

# ---------------------------------------------------------------------------
# console questions
# ---------------------------------------------------------------------------

test_that("bd_ask returns the answer, the default, or insists when an answer is required", {
  ask <- internal("bd_ask")
  answers <- function(...) {
    given <- c(...)
    i <- 0
    function(prompt) {
      i <<- i + 1
      given[i]
    }
  }
  expect_equal(ask("q", "def", .read = answers("  typed ")), "typed")
  expect_equal(ask("q", "def", .read = answers("")), "def")
  expect_equal(ask("q", .read = answers("")), "")
  expect_output(res <- ask("q", required = TRUE, .read = answers("", "", "ok")), "answer is required")
  expect_equal(res, "ok")
})

test_that("bd_ask shows the default in brackets", {
  shown <- NULL
  internal("bd_ask")("Unit", "kg", .read = function(prompt) {
    shown <<- prompt
    ""
  })
  expect_equal(shown, "Unit [kg]: ")
})

test_that("bd_interview asks for PD outputs and covariates", {
  bd <- list(description = list(
    description = "old", reference = "ref",
    covariates = list(
      names = I(c("weight", "sex")), label = I(c("Weight", "Sex")), units = I(c("", "")),
      types = I(c("numeric", "categorical"))
    ),
    compartment = list(outputs = list(
      list(name = "central", equation = 1),
      list(name = "pd2", equation = 2, pd = list(label = "Output 2", unit = ""))
    ))
  ))
  asked <- character()
  fake <- function(question, default = "", required = FALSE) {
    asked <<- c(asked, question)
    paste0("<", trimws(question), ">")
  }
  out <- utils::capture.output(res <- internal("bd_interview")(bd, ask = fake))

  expect_equal(res$description$description, "<Short model description>")
  expect_equal(as.character(res$description$covariates$label)[1], "<Label>")
  expect_equal(as.character(res$description$covariates$units)[1], "<Unit>")
  expect_equal(as.character(res$description$covariates$label)[2], "Sex") # sex is not asked
  pd <- res$description$compartment$outputs[[2]]
  expect_equal(c(pd$pd$label, pd$pd$unit), c("<Label>", "<Unit>"))
  expect_match(pd$name, "Name")
  expect_equal(res$description$compartment$outputs[[1]]$name, "central")
  expect_length(asked, 2 + 2 + 3)
})

# ---------------------------------------------------------------------------
# bd_check on hand made input
# ---------------------------------------------------------------------------

test_that("bd_check does not fail on an equation it cannot parse", {
  minimal <- list(
    description = list(
      drug = "d", name = "m", description = "x", reference = "y",
      compartment = list(number = 1, central = 1, outputs = list(list(name = "central", equation = 1))),
      route = data.frame(route = "IV", compartment = 1L), covariates = NULL, pd_parameters = I(character())
    ),
    model = list(
      primary = list(k = list(type = "ab", min = 0, max = 1), v = list(type = "ab", min = 0, max = 1)),
      covariates = NULL, secondary = NULL, initial_conditions = NULL, fa = NULL, lag = NULL,
      equation = list(dx1 = "dx[1] = ((", dx2 = "dx[1] = R[1] - k * X[1]"),
      out = list(y1 = "Y[1] = X[1] / v"),
      error = list(
        type = "proportional", initial_value = 1,
        coefficient = list(list(c0 = 0.1, c1 = 0, c2 = 0, c3 = 0))
      )
    ),
    support_point = data.frame(k = 0.5, v = 1, prob = 1)
  )
  res <- internal("bd_check")(minimal)
  expect_type(res$errors, "character")
  expect_true(any(grepl("not written dx", res$errors)))
})

# ---------------------------------------------------------------------------
# export arguments
# ---------------------------------------------------------------------------

test_that("bd_export validates the names it is given", {
  export <- internal("bd_export")
  expect_error(export(list(), drug = c("a", "b"), model = "m", ask = FALSE), "drug")
  expect_error(export(list(), drug = "", model = "m", ask = FALSE), "drug")
  expect_error(export(list(), drug = "d", model = 1, ask = FALSE), "model")
  expect_error(export(list(), drug = "d", ask = FALSE), "model")
})

# ---------------------------------------------------------------------------
# primary parameters and error model
# ---------------------------------------------------------------------------

test_that("bd_primary writes lower case names and ab ranges", {
  out <- internal("bd_primary")(list(Ka = list(min = 0, max = 1), V = list(min = 5, max = 9)))
  expect_equal(names(out), c("ka", "v"))
  expect_equal(out$v, list(type = "ab", min = 5, max = 9))
})

test_that("bd_error orders the rows by output and uses the estimated values", {
  err <- function(type, initial, coeff, fixed = FALSE, outeq = 1) {
    list(type = type, initial = initial, coeff = coeff, fixed = fixed, outeq = outeq)
  }
  cyc <- data.frame(cycle = c(1, 2, 1, 2), value = c(5, 3, 7, 4), outeq = c(1, 1, 2, 2))
  res <- internal("bd_error")(list(err("additive", 2, c(1, 0, 0, 0), outeq = 2), err("proportional", 5, c(0, 0.1), outeq = 1)), cyc)

  expect_equal(res$type, "proportional")
  expect_equal(res$initial_value, 3) # last cycle of output 1
  expect_length(res$coefficient, 2)
  expect_equal(unlist(res$coefficient[[1]]), c(c0 = 0, c1 = 0.1, c2 = 0, c3 = 0)) # padded to four
  expect_equal(res$coefficient[[2]]$type, "additional")
  expect_equal(res$coefficient[[2]]$initial_value, 4)
})

test_that("bd_error keeps the starting value of a fixed error and without cycle information", {
  e <- list(type = "proportional", initial = 9, coeff = c(0, 0.1, 0, 0), fixed = TRUE, outeq = 1)
  expect_equal(internal("bd_error")(list(e), data.frame(cycle = 1, value = 1, outeq = 1))$initial_value, 9)
  e$fixed <- FALSE
  expect_equal(internal("bd_error")(list(e), NULL)$initial_value, 9)
  expect_equal(internal("bd_error")(list(e), data.frame(cycle = 1, value = 1, outeq = 3))$initial_value, 9) # none for this output
})

# ---------------------------------------------------------------------------
# bd_check on a hand made model
# ---------------------------------------------------------------------------

hand_bd <- function() {
  list(
    description = list(
      drug = "d", name = "m", description = "x", reference = "y",
      compartment = list(number = 1, central = 1, outputs = list(list(name = "central", equation = 1))),
      route = data.frame(route = "IV", compartment = 1L),
      covariates = list(
        number = 1, names = I("weight"), label = I("Weight"), units = I("kg"), types = I("numeric"),
        value = list(weight = list(list(min = 40, max = 100, default = 70))), description = list(weight = "w")
      ),
      pd_parameters = I(character())
    ),
    model = list(
      primary = list(k = list(type = "ab", min = 0, max = 1), v = list(type = "ab", min = 1, max = 9)),
      covariates = list(weight = list(interp = "linear")),
      secondary = list(cl = "cl = k * weight"), initial_conditions = NULL, fa = NULL, lag = NULL,
      equation = list(dx1 = "dx[1] = R[1] - cl * X[1]"),
      out = list(y1 = "Y[1] = X[1] / v"),
      error = list(
        type = "proportional", initial_value = 1,
        coefficient = list(list(c0 = 0.1, c1 = 0, c2 = 0, c3 = 0))
      )
    ),
    support_point = data.frame(k = 0.5, v = 1, prob = 1)
  )
}

test_that("bd_check accepts the hand made model", {
  res <- internal("bd_check")(hand_bd())
  expect_equal(res$errors, character())
  expect_equal(res$warnings, character())
})

test_that("bd_check validates covariate descriptions", {
  check <- internal("bd_check")
  has <- function(bd, pattern) expect_match(check(bd)$errors, pattern, all = FALSE)

  bd <- hand_bd()
  bd$description$covariates$names <- I("we ight")
  res <- check(bd)$errors
  expect_match(res, "letters, numbers", all = FALSE)
  expect_match(res, "has no values", all = FALSE) # the name no longer matches its values

  bd <- hand_bd()
  bd$description$covariates$value$weight[[1]]$min <- 100
  has(bd, "min must be lower than max")

  bd <- hand_bd()
  bd$description$covariates$value$weight[[1]]$default <- 500
  has(bd, "outside min/max")

  bd <- hand_bd()
  bd$description$covariates$types <- I("categorical")
  bd$description$covariates$value$weight <- data.frame(cat_value = c("a", "b"), num_value = 1:2, default = c(TRUE, TRUE))
  res <- check(bd)$errors
  expect_match(res, "exactly one default", all = FALSE)
  expect_match(res, "no interpolation", all = FALSE)
})

test_that("bd_check validates secondary equations", {
  check <- internal("bd_check")
  has <- function(bd, pattern) expect_match(check(bd)$errors, pattern, all = FALSE)

  bd <- hand_bd()
  bd$model$secondary$cl <- "cl * 5"
  has(bd, "not an equation")

  bd <- hand_bd()
  bd$model$secondary$cl <- "cl = weight * 2"
  has(bd, "uses no primary parameter")

  bd <- hand_bd()
  bd$model$secondary$cl <- "cl = 5" # a constant is a fixed parameter
  expect_false(any(grepl("cl", check(bd)$errors)))
})

test_that("bd_check validates PD outputs", {
  check <- internal("bd_check")
  bd <- hand_bd()
  bd$description$compartment$outputs[[2]] <- list(name = "effect", equation = 2, pd = list(label = "", unit = ""))
  bd$model$out$y2 <- "Y[2] = X[1]"
  res <- check(bd)
  expect_match(res$errors, "has no label", all = FALSE)
  expect_match(res$warnings, "has no unit", all = FALSE)
  expect_match(res$warnings, "generic", all = FALSE)

  bd$description$compartment$outputs[[1]]$pd <- list(label = "x", unit = "y")
  expect_match(check(bd)$errors, "central output cannot be a PD effect", all = FALSE)
})

# ---------------------------------------------------------------------------
# more small functions
# ---------------------------------------------------------------------------

test_that("bd_cov_rename maps names to reserved ones and avoids clashes", {
  ren <- internal("bd_cov_rename")
  expect_equal(ren(c("Poids", "age"), character()), c(poids = "weight", age = "age"))
  expect_equal(unname(ren(c("wt", "weight"), character())[1]), "wt") # would duplicate "weight"
  expect_equal(unname(ren("wt", "weight")), "wt") # a primary parameter is already called weight
  expect_equal(names(ren(c("A b", "a_b"), character())), c("a b", "a_b"))
  expect_false(anyDuplicated(ren(c("A b", "a_b"), character())) > 0)
})

test_that("bd_parse_model refuses models without differential or output equations", {
  expect_error(parse_blocks(eqn = function() {
    a <- 1
  }, out = function() {
    y[1] <- a
  }), "differential equations")
  expect_error(parse_blocks(eqn = function() {
    dx[1] <- -k * x[1]
  }, out = function() {
    a <- 1
  }), "output equations")
})

test_that("bd_structure flags an output on a non PK compartment as PD", {
  p <- parse_blocks(
    eqn = function() {
      dx[1] <- rateiv[1] - k * x[1]
      dx[2] <- kin - kout * x[2] - slope * x[1] / v
    },
    ini = function() {
      x[2] <- kin / kout
    },
    out = function() {
      y[1] <- x[1] / v
      y[2] <- x[2]
    }
  )
  s <- internal("bd_structure")(p, c("k", "v", "kin", "kout", "slope"))
  expect_equal(s$is_pd, c(FALSE, TRUE))
  expect_equal(s$out_names, c("central", "pd2"))
  expect_equal(s$pk, 1L)
  expect_setequal(s$pd_parameters, c("kin", "kout", "slope")) # v is shared with the PK part
})

test_that("bd_routes gives an IV route for infusions", {
  p <- parse_blocks(eqn = function() {
    dx[1] <- rateiv[1] - k * x[1]
  }, out = function() {
    y[1] <- x[1] / v
  })
  dat <- data.frame(evid = c(1, 0), dur = c(2, NA), input = c(1, NA))
  r <- internal("bd_routes")(p, dat, 1)
  expect_equal(r$routes, data.frame(route = "IV", compartment = 1L))
  expect_equal(r$use_r, 1)
  expect_length(r$use_b, 0)
})

test_that("createBDmodel refuses runs it cannot export", {
  create <- internal("createBDmodel")
  expect_error(create(list(model = list(arg_list = NULL))), "cannot be exported")
  ok_model <- list(arg_list = list(pri = list(k = 1), eqn = function() NULL, out = function() NULL))
  expect_error(create(list(model = ok_model, final = list(popPoints = NULL))), "support points")
})

# ---------------------------------------------------------------------------
# a whole export from plain lists (no compiled model needed)
# ---------------------------------------------------------------------------

plain_run <- function() {
  list(
    model = list(arg_list = list(
      pri = list(K = list(min = 0, max = 1), V = list(min = 5, max = 50)),
      cov = list(Poids = 1, unused = 0),
      sec = function() {
        kk <- K * (Poids / 70)
      },
      eqn = function() {
        dx[1] <- rateiv[1] - kk * x[1]
      },
      out = function() {
        y[1] <- x[1] / V
      },
      err = list(list(type = "proportional", initial = 2, coeff = c(0.1, 0.1), fixed = FALSE, outeq = 1))
    )),
    data = list(data = data.frame(
      id = 1, evid = c(1, 0), time = c(0, 1), dur = c(1, NA), dose = c(100, NA), input = c(1, NA),
      Poids = c(60, 80), unused = c(1, 2)
    )),
    cycle = list(data = list(
      objective = data.frame(cycle = 1:3, neg2ll = c(30, 20, 10)),
      gamlam = data.frame(cycle = 1:3, value = c(3, 2, 1.5), outeq = 1, type = "gamma")
    )),
    final = list(popPoints = data.frame(K = c(0.2, 0.4), V = c(10, 30), prob = c(0.4, 0.6)), nsub = 12)
  )
}

test_that("createBDmodel builds every block from a run", {
  bd <- internal("createBDmodel")(plain_run(), "Drug", "mine")
  d <- bd$description
  expect_equal(c(d$drug, d$name), c("Drug", "mine"))
  expect_match(d$description, "12 subject(s) and 2 support points. Fit: 3 cycles, -2LL = 10.0", fixed = TRUE)
  expect_equal(as.character(d$covariates$names), "weight") # renamed, and the unused one left out
  expect_equal(d$route, data.frame(route = "IV", compartment = 1L))
  expect_equal(d$compartment$number, 1)
  expect_equal(as.character(d$pd_parameters), character())

  m <- bd$model
  expect_equal(names(m$primary), c("k", "v"))
  expect_equal(m$secondary$kk, "kk = k * (weight / 70)")
  expect_equal(m$equation$dx1, "dx[1] = R[1] - kk * X[1]")
  expect_equal(m$out$y1, "Y[1] = X[1] / v")
  expect_equal(m$error$initial_value, 1.5) # estimated in the last cycle
  expect_equal(names(bd$support_point), c("k", "v", "prob"))
  expect_equal(attr(bd, "notes")$renamed, c(poids = "weight"))
  expect_equal(attr(bd, "notes")$dropped, "unused")
})

test_that("bd_export writes a file that passes the checklist", {
  dir <- withr::local_tempdir()
  expect_message(
    file <- internal("bd_export")(plain_run(), path = dir, drug = "Drug", model = "mine", ask = FALSE),
    "BestDose model written"
  )
  j <- jsonlite::read_json(file)
  expect_equal(j$description$drug, "Drug")
  expect_equal(length(j$support_point), 2)
  expect_length(internal("bd_check")(internal("createBDmodel")(plain_run(), "Drug", "mine"))$errors, 0)
})

test_that("the interview patches the description and the covariate labels", {
  answers <- c("Some description", "Some reference", "Weight label", "kg")
  i <- 0
  fake <- function(question, default = "", required = FALSE) {
    i <<- i + 1
    answers[i]
  }
  bd <- internal("createBDmodel")(plain_run(), "Drug", "mine")
  utils::capture.output(res <- internal("bd_interview")(bd, ask = fake))
  expect_equal(res$description$description, "Some description")
  expect_equal(res$description$reference, "Some reference")
  expect_equal(as.character(res$description$covariates$label), "Weight label")
  expect_equal(as.character(res$description$covariates$units), "kg")
})
