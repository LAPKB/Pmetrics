testthat::skip_on_cran()

# A minimal stand-in for a PM_result: export only reads these fields.
fake_run <- function(model, data, support, objective_cycles = 1) {
  n_out <- length(model$arg_list$err)
  list(
    model = model,
    data = list(data = data),
    cycle = list(data = list(
      objective = data.frame(cycle = objective_cycles, neg2ll = 100),
      gamlam = data.frame(cycle = 1, value = seq_len(n_out) + 0.5, outeq = seq_len(n_out), type = "gamma")
    )),
    final = list(popPoints = support, nsub = 3)
  )
}

dose_data <- function(...) {
  covs <- list(...)
  base <- data.frame(
    id = 1, evid = c(1, 0), time = c(0, 1), dur = c(0, NA), dose = c(600, NA),
    addl = NA, ii = NA, input = c(1, NA), out = c(NA, 1), outeq = c(NA, 1),
    cens = NA, c0 = NA, c1 = NA, c2 = NA, c3 = NA
  )
  for (n in names(covs)) base[[n]] <- covs[[n]]
  base
}

export_bd <- function(run, ...) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  file <- suppressMessages(Pmetrics:::bd_export(run, path = dir, ask = FALSE, ...))
  jsonlite::read_json(file)
}

oral_model <- function(cov_names) {
  PM_model$new(
    pri = list(ka = ab(0.1, 3), cl0 = ab(1, 20), v0 = ab(10, 100)),
    cov = stats::setNames(
      list(interp(), interp(), interp("none")),
      cov_names
    ),
    sec = eval(parse(text = sprintf(
      "function() { cl = cl0 * (%s/70)^0.75 * (%s/100); v = v0 * %s/70 * (1 + 0.1 * %s) }",
      cov_names[1], cov_names[2], cov_names[1], cov_names[3]
    ))),
    eqn = function() {
      one_comp_bolus_cl
    },
    out = function() {
      y[1] <- x[2] / v
    },
    err = list(proportional(1, c(0.1, 0.1, 0, 0)))
  )
}

oral_support <- data.frame(ka = c(1, 2), cl0 = c(5, 8), v0 = c(30, 50), prob = c(0.5, 0.5))

test_that("covariate names map to BestDose reserved names in four languages", {
  canon <- getFromNamespace("bd_cov_canonical", "Pmetrics")
  expect_equal(canon(c("wt")), "weight")
  expect_equal(unname(vapply(c("Poids", "peso", "Vekt", "TBW"), canon, "")), rep("weight", 4))
  expect_equal(unname(vapply(c("DFG", "TFG", "GFR", "ClCr"), canon, "")), rep("crcl", 4))
  expect_equal(unname(vapply(c("cr", "creatinina", "kreatinin", "SCr"), canon, "")), rep("creat", 4))
  expect_equal(unname(vapply(c("Taille", "Estatura", "Høyde", "size"), canon, "")), rep("height", 4))
  expect_equal(unname(vapply(c("IMC", "kmi"), canon, "")), rep("bmi", 2))
  expect_equal(unname(vapply(c("Sexe", "sexo", "Kjønn", "gender"), canon, "")), rep("sex", 4))
  expect_equal(canon("My Covariate"), "my_covariate")
})

test_that("an oral library model exports to a valid BestDose model", {
  for (names_set in list(
    c("Poids", "DFG", "Sexe", "Taille"),
    c("peso", "TFG", "sexo", "Estatura"),
    c("Vekt", "GFR", "Kjonn", "Hoyde")  # model identifiers must be ASCII
  )) {
    dat <- dose_data()
    dat[[names_set[1]]] <- c(60, 90)
    dat[[names_set[2]]] <- c(80, 100)
    dat[[names_set[3]]] <- c(1, 2)
    dat[[names_set[4]]] <- c(160, 180)
    j <- export_bd(fake_run(oral_model(names_set[1:3]), dat, oral_support), drug = "drug", model = "m")

    expect_equal(unlist(j$description$covariates$names), c("weight", "crcl", "sex"))
    expect_equal(unlist(j$description$covariates$types), c("numeric", "numeric", "categorical"))
    expect_equal(j$model$covariates$sex$interp, "none")
    expect_match(j$model$secondary$cl, "weight", fixed = TRUE)
  }
})

test_that("input terms the data never use are removed and the route follows the data", {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  j <- export_bd(fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support), drug = "d", model = "m")

  expect_equal(j$description$route[[1]]$route, "PO")
  expect_equal(j$description$route[[1]]$compartment, 1)
  expect_false(any(grepl("R\\[", unlist(j$model$equation))))
  expect_true(any(grepl("B\\[1\\]", unlist(j$model$equation))))
  expect_equal(j$model$equation$dx2, "dx[2] = ka * X[1] - ke * X[2]")
})

test_that("covariates no equation uses are not exported, categorical sex follows BestDose", {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2), unused = c(1, 2))
  mod <- oral_model(c("wt", "crcl", "sex"))
  mod$arg_list$cov$unused <- interp()
  j <- export_bd(fake_run(mod, dat, oral_support), drug = "d", model = "m")

  expect_false("unused" %in% unlist(j$description$covariates$names))
  sex <- j$description$covariates$value$sex
  expect_equal(vapply(sex, function(x) x$cat_value, ""), c("male", "female"))
  expect_equal(sum(vapply(sex, function(x) x$default, TRUE)), 1)
})

test_that("a PK/PD ODE model flags the PD output and parameters", {
  mod <- PM_model$new(
    pri = list(ka = ab(0.5, 6), cl = ab(2, 15), v = ab(20, 80), e0 = ab(100, 400), slope = ab(0.002, 0.03), kout = ab(0.01, 0.2)),
    ini = function() {
      X[3] <- e0
    },
    eqn = function() {
      dx[1] <- B[1] - ka * X[1]
      dx[2] <- ka * X[1] - cl / v * X[2]
      dx[3] <- kout * e0 * (1 - slope * X[2] / v) - kout * X[3]
    },
    out = function() {
      Y[1] <- X[2] / v
      Y[2] <- X[3]
    },
    err = list(proportional(1, c(0.1, 0.1, 0, 0)), additive(1, c(15, 0, 0, 0), outeq = 2))
  )
  sp <- data.frame(ka = 1, cl = 5, v = 40, e0 = 200, slope = 0.01, kout = 0.1, prob = 1)
  j <- export_bd(fake_run(mod, dose_data(), sp), drug = "d", model = "m", outputs = list("2" = list(name = "platelets", label = "Platelets", unit = "10^9/L")))

  expect_setequal(unlist(j$description$pd_parameters), c("e0", "slope", "kout"))
  eff <- j$description$compartment$outputs[[2]]
  expect_equal(eff$name, "platelets")
  expect_equal(eff$pd$unit, "10^9/L")
  expect_null(j$description$compartment$outputs[[1]]$pd)
  expect_equal(j$model$error$coefficient[[2]]$type, "additional")
  expect_equal(names(j$model$initial_conditions), "x3")
})

test_that("the estimated error value is exported, not the starting value", {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  j <- export_bd(fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support), drug = "d", model = "m")
  expect_equal(j$model$error$initial_value, 1.5)
})

test_that("bd_check reports what the export cannot repair", {
  check <- getFromNamespace("bd_check", "Pmetrics")
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  bd <- Pmetrics:::createBDmodel(fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support), "d", "m")
  attr(bd, "notes") <- NULL
  expect_length(check(bd)$errors, 0)

  bd$model$equation$dx2 <- "dx[2] = ka * X[1] - zzz * X[2]"
  expect_match(check(bd)$errors, "undefined variable", all = FALSE)
})

test_that("export asks for the names when none are given, and refuses without them", {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  run <- fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support)
  dir <- withr::local_tempdir()

  expect_error(suppressMessages(Pmetrics:::bd_export(run, path = dir, ask = FALSE)), "drug")

  asked <- character()
  testthat::local_mocked_bindings(
    bd_ask = function(question, default = "", required = FALSE) {
      asked <<- c(asked, question)
      if (grepl("^Drug", question)) "vancomycin" else if (grepl("^Model", question)) "vanco_test" else default
    },
    .package = "Pmetrics"
  )
  utils::capture.output(suppressMessages(Pmetrics:::bd_export(run, path = dir, ask = TRUE)))

  expect_true(file.exists(file.path(dir, "vanco_test.bdm")))
  expect_equal(jsonlite::read_json(file.path(dir, "vanco_test.bdm"))$description$drug, "vancomycin")
  expect_true(any(grepl("Label", asked)) && any(grepl("Unit", asked)))
})

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

internal <- function(name) getFromNamespace(name, "Pmetrics")

lib_model <- function(template, pri, out, ...) {
  PM_model$new(
    pri = pri,
    eqn = eval(parse(text = sprintf("function() { %s }", template))),
    out = out,
    err = list(proportional(1, c(0.1, 0.1, 0, 0))),
    ...
  )
}

infusion_data <- function(dur = 2) {
  dat <- dose_data()
  dat$dur[1] <- dur
  dat
}

oral_bd <- function() {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  bd <- internal("createBDmodel")(fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support), "d", "m")
  attr(bd, "notes") <- NULL
  bd
}

infusion_bd <- function() {
  mod <- lib_model("one_comp_iv_cl", list(cl = ab(1, 20), v = ab(10, 100)), function() {
    y[1] <- x[1] / v
  })
  bd <- internal("createBDmodel")(fake_run(mod, infusion_data(), data.frame(cl = c(5, 8), v = c(30, 50), prob = c(0.5, 0.5))), "d", "m")
  attr(bd, "notes") <- NULL
  bd
}

# ---------------------------------------------------------------------------
# pure helpers
# ---------------------------------------------------------------------------

test_that("bd_drop_terms removes standalone input terms from sums", {
  drop <- internal("bd_drop_terms")
  txt <- function(e) paste(deparse(e), collapse = "")
  expect_equal(txt(drop(quote(R[1] + ka * X[1] - ke * X[2]), "R1")), "ka * X[1] - ke * X[2]")
  expect_equal(txt(drop(quote(a - R[1]), "R1")), "a")
  expect_equal(txt(drop(quote(R[1] - a), "R1")), "-a")
  expect_equal(txt(drop(quote(B[1] + R[1]), "B1")), "R[1]")
  expect_null(drop(quote(B[1]), "B1"))
  expect_equal(txt(drop(quote(-B[1] + a), "B1")), "a")
  expect_equal(txt(drop(quote(ka * X[1]), "R1")), "ka * X[1]") # untouched
})

test_that("covariate ranges are always valid", {
  rng <- internal("bd_cov_range")
  const <- rng(c(1, 1))
  expect_lt(const$min, const$max)
  expect_true(const$default >= const$min && const$default <= const$max)
  expect_equal(unlist(rng(c(0, 1, 1))[c("min", "max")]), c(min = 0, max = 1))
  wide <- rng(c(43.4, 190.2))
  expect_equal(c(wide$min, wide$max), c(43, 191))
  expect_equal(unlist(rng(NULL)[c("min", "max", "default")]), c(min = 0, max = 1, default = 0))
  expect_equal(rng(c(0.51, 0.55))$min, 0.5) # narrow ranges keep one decimal
})

test_that("sex levels follow the coding found in the data", {
  sx <- internal("bd_sex_values")
  a <- sx(c(1, 0, 0))
  expect_equal(a$num_value, c(1, 0))
  expect_equal(a$default, c(FALSE, TRUE))
  b <- sx(c(1, 2, 2, 2))
  expect_equal(b$num_value, c(1, 2))
  expect_equal(b$default, c(FALSE, TRUE))
  expect_equal(sum(sx(NULL)$default), 1)
})

test_that("covariate renaming avoids clashes", {
  ren <- internal("bd_cov_rename")
  expect_equal(unname(ren(c("wt", "age"), character())), c("weight", "age"))
  both <- unname(ren(c("wt", "weight"), character()))
  expect_false(anyDuplicated(both) > 0)
  expect_equal(unname(ren("wt", "weight")), "wt") # primary parameter already called weight
  expect_equal(names(ren("Poids", character())), "poids")
})

# ---------------------------------------------------------------------------
# routes and compartments
# ---------------------------------------------------------------------------

test_that("infusion data give an IV route and keep the infusion term", {
  bd <- infusion_bd()
  expect_equal(bd$description$route$route, "IV")
  expect_equal(bd$description$route$compartment, 1)
  expect_match(bd$model$equation$dx1, "R[1]", fixed = TRUE)
})

test_that("a bolus into an infusion-only model is exported as IV", {
  mod <- lib_model("one_comp_iv_cl", list(cl = ab(1, 20), v = ab(10, 100)), function() {
    y[1] <- x[1] / v
  })
  j <- export_bd(fake_run(mod, dose_data(), data.frame(cl = c(5, 8), v = c(30, 50), prob = c(0.5, 0.5))), drug = "d", model = "m")
  expect_equal(j$description$route[[1]]$route, "IV")
  expect_match(j$model$equation$dx1, "R[1]", fixed = TRUE)
})

test_that("bolus and infusion data on a library model give both routes and keep both terms", {
  mod <- lib_model("one_comp_bolus_cl", list(ka = ab(0.1, 3), cl = ab(1, 20), v = ab(10, 100)), function() {
    y[1] <- x[2] / v
  })
  dat <- rbind(dose_data(), infusion_data())
  sp <- data.frame(ka = c(1, 2), cl = c(5, 8), v = c(30, 50), prob = c(0.5, 0.5))
  j <- export_bd(fake_run(mod, dat, sp), drug = "d", model = "m")
  routes <- vapply(j$description$route, function(r) r$route, "")
  expect_setequal(routes, c("PO", "IV"))
  eq <- unlist(j$model$equation)
  expect_true(any(grepl("B\\[1\\]", eq)) && any(grepl("R\\[1\\]", eq)))
})

test_that("without dosing data the routes come from the model", {
  mod <- lib_model("one_comp_bolus_cl", list(ka = ab(0.1, 3), cl = ab(1, 20), v = ab(10, 100)), function() {
    y[1] <- x[2] / v
  })
  sp <- data.frame(ka = c(1, 2), cl = c(5, 8), v = c(30, 50), prob = c(0.5, 0.5))
  j <- export_bd(fake_run(mod, dose_data()[0, ], sp), drug = "d", model = "m")
  routes <- vapply(j$description$route, function(r) r$route, "")
  expect_setequal(routes, c("PO", "IV"))
})

test_that("peripheral compartments are named and numbered after the central one", {
  two <- lib_model(
    "two_comp_iv_cl", list(cl = ab(1, 20), q = ab(1, 10), v1 = ab(10, 100), v2 = ab(10, 100)),
    function() {
      y[1] <- x[1] / v1
    }
  )
  sp2 <- data.frame(cl = 5, q = 3, v1 = 30, v2 = 40, prob = 1)
  c2 <- export_bd(fake_run(two, infusion_data(), sp2), drug = "d", model = "m")$description$compartment
  expect_equal(c2$number, 2)
  expect_equal(c2$central, 1)
  expect_equal(c2$peripheral, 2)
  expect_null(c2$depot)

  three <- lib_model(
    "three_comp_iv_cl", list(cl = ab(1, 20), q2 = ab(1, 10), q3 = ab(1, 10), v1 = ab(10, 100), v2 = ab(10, 100), v3 = ab(10, 100)),
    function() {
      y[1] <- x[1] / v1
    }
  )
  sp3 <- data.frame(cl = 5, q2 = 3, q3 = 2, v1 = 30, v2 = 40, v3 = 50, prob = 1)
  c3 <- export_bd(fake_run(three, infusion_data(), sp3), drug = "d", model = "m")$description$compartment
  expect_equal(c3$number, 3)
  expect_equal(c(c3$peripheral1, c3$peripheral2), c(2, 3))
})

test_that("lag and fa blocks are exported", {
  mod <- lib_model(
    "one_comp_bolus_cl", list(ka = ab(0.1, 3), cl = ab(1, 20), v = ab(10, 100), lag1 = ab(0, 2), fa1 = ab(0.5, 1)),
    function() {
      y[1] <- x[2] / v
    },
    lag = function() {
      lag[1] <- lag1
    },
    fa = function() {
      fa[1] <- fa1
    }
  )
  sp <- data.frame(ka = 1, cl = 5, v = 30, lag1 = 0.5, fa1 = 0.8, prob = 1)
  j <- export_bd(fake_run(mod, dose_data(), sp), drug = "d", model = "m")
  expect_equal(j$model$lag$lag1, "lag[1] = lag1")
  expect_equal(j$model$fa$fa1, "fa[1] = fa1")
})

test_that("an additive first error model is written with the BestDose spelling", {
  mod <- lib_model("one_comp_iv_cl", list(cl = ab(1, 20), v = ab(10, 100)), function() {
    y[1] <- x[1] / v
  })
  mod$arg_list$err <- list(additive(1, c(0.2, 0.1, 0, 0)))
  bd <- internal("createBDmodel")(fake_run(mod, infusion_data(), data.frame(cl = 5, v = 30, prob = 1)), "d", "m")
  expect_equal(bd$model$error$type, "additional")
  expect_equal(bd$model$error$coefficient[[1]]$c0, 0.2)
})

# ---------------------------------------------------------------------------
# options, files and messages
# ---------------------------------------------------------------------------

test_that("description overrides and output names are applied", {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  j <- export_bd(
    fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support),
    drug = "d", model = "m", reference = "Someone, 2024", description = "My text",
    outputs = list("1" = list(name = "plasma"))
  )
  expect_equal(j$description$reference, "Someone, 2024")
  expect_equal(j$description$description, "My text")
  expect_equal(j$description$compartment$outputs[[1]]$name, "plasma")
})

test_that("the file is written in a new folder and the .bdm suffix is not doubled", {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  run <- fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support)
  dir <- file.path(withr::local_tempdir(), "a", "b")
  file <- suppressMessages(internal("bd_export")(run, path = dir, drug = "d", model = "mine.bdm", ask = FALSE))
  expect_equal(basename(file), "mine.bdm")
  expect_true(file.exists(file))
})

test_that("a model that fails the checklist is exported with a warning", {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  run <- fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support)
  dir <- withr::local_tempdir()
  expect_warning(
    suppressMessages(Pmetrics:::bd_export(run, path = dir, drug = "d", model = "m", ask = FALSE, outputs = list("1" = list(name = "concentration")))),
    "BestDose checks"
  )
  expect_true(file.exists(file.path(dir, "m.bdm"))) # still written
})

test_that("renamed and dropped covariates are reported", {
  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  run <- fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support)
  dir <- withr::local_tempdir()
  expect_message(internal("bd_export")(run, path = dir, drug = "d", model = "m", ask = FALSE), "renamed to BestDose")
})

test_that("export is a method of PM_result with the interactive switch", {
  expect_true("export" %in% names(PM_result$public_methods))
  expect_named(formals(PM_result$public_methods$export), c("path", "drug", "model", "outputs", "...", "ask"))
})

# ---------------------------------------------------------------------------
# what cannot be exported
# ---------------------------------------------------------------------------

test_that("models and runs that cannot be exported are refused", {
  parse_model <- internal("bd_parse_model")
  expect_error(parse_model(list(eqn = function() {
    a <- 1
  }, out = function() {
    y[1] <- a
  })), "differential equations")
  expect_error(parse_model(list(eqn = function() {
    dx[1] <- -k * x[1]
  }, out = function() {
    a <- 1
  })), "output equations")

  dat <- dose_data(wt = c(60, 90), crcl = c(80, 100), sex = c(1, 2))
  run <- fake_run(oral_model(c("wt", "crcl", "sex")), dat, oral_support)
  create <- internal("createBDmodel")

  no_pts <- run
  no_pts$final$popPoints <- NULL
  expect_error(create(no_pts), "support points")

  short <- run
  short$final$popPoints <- oral_support[, c("ka", "cl0", "prob")]
  expect_error(create(short), "missing")

  broken <- run
  broken$model <- list(arg_list = NULL)
  expect_error(create(broken), "cannot be exported")
})

# ---------------------------------------------------------------------------
# bd_check
# ---------------------------------------------------------------------------

test_that("bd_check accepts a model that follows the checklist", {
  check <- internal("bd_check")
  expect_length(check(oral_bd())$errors, 0)
  expect_length(check(infusion_bd())$errors, 0)
})

test_that("bd_check reports compartment, output and route problems", {
  check <- internal("bd_check")
  has <- function(bd, pattern) expect_match(check(bd)$errors, pattern, all = FALSE)

  bd <- oral_bd()
  bd$description$compartment$number <- 11
  has(bd, "between 1 and 10")

  bd <- oral_bd()
  bd$description$compartment$central <- NULL
  has(bd, "central compartment")

  bd <- oral_bd()
  bd$description$compartment$outputs[[2]] <- bd$description$compartment$outputs[[1]]
  has(bd, "names must be unique")

  bd <- oral_bd()
  bd$description$route <- data.frame(route = "IV", compartment = 2L)
  has(bd, "no route")

  bd <- infusion_bd()
  bd$description$route$route <- "PO"
  has(bd, "not IV or SC")

  bd <- oral_bd()
  bd$description$compartment$outputs[[1]]$name <- "Concentration"
  has(bd, "named 'concentration'")
})

test_that("bd_check reports parameter, covariate and error problems", {
  check <- internal("bd_check")
  has <- function(bd, pattern) expect_match(check(bd)$errors, pattern, all = FALSE)
  warns <- function(bd, pattern) expect_match(check(bd)$warnings, pattern, all = FALSE)

  bd <- oral_bd()
  bd$model$primary$ka$min <- 5
  has(bd, "min must be lower than max")

  bd <- oral_bd()
  bd$model$secondary$ka <- "ka = cl0 * 2"
  has(bd, "share a name")

  bd <- oral_bd()
  bd$model$secondary$cl <- "cl = weight * 2"
  has(bd, "uses no primary parameter")

  bd <- oral_bd()
  bd$description$pd_parameters <- I("zzz")
  has(bd, "PD parameter")

  bd <- oral_bd()
  bd$description$covariates$value$weight[[1]]$min <- 1000
  has(bd, "min must be lower than max")

  bd <- oral_bd()
  bd$description$covariates$value$sex$default <- c(TRUE, TRUE)
  has(bd, "exactly one default")

  bd <- oral_bd()
  bd$model$error$initial_value <- 0
  has(bd, "greater than zero")

  bd <- oral_bd()
  bd$model$equation$dx2 <- "dx[2] = ka * X[1] - zzz * X[2]"
  has(bd, "undefined variable")

  bd <- oral_bd()
  bd$model$equation$dx2 <- "dx2 = ka * X[1]"
  has(bd, "not written dx")

  bd <- oral_bd()
  bd$model$error$coefficient[[1]][c("c0", "c1", "c2", "c3")] <- 0
  warns(bd, "zero coefficients")

  bd <- oral_bd()
  bd$support_point$prob <- c(0.2, 0.2)
  warns(bd, "sum to")

  bd <- oral_bd()
  bd$model$covariates$extra <- list(interp = "none")
  warns(bd, "not used by any secondary")

  bd <- oral_bd()
  bd$description$reference <- ""
  warns(bd, "reference is empty")
})

test_that("bd_check warns about PD outputs without a unit or with a generic name", {
  check <- internal("bd_check")
  mod <- PM_model$new(
    pri = list(ka = ab(0.5, 6), cl = ab(2, 15), v = ab(20, 80), e0 = ab(100, 400), kout = ab(0.01, 0.2)),
    ini = function() {
      X[3] <- e0
    },
    eqn = function() {
      dx[1] <- B[1] - ka * X[1]
      dx[2] <- ka * X[1] - cl / v * X[2]
      dx[3] <- kout * e0 * (1 - 0.01 * X[2] / v) - kout * X[3]
    },
    out = function() {
      Y[1] <- X[2] / v
      Y[2] <- X[3]
    },
    err = list(proportional(1, c(0.1, 0.1, 0, 0)), additive(1, c(15, 0, 0, 0), outeq = 2))
  )
  sp <- data.frame(ka = 1, cl = 5, v = 40, e0 = 200, kout = 0.1, prob = 1)
  bd <- internal("createBDmodel")(fake_run(mod, dose_data(), sp), "d", "m", outputs = list("2" = list(name = "effect")))
  attr(bd, "notes") <- NULL
  w <- check(bd)$warnings
  expect_match(w, "has no unit", all = FALSE)
  expect_match(w, "generic", all = FALSE)

  bd$description$compartment$outputs[[2]]$pd$label <- ""
  expect_match(check(bd)$errors, "has no label", all = FALSE)
  bd$description$compartment$outputs[[1]]$pd <- list(label = "x", unit = "y")
  expect_match(check(bd)$errors, "central output cannot be a PD effect", all = FALSE)
})
