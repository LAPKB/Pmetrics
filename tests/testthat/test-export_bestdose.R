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
    c("Vekt", "GFR", "Kjønn", "Høyde")
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
