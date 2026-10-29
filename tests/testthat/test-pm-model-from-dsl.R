simple_dsl <- "
name = simple_iv
kind = ode
params = k, v
states = central
outputs = cp
dx(central) = infusion(input_1) - k * central
out(cp) = central / v ~ continuous()
"

testthat::test_that("PM_model$from_dsl creates a model from DSL text", {
  model <- PM_model$from_dsl(
    simple_dsl,
    pri = list(v = ab(1, 100), k = ab(0.01, 2)),
    err = list(cp = proportional(1, c(0.1, 0, 0, 0))),
    solver = "TSIT45",
    quiet = TRUE
  )

  expect_s3_class(model, "PM_model")
  expect_identical(model$dsl, simple_dsl)
  expect_identical(model$model_list$parameters, c("k", "v"))
  expect_identical(names(model$model_list$pri), c("k", "v"))
  expect_identical(model$model_list$outputs, "cp")
  expect_identical(model$model_list$solver, "tsit45")
  expect_identical(model$model_list$route_inputs$infusion, "input_1")
  expect_identical(model$model_list$err[[1]]$outeq, "cp")
  expect_true(isTRUE(model$model_list$from_dsl))
})

testthat::test_that("PM_model$from_dsl accepts named routes and outputs without a mapping", {
  dsl <- "
name = named_routes
kind = ode
params = k
states = central
outputs = cp
dx(central) = bolus(oral) + infusion(iv) - k * central
out(cp) = central ~ continuous()
"

  model <- PM_model$from_dsl(
    dsl,
    pri = list(k = ab(0.01, 2)),
    err = list(cp = additive(1, c(1, 0, 0, 0))),
    quiet = TRUE
  )

  expect_identical(model$model_list$route_inputs$bolus, "oral")
  expect_identical(model$model_list$route_inputs$infusion, "iv")
  expect_identical(model$model_list$err[[1]]$outeq, "cp")

  # The data carries the same labels the DSL declares; nothing is renumbered.
  data <- PM_data$new(
    data.frame(
      id = c("1", "1"), time = c(0, 1), evid = c(1L, 0L),
      dose = c(100, NA), dur = c(0, NA), input = c("oral", NA),
      out = c(NA, 80), outeq = c(NA, "cp")
    ),
    quiet = TRUE
  )
  simulated <- model$sim(data, matrix(0.2, nrow = 1), quiet = TRUE)
  expect_equal(nrow(simulated), 1)

  run_root <- withr::local_tempdir(pattern = "from-dsl-fit-")
  fitted <- model$fit(
    data = data, path = run_root, run = 1, cycles = 1, points = 5,
    overwrite = TRUE, report = "none", quiet = TRUE
  )
  expect_s3_class(fitted, "PM_result")
})

testthat::test_that("PM_model$from_dsl validates Pmetrics settings", {
  expect_error(
    PM_model$from_dsl(
      simple_dsl,
      pri = list(k = ab(0.01, 2)),
      err = list(cp = proportional(1, c(0.1, 0, 0, 0))),
      quiet = TRUE
    ),
    "does not match the DSL parameters"
  )

  expect_error(
    PM_model$from_dsl(
      simple_dsl,
      pri = list(k = ab(0.01, 2), v = ab(1, 100)),
      err = list(wrong = proportional(1, c(0.1, 0, 0, 0))),
      quiet = TRUE
    ),
    "must match the DSL outputs"
  )
})

testthat::test_that("A numeric error model outeq resolves to a legacy outeq_N output label", {
  numeric_output_dsl <- gsub("cp", "outeq_1", simple_dsl, fixed = TRUE)

  model <- PM_model$from_dsl(
    numeric_output_dsl,
    pri = list(k = ab(0.01, 2), v = ab(1, 100)),
    err = list(proportional(1, c(0.1, 0, 0, 0), outeq = 1)),
    quiet = TRUE
  )

  expect_identical(model$model_list$outputs, "outeq_1")
  expect_identical(model$model_list$err[[1]]$outeq, "outeq_1")
  expect_identical(model$model_list$err[[1]]$flatten()$outeq, "outeq_1")
})

testthat::test_that("Data labels are canonicalised for the backend", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "ID,EVID,TIME,DUR,DOSE,INPUT,OUT,OUTEQ",
      "1,1,0,0,100,1,.,.",
      "1,1,1,2,100,2,.,.",
      "1,0,2,.,.,.,10,1",
      "1,0,3,.,.,.,10,cp"
    ),
    path
  )

  label_data_csv(path)

  mapped <- utils::read.csv(path, check.names = FALSE, colClasses = "character")
  expect_identical(mapped$INPUT[1], "1")
  expect_identical(mapped$INPUT[2], "2")
  expect_identical(mapped$OUTEQ[3], "1")
  # Named labels are passed through untouched.
  expect_identical(mapped$OUTEQ[4], "cp")
  # Non-identifier placeholders stay as they are.
  expect_identical(mapped$INPUT[3], ".")
})

testthat::test_that("DSL models can be rebuilt", {
  model <- PM_model$from_dsl(
    simple_dsl,
    pri = list(k = ab(0.01, 2), v = ab(1, 100)),
    err = list(cp = proportional(1, c(0.1, 0, 0, 0))),
    quiet = TRUE
  )

  rebuilt <- PM_model$new(model)
  expect_identical(rebuilt$dsl, model$dsl)
  expect_identical(rebuilt$model_list$parameters, model$model_list$parameters)
})

testthat::test_that("Numeric DSL output labels must match their position", {
  swapped <- sub("outputs = cp", "outputs = 2, 1", simple_dsl, fixed = TRUE)
  swapped <- sub("out(cp) = central / v ~ continuous()",
    "out(2) = central / v ~ continuous()\nout(1) = central ~ continuous()",
    swapped,
    fixed = TRUE
  )
  expect_error(
    PM_model$from_dsl(
      swapped,
      pri = list(k = ab(0.01, 2), v = ab(1, 100)),
      err = list(
        proportional(1, c(0.1, 0, 0, 0), outeq = 1),
        proportional(1, c(0.1, 0, 0, 0), outeq = 2)
      ),
      quiet = TRUE
    ),
    "must match their position"
  )
})

testthat::test_that("Error model names that resolve to the same output are rejected", {
  numeric_output_dsl <- gsub("cp", "outeq_1", simple_dsl, fixed = TRUE)
  expect_error(
    PM_model$from_dsl(
      numeric_output_dsl,
      pri = list(k = ab(0.01, 2), v = ab(1, 100)),
      err = list(
        `1` = proportional(1, c(0.1, 0, 0, 0)),
        outeq_1 = proportional(1, c(0.1, 0, 0, 0))
      ),
      quiet = TRUE
    ),
    "refer to the same DSL output"
  )
})

testthat::test_that("Data labels fall back to the calling model's DSL", {
  # Models saved by earlier releases call `label_data_csv(path)` from their own
  # `$fit`/`$sim` methods, with a cached DSL that uses `input_N`/`outeq_N`.
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "ID,EVID,TIME,DUR,DOSE,INPUT,OUT,OUTEQ",
      "1,1,0,0,100,1,.,.",
      "1,0,2,.,.,.,10,1"
    ),
    path
  )
  legacy_dsl <- gsub("cp", "outeq_1", simple_dsl, fixed = TRUE)
  self <- PM_model$from_dsl(
    legacy_dsl,
    pri = list(k = ab(0.01, 2), v = ab(1, 100)),
    err = list(proportional(1, c(0.1, 0, 0, 0), outeq = 1)),
    quiet = TRUE
  )

  label_data_csv(path)

  mapped <- utils::read.csv(path, check.names = FALSE, colClasses = "character")
  expect_identical(mapped$INPUT[1], "input_1")
  expect_identical(mapped$OUTEQ[2], "outeq_1")
})

testthat::test_that("Cycle data is parsed when a declared output has no observations", {
  model <- PM_model$from_dsl(
    "
name = two_outputs
kind = ode
params = k, v
states = central
outputs = 1, 2
dx(central) = infusion(1) - k * central
out(1) = central / v ~ continuous()
out(2) = central ~ continuous()
",
    pri = list(k = ab(0.1, 1), v = ab(5, 20)),
    err = list(
      additive(1, c(0.1, 0, 0, 0), outeq = 1),
      additive(1, c(0.1, 0, 0, 0), outeq = 2)
    ),
    quiet = TRUE
  )
  data <- PM_data$new(
    data.frame(
      id = "1", time = c(0, 1, 2), evid = c(1L, 0L, 0L),
      dose = c(100, NA, NA), dur = c(1, NA, NA), input = c(1, NA, NA),
      out = c(NA, 8, 5), outeq = c(NA, 1, 1)
    ),
    quiet = TRUE
  )
  run_root <- withr::local_tempdir(pattern = "sparse-outputs-")
  fitted <- model$fit(
    data = data, path = run_root, run = 1, cycles = 3, points = 20,
    overwrite = TRUE, report = "none", quiet = TRUE
  )

  gamlam <- fitted$cycle$data$gamlam
  expect_s3_class(fitted$cycle, "PM_cycle")
  expect_identical(unique(gamlam$outeq), 1)
  expect_equal(sort(gamlam$cycle), seq_len(max(gamlam$cycle)))
})
