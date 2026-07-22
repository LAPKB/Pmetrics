simple_dsl <- "
name = simple_iv
kind = ode
params = k, v
states = central
outputs = cp
infusion(input_1) -> central
dx(central) = rate(input_1) - k * central
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
  expect_identical(model$model_list$route_inputs$infusion, 1L)
  expect_equal(model$model_list$err[[1]]$outeq, 1)
  expect_true(isTRUE(model$model_list$from_dsl))
})

testthat::test_that("PM_model$from_dsl maps named routes and outputs", {
  dsl <- "
name = named_routes
kind = ode
params = k
states = central
outputs = cp
bolus(oral) -> central
infusion(iv) -> central
dx(central) = -k * central
out(cp) = central ~ continuous()
"

  model <- PM_model$from_dsl(
    dsl,
    pri = list(k = ab(0.01, 2)),
    err = list(cp = additive(1, c(1, 0, 0, 0))),
    input_map = c(oral = 1L, iv = 2L),
    quiet = TRUE
  )

  expect_identical(model$arg_list$input_map, c(oral = 1L, iv = 2L))
  expect_identical(model$model_list$route_inputs$bolus, 1L)
  expect_identical(model$model_list$route_inputs$infusion, 2L)
  expect_true(any(vapply(model$input_remap, function(x) identical(x$to, "oral"), logical(1))))
  expect_true(any(vapply(model$input_remap, function(x) identical(x$to, "iv"), logical(1))))
  expect_true(any(vapply(model$input_remap, function(x) identical(x$to, "cp"), logical(1))))

  data <- PM_data$new(
    data.frame(
      id = c("1", "1"), time = c(0, 1), evid = c(1L, 0L),
      dose = c(100, NA), dur = c(0, NA), input = c(1L, NA),
      out = c(NA, 80), outeq = c(NA, 1L)
    ),
    quiet = TRUE
  )
  simulated <- model$sim(data, matrix(0.2, nrow = 1), quiet = TRUE)
  expect_equal(nrow(simulated), 1)
  expect_equal(simulated$outeq, 1)

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

  named_route_dsl <- gsub("input_1", "iv", simple_dsl, fixed = TRUE)
  expect_error(
    PM_model$from_dsl(
      named_route_dsl,
      pri = list(k = ab(0.01, 2), v = ab(1, 100)),
      err = list(cp = proportional(1, c(0.1, 0, 0, 0))),
      quiet = TRUE
    ),
    "input_map.*required"
  )

  mixed_route_dsl <- sub(
    "infusion\\(input_1\\) -> central",
    "bolus(input_1) -> central\ninfusion(iv) -> central",
    simple_dsl
  )
  mixed_route_dsl <- sub("rate\\(input_1\\)", "rate(iv)", mixed_route_dsl)
  mixed <- PM_model$from_dsl(
    mixed_route_dsl,
    pri = list(k = ab(0.01, 2), v = ab(1, 100)),
    err = list(cp = proportional(1, c(0.1, 0, 0, 0))),
    input_map = c(iv = 2L),
    quiet = TRUE
  )
  expect_identical(mixed$arg_list$input_map, c(input_1 = 1L, iv = 2L))
})

testthat::test_that("DSL data mappings rewrite route and output labels", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "ID,EVID,TIME,DUR,DOSE,INPUT,OUT,OUTEQ",
      "1,1,0,0,100,1,.,.",
      "1,1,1,2,100,2,.,.",
      "1,0,2,.,.,.,10,1"
    ),
    path
  )

  remap_input_csv(path, list(
    list(kind = "bolus", from = 1L, to = "oral"),
    list(kind = "infusion", from = 2L, to = "iv"),
    list(kind = "output", from = 1L, to = "cp")
  ))

  mapped <- utils::read.csv(path, check.names = FALSE, colClasses = "character")
  expect_identical(mapped$INPUT[1], "oral")
  expect_identical(mapped$INPUT[2], "iv")
  expect_identical(mapped$OUTEQ[3], "cp")
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
