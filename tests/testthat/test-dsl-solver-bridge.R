make_solver_bridge_model <- function() {
  PM_model$new(
    pri = list(k = ab(0.1, 1)),
    eqn = function() {
      dX[1] <- R[1] - k * X[1]
    },
    out = function() {
      Y[1] <- X[1]
    },
    err = list(additive(0, c(1, 0, 0, 0))),
    solver = "TSIT45"
  )
}

make_solver_bridge_data <- function() {
  PM_data$new(
    data.frame(
      id = c("1", "1"),
      time = c(0, 1),
      evid = c(1L, 0L),
      dose = c(100, NA_real_),
      dur = c(1, NA_real_),
      input = c(1L, NA_integer_),
      out = c(NA_real_, 10),
      outeq = c(NA_integer_, 1L)
    ),
    quiet = TRUE
  )
}

test_that("fit forwards the ODE solver and numeric settings", {
  model <- make_solver_bridge_model()
  data <- make_solver_bridge_data()
  run_root <- withr::local_tempdir(pattern = "solver-fit-")
  captured <- NULL

  result <- testthat::with_mocked_bindings(
    fit = function(model_source, data, params, output_path, solver = NULL) {
      captured <<- list(solver = solver, params = params)
      dir.create(output_path, recursive = TRUE)
      invisible(NULL)
    },
    PM_parse = function(path) invisible(NULL),
    PM_load = function(path, file, ...) structure(list(path = path), class = c("PM_result", "R6")),
    {
      model$fit(
        data = data,
        path = run_root,
        run = 1,
        cycles = 10L,
        points = 25L,
        seed = 7L,
        overwrite = TRUE,
        report = "none",
        quiet = TRUE
      )
    }
  )

  expect_s3_class(result, "PM_result")
  expect_identical(captured$solver, "tsit45")
  expect_identical(captured$params$max_cycles, 10)
  expect_identical(captured$params$points, 25)
  expect_identical(captured$params$seed, 7)
})

test_that("simulation forwards the solver and returns one-based indices", {
  model <- make_solver_bridge_model()
  data <- make_solver_bridge_data()
  captured_solver <- NULL

  result <- testthat::with_mocked_bindings(
    simulate_all = function(data_path, model_source, theta, solver = NULL) {
      captured_solver <<- solver
      data.frame(
        id = "1",
        time = 1,
        out = 10,
        outeq = 0L,
        state = 10,
        state_index = 0L,
        spp_index = 0L
      )
    },
    {
      model$sim(data, matrix(0.5, nrow = 1), quiet = TRUE)
    }
  )

  expect_identical(captured_solver, "tsit45")
  expect_identical(result$outeq, 1L)
  expect_identical(result$state_index, 1L)
})
