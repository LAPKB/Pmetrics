testthat::skip_on_cran()

# Derived values - the `sec` block, plus any scalar assigned in the equation or
# output block - live in the runtime's f64 buffer. A derived value whose
# initialiser folds to a whole number used to be typed `Int` by the DSL front
# end, so the backend wrote a double into the slot and read it back as an
# integer: the value came out as its own f64 bit pattern (4.607182e18 for 1.0)
# rather than as the value. Outputs, conditionals and derivatives built from it
# were then garbage, which is what drove a reported model to 10^19.
#
# Fractional initialisers (0.5, 0.001) were always typed real and are unaffected.
# These tests pin the numbers the compiled DSL produces, so a backend that
# narrows derived values to integers again cannot pass silently.

# A dose at t = 0 plus one observation per requested time; the dose is routed to
# a state that no equation reads, so it cannot influence the values under test.
probe_data <- function(times = c(1, 2)) {
  data <- PM_data$new(quiet = TRUE)
  data <- data$addEvent(id = 1, time = 0, dose = 100, quiet = TRUE)
  for (time in times) {
    data <- data$addEvent(id = 1, time = time, out = -1, validate = TRUE, quiet = TRUE)
  }
  data
}

# The simulator returns one row per observation and state; the output value is
# the same for each state row, so keep the first row for each requested time.
simulate_probe <- function(model, times = c(1, 2)) {
  result <- model$sim(data = probe_data(times), theta = matrix(0.5, nrow = 1), quiet = TRUE)
  result$out[match(times, result$time)]
}

# The value of 1.0 is 0x3FF0000000000000 read as an integer.
whole_number_bit_pattern <- 4607182418800017408

testthat::test_that("a whole-number derived value read by an output keeps its value", {
  model <- PM_model$new(
    pri = list(k = ab(0.1, 1)),
    sec = function() {
      z1 <- 1.0
    },
    ini = function() {
      x[1] <- 2.0
      x[2] <- 0.0
    },
    eqn = function() {
      dx[1] <- 0.0
      dx[2] <- b[1]
    },
    out = function() {
      y[1] <- z1
    },
    err = list(additive(1, c(0, 0.1, 0, 0)))
  )

  out <- simulate_probe(model)
  testthat::expect_false(any(abs(out - whole_number_bit_pattern) < 1), "output is the f64 bit pattern of 1.0")
  testthat::expect_equal(out, c(1, 1), tolerance = 1e-9)
})

testthat::test_that("fractional derived values are read back as reals", {
  model <- PM_model$new(
    pri = list(k = ab(0.1, 1)),
    sec = function() {
      z1 <- 0.5
    },
    ini = function() {
      x[1] <- 2.0
      x[2] <- 0.0
    },
    eqn = function() {
      dx[1] <- 0.0
      dx[2] <- b[1]
    },
    out = function() {
      y[1] <- z1
    },
    err = list(additive(1, c(0, 0.1, 0, 0)))
  )

  testthat::expect_equal(simulate_probe(model), c(0.5, 0.5), tolerance = 1e-9)
})

testthat::test_that("whole-number derived values in a conditional scale a derivative correctly", {
  # The reported shape: whole-number constants feed a conditional whose result
  # drives an ODE, so a bit-pattern read sends the solver to ~10^19.
  model <- PM_model$new(
    pri = list(k = ab(0.1, 1)),
    sec = function() {
      max_r_ch <- 1.0
      min_r_ch <- 0.001
      h_r_ch <- 2.3
      r_ch_e50 <- 10.0^3
      r_ch <- if (x[1] > 0) max_r_ch - (max_r_ch - min_r_ch) / (1.0 + (r_ch_e50 / x[1])^h_r_ch) else max_r_ch
    },
    ini = function() {
      x[1] <- 5.0
      x[2] <- 0.0
      x[3] <- 0.0
    },
    eqn = function() {
      dx[1] <- 0.0
      dx[2] <- r_ch * x[1] - x[2]
      dx[3] <- b[1]
    },
    out = function() {
      y[1] <- x[2]
    },
    err = list(additive(1, c(0, 0.1, 0, 0)))
  )

  times <- c(1, 2)
  out <- simulate_probe(model, times = times)
  # R's own answer for the E50 term at x[1] = 5, so x[2] approaches 5 * r_ch.
  r_ch <- 1.0 - (1.0 - 0.001) / (1.0 + (10.0^3 / 5.0)^2.3)
  testthat::expect_equal(out, 5.0 * r_ch * (1 - exp(-times)), tolerance = 1e-4)
})

testthat::test_that("a whole-number derived constant that is never reassigned stays real", {
  # Reading the same constant from a derivative, where there is no conditional
  # and no output in between.
  model <- PM_model$new(
    pri = list(k = ab(0.1, 1)),
    sec = function() {
      rate_scale <- 1.0
    },
    ini = function() {
      x[1] <- 5.0
      x[2] <- 0.0
      x[3] <- 0.0
    },
    eqn = function() {
      dx[1] <- 0.0
      dx[2] <- rate_scale * x[1] - x[2]
      dx[3] <- b[1]
    },
    out = function() {
      y[1] <- x[2]
    },
    err = list(additive(1, c(0, 0.1, 0, 0)))
  )

  times <- c(1, 2)
  out <- simulate_probe(model, times = times)
  testthat::expect_equal(out, 5.0 * (1 - exp(-times)), tolerance = 1e-4)
})
