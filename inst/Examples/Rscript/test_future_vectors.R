devtools::load_all(quiet = TRUE)

mod_onecomp <- PM_model$new(
  pri = list(ke = ab(0.001, 3.0), v = ab(25.0, 250.0)),
  eqn = function() { dx[1] <- -ke * X[1] + B[1] },
  out = function() { Y[1] <- X[1] / v },
  err = list(additive(0, c(0, 0.20, 0, 0)))
)

# Test 1: scalar (legacy behavior)
cat("--- Test 1: scalar dose/freq/route, number=3 ---\n")
bd1 <- bd$new(
  prior = "inst/Examples/src/bestdose_prior.csv",
  model = mod_onecomp,
  past_data = PM_data$new("inst/Examples/src/bestdose_past.csv", quiet = TRUE),
  max_cycles = 5,
  future = list(dose = 1000, frequency = 12, route = 0, number = 3, target_time = 11.5, target = 0.3),
  dose_range = list(min = 0, max = 5000), prior_weight = 0, start = "02/02/26 00:00",
  target_type = "concentration", quiet = TRUE
)
cat("doses:", paste(bd1$future$data$dose[!is.na(bd1$future$data$dose)], collapse = ","), "\n")
cat("times:", paste(bd1$future$data$time[!is.na(bd1$future$data$dose)], collapse = ","), "\n\n")

# Test 2: vector dose (3 elements, number=3)
cat("--- Test 2: vector dose c(500,750,1000), number=3 ---\n")
bd2 <- bd$new(
  prior = "inst/Examples/src/bestdose_prior.csv",
  model = mod_onecomp,
  past_data = PM_data$new("inst/Examples/src/bestdose_past.csv", quiet = TRUE),
  max_cycles = 5,
  future = list(dose = c(500, 750, 1000), frequency = 12, route = 0, number = 3, target_time = 11.5, target = 0.3),
  dose_range = list(min = 0, max = 5000), prior_weight = 0, start = "02/02/26 00:00",
  target_type = "concentration", quiet = TRUE
)
cat("doses:", paste(bd2$future$data$dose[!is.na(bd2$future$data$dose)], collapse = ","), "\n")
cat("times:", paste(bd2$future$data$time[!is.na(bd2$future$data$dose)], collapse = ","), "\n\n")

# Test 3: number < vector length - should inform and set number=3
cat("--- Test 3: number=2 < dose length 3 (should inform, expand to 3) ---\n")
bd3 <- bd$new(
  prior = "inst/Examples/src/bestdose_prior.csv",
  model = mod_onecomp,
  past_data = PM_data$new("inst/Examples/src/bestdose_past.csv", quiet = TRUE),
  max_cycles = 5,
  future = list(dose = c(500, 750, 1000), frequency = 12, route = 0, number = 2, target_time = 11.5, target = 0.3),
  dose_range = list(min = 0, max = 5000), prior_weight = 0, start = "02/02/26 00:00",
  target_type = "concentration", quiet = TRUE
)
cat("doses:", paste(bd3$future$data$dose[!is.na(bd3$future$data$dose)], collapse = ","), "\n\n")

# Test 4: short dose vector recycled (dose length 2, number=4 -> last value 1000 recycled)
cat("--- Test 4: dose c(500,1000), number=4 (recycle last -> 500,1000,1000,1000) ---\n")
bd4 <- bd$new(
  prior = "inst/Examples/src/bestdose_prior.csv",
  model = mod_onecomp,
  past_data = PM_data$new("inst/Examples/src/bestdose_past.csv", quiet = TRUE),
  max_cycles = 5,
  future = list(dose = c(500, 1000), frequency = 12, route = 0, number = 4, target_time = 11.5, target = 0.3),
  dose_range = list(min = 0, max = 5000), prior_weight = 0, start = "02/02/26 00:00",
  target_type = "concentration", quiet = TRUE
)
cat("doses:", paste(bd4$future$data$dose[!is.na(bd4$future$data$dose)], collapse = ","), "\n\n")

# Test 5: vector frequency (variable intervals)
cat("--- Test 5: dose=1000, freq=c(8,12,24), number=3 ---\n")
bd5 <- bd$new(
  prior = "inst/Examples/src/bestdose_prior.csv",
  model = mod_onecomp,
  past_data = PM_data$new("inst/Examples/src/bestdose_past.csv", quiet = TRUE),
  max_cycles = 5,
  future = list(dose = 1000, frequency = c(8, 12, 24), route = 0, number = 3, target_time = 7.5, target = 0.3),
  dose_range = list(min = 0, max = 5000), prior_weight = 0, start = "02/02/26 00:00",
  target_type = "concentration", quiet = TRUE
)
cat("doses:", paste(bd5$future$data$dose[!is.na(bd5$future$data$dose)], collapse = ","), "\n")
cat("times:", paste(bd5$future$data$time[!is.na(bd5$future$data$dose)], collapse = ","), "\n\n")

cat("All tests completed.\n")
