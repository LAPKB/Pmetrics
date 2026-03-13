devtools::load_all(quiet = TRUE)

mod_onecomp <- PM_model$new(
  pri = list(ke = ab(0.001, 3.0), v = ab(25.0, 250.0)),
  eqn = function() { dx[1] <- -ke * X[1] + B[1] },
  out = function() { Y[1] <- X[1] / v },
  err = list(additive(0, c(0, 0.20, 0, 0)))
)

# Mixed: dose 1 = fixed (1000), dose 2 = optimize (0), dose 3 = fixed (1500)
bd_obj <- bd$new(
  prior = "inst/Examples/src/bestdose_prior.csv",
  model = mod_onecomp,
  past_data = PM_data$new("inst/Examples/src/bestdose_past.csv", quiet = TRUE),
  max_cycles = 5,
  future = list(dose = c(1000, 0, 1500), frequency = 12, route = 0, number = 3,
                target_time = 11.5, target = 0.3),
  dose_range = list(min = 0, max = 5000), prior_weight = 0, start = "02/02/26 00:00",
  target_type = "concentration", quiet = TRUE
)

# Debug: check what standard_data looks like for future evid==1 rows
cat("x$future$standard_data evid==1 rows:\n")
sd <- bd_obj$future$standard_data
print(sd[sd$evid == 1, ])

report <- Pmetrics:::bd_report_build(bd_obj)
cat("future_doses status column:\n")
print(report$future_doses[, c("dose", "status")])

out <- file.path(tempdir(), "bd-report-status-test")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
status <- bd_obj$report(path = out, show = FALSE, quiet = TRUE)
cat("\nReport status:", status, "\n")
cat("HTML exists:", file.exists(file.path(out, "bestdose_report.html")), "\n")
