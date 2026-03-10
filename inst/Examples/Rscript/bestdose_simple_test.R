devtools::load_all()

mod_onecomp <- PM_model$new(
    pri = list(
        ke = ab(0.001, 3.0),
        v = ab(25.0, 250.0)
    ),
    eqn = function() {
        dx[1] <- -ke * X[1] + B[1]
    },
    out = function() {
        Y[1] <- X[1] / v
    },
    err = list(
        additive(1, c(0, 0.20, 0, 0))
    )
)


past_file <- "inst/Examples/src/bestdose_past.csv"
target_file <- "inst/Examples/src/bestdose_target.csv"
prior_file <- "inst/Examples/src/bestdose_prior.csv"


# ===== Two-stage API =====
# Step 1: Compute the posterior once (expensive step)
posterior <- bd_post$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = PM_data$new(past_file, quiet = TRUE),
    max_cycles = 500
)

cat("\nPosterior support points:\n")
print(head(posterior$theta))

# Step 2: Reuse the posterior for different bias weights (cheap)
bias_weights <- seq(0, 1, by = 0.25)
results <- lapply(bias_weights, function(lambda) {
    posterior$optimize(
        target = target_file,
        dose_range = list(min = 0, max = 300),
        bias_weight = lambda, # ought to be called "prior_weight"
        target_type = "concentration"
    )
})
# bd_post$optimize(...) returns a bd object too.
for (i in seq_along(results)) {
    cat("\n=== Bias weight:", bias_weights[i], "===\n")
    results[[i]]$print()
}


# ===== One-shot API =====
bd1 <- bd$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = PM_data$new(past_file, quiet = TRUE),
    max_cycles = 500,
    future = PM_data$new(target_file, quiet = TRUE),
    dose_range = list(min = 0, max = 5000),
    bias_weight = 0,
    target_type = "concentration",
    time_offset = 0
)

bd1


bd2 <- bd$new(
    prior = prior_file,
    model = mod_onecomp,
    max_cycles = 500,
    target = target_file,
    dose_range = list(min = 0, max = 300),
    bias_weight = 0.5,
    target_type = "concentration"
)

bd2


bd1$plot()

bd2$plot()

plot(bd1)


## adding ability to specify future as argument to bd$new() instead of target file. This will allow for more flexible future specifications and avoid the need for a separate target file.

future_list <- list(dose = 0, frequency = 12, route = 0, number = 3, target_time = 11.5, target = 0.3)

bd_new <- bd$new(
  prior = prior_file,
  model = mod_onecomp,
  past_data = PM_data$new(past_file, quiet = TRUE),
  max_cycles = 50,
  future = future_list,
  dose_range = list(min = 0, max = 5000),
  bias_weight = 0,
  target_type = "concentration",
  time_offset = 0
)

bd_new$plot()
