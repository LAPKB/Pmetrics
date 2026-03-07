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


# Step 1: Compute the posterior once (expensive step)
posterior <- bd_post$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = PM_data$new(past_file, quiet = TRUE),
    max_cycles = 500
)

cat("\nPosterior support points:\n")
print(head(posterior$theta))

# Step 2: Reuse the posterior for different bias weights
bias_weights <- seq(0, 1, by = 0.25)
results <- lapply(bias_weights, function(lambda) {
    bd$new(
        target = target_file,
        dose_range = list(min = 0, max = 300),
        bias_weight = lambda,
        posterior = posterior,
        target_type = "concentration"
    )
})

for (i in seq_along(results)) {
    cat("\n=== Bias weight:", bias_weights[i], "===\n")
    results[[i]]$print()
}


bd1 <- bd$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = PM_data$new(past_file, quiet = TRUE),
    max_cycles = 500,
    target = target_file,
    dose_range = list(min = 0, max = 5000),
    bias_weight = 0,
    target_type = "concentration",
    time_offset = 0
)

bd1

bd1b <- bd$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = past_file,
    max_cycles = 500,
    target = target_file,
    dose_range = list(min = 0, max = 5000),
    bias_weight = 0,
    target_type = "concentration",
    time_offset = 12
)

bd1b


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


plot(bd1)

plot(bd2)
