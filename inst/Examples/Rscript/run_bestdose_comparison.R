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
        additive(0, c(0, 0.20, 0, 0))
    )
)

past_file <- "inst/Examples/src/bestdose_past.csv"
target_file <- "inst/Examples/src/bestdose_target_pmcore.csv"
prior_file <- "inst/Examples/src/bestdose_prior_pmcore.csv"

cat("Using PMcore prior file\n")

posterior <- bd_post$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = PM_data$new(past_file, quiet = TRUE),
    max_cycles = 500,
    quiet = TRUE
)

cat("\nPosterior theta (first 5 rows):\n")
print(head(posterior$theta, 5))
cat("\nPosterior weights (first 10):\n")
print(head(posterior$posterior_weights, 10))

bias_weights <- seq(0, 1, by = 0.1)
for (lambda in bias_weights) {
    r <- posterior$optimize(
        target = target_file,
        dose_range = list(min = 0, max = 300),
        bias_weight = lambda,
        target_type = "concentration"
    )
    cat(sprintf(
        "Bias weight: %.2f\t\tOptimal dose: [%.4f, %.4f]\t\tCost: %.6f\t\tln Cost: %.4f\t\tMethod: %s\n",
        lambda,
        r$doses[1], r$doses[2],
        r$objf,
        log(r$objf),
        r$method
    ))
}

# Last result predictions
r <- posterior$optimize(
    target = target_file,
    dose_range = list(min = 0, max = 300),
    bias_weight = 1.0,
    target_type = "concentration"
)
cat("\nConcentration-time predictions for bias_weight=1.0:\n")
preds <- r$result$predictions
for (j in seq_len(nrow(preds))) {
    p <- preds[j, ]
    cat(sprintf(
        "Time: %.2f h, Observed: %.2f, (Pop Mean: %.4f, Pop Median: %.4f, Post Mean: %.4f, Post Median: %.4f)\n",
        p$time, p$obs, p$pop_mean, p$pop_median, p$post_mean, p$post_median
    ))
}
