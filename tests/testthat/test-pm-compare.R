test_that("PM_compare uses the selected output equation and central tendency", {
  testthat::local_mocked_bindings(
    getPMoptions = function(opt) {
      switch(opt, bias_method = "mae", imp_method = "mse",
             ic_method = "aic", digits = 3)
    },
    .package = "Pmetrics"
  )

  op <- tidyr::expand_grid(
    outeq = 1:2, icen = c("median", "mean"),
    pred.type = c("pop", "post"), time = 1:5
  ) |>
    dplyr::mutate(
      pred = time,
      obs = outeq + (outeq + (icen == "mean") + (pred.type == "post")) * pred +
        c(0.2, -0.1, 0.3, -0.2, 0.1)[time],
      obsSD = 1,
      d = pred - obs, wd = d, ds = d^2, wds = ds
    )
  class(op) <- c("PM_op_data", class(op))

  make_result <- function(parameter) {
    structure(list(
      op = list(data = op),
      cycle = list(data = list(
        objective = data.frame(cycle = 1L, neg2ll = 100, aic = 102, bic = 103),
        status = "Converged"
      )),
      final = list(
        popMean = stats::setNames(1, parameter),
        data = list(popPoints = stats::setNames(data.frame(1:5), parameter))
      )
    ), class = "PM_result")
  }
  # Distinct parameter names avoid an unrelated nearest-neighbor comparison.
  run1 <- make_result("a")
  run2 <- make_result("b")

  for (eq in 1:2) {
    for (center in c("median", "mean")) {
      result <- PM_compare(run1, run2, outeq = eq, icen = center)
      for (type in c("pop", "post")) {
        selected <- op[op$outeq == eq & op$icen == center & op$pred.type == type, ]
        fit <- stats::lm(obs ~ pred, data = selected)
        expected <- c(
          Int = unname(stats::coef(fit)[1]),
          Sl = unname(stats::coef(fit)[2]),
          R2 = summary(fit)$r.squared,
          Bias = mean(selected$d),
          Imp = mean(selected$ds)
        )
        expected[c("Bias", "Imp")] <- round(expected[c("Bias", "Imp")], 3)
        for (metric in names(expected)) {
          expect_equal(
            unname(result[[paste0(type, metric)]]),
            rep(expected[[metric]], 2),
            tolerance = 1e-10,
            info = paste("outeq", eq, "icen", center, type, metric)
          )
        }
      }
      expect_equal(result[["-2*LL"]], c(100, 100))
      expect_equal(result$aic, c(102, 102))
    }
  }
})
