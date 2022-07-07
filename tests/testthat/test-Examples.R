test_that("Data object creation", {
  exData <- PM_data$new(data = "ex.csv")
  summary<-exData$summary()
  expect_equal(summary$nsub, 20)
  expect_equal(summary$ndrug, 1)
  expect_equal(summary$numeqt, 1)
  expect_equal(summary$nobsXouteq[[1]],139)
  expect_equal(summary$covnames, c("wt","africa","age","gender","height"))
})


test_that("PMdata print",{
  exData <- PM_data$new(data = "ex.csv")
  expect_output(exData$print(viewer = F),"2    1    1  24.00   0  600   NA NA     1    NA    NA   NA     NA     NA NA")
  expect_output(exData$print(viewer = F),"259 59.0      1  31      1    170")
})

test_that("Model object creation",{
  mod1 <- PM_model$new(list(
  pri = list(
    Ka = range(0.1, 0.9),
    Ke = range(0.001, 0.1),
    V = range(30, 120),
    Tlag1 = range(0, 4)
  ),
  cov = list("WT", "AFRICA", "AGE", "GENDER", "HEIGHT"),
  lag = list("Tlag(1) = Tlag1"),
  out = list(
    Y1 = list(
      value = "X(2)/V",
      err = list(
        model = proportional(5),
        assay = c(0.02, 0.05, -0.0002, 0)
      )
    )
  )
))
expect_equal(mod1$model_list$pri$Ka, range(0.1,0.9))
expect_equal(mod1$model_list$pri$Ka$max, 0.9)
expect_equal(mod1$model_list$pri$Ka$min, 0.1)
expect_equal(mod1$model_list$pri$Ka$mean, 0.5)
expect_equal(mod1$model_list$pri$Ka$sd, 0.133)
expect_equal(mod1$model_list$pri$Ka$mode, "range")
expect_equal(mod1$model_list$pri$Ka$gtz, F)
expect_equal(range(0.1,0.9,gtz=T)$gtz,T)
expect_equal(mod1$model_list$cov, list("WT", "AFRICA", "AGE", "GENDER", "HEIGHT"))
expect_equal(mod1$model_list$lag, list("Tlag(1) = Tlag1"))
expect_equal(names(mod1$model_list$out), "Y1")
expect_equal(mod1$model_list$out$Y1$value, "X(2)/V")
expect_equal(mod1$model_list$out$Y1$err$model, proportional(5))
expect_equal(mod1$model_list$out$Y1$err$model$proportional, 5)
expect_equal(mod1$model_list$out$Y1$err$assay, c(0.02, 0.05, -0.0002, 0))

})

test_that("Object representation",{
  mod1 <- PM_model$new(list(
  pri = list(
    Ka = range(0.1, 0.9),
    Ke = range(0.001, 0.1),
    V = range(30, 120),
    Tlag1 = range(0, 4)
  ),
  cov = list("WT", "AFRICA", "AGE", "GENDER", "HEIGHT"),
  lag = list("Tlag(1) = Tlag1"),
  out = list(
    Y1 = list(
      value = "X(2)/V",
      err = list(
        model = proportional(5),
        assay = c(0.02, 0.05, -0.0002, 0)
      )
    )
  )
))
mod1$write("mod1.txt")
expect_equal(readLines("mod1.txt"),readLines("generated_model.txt"))
})

test_that("Object update",{
  mod1 <- PM_model$new(list(
  pri = list(
    Ka = range(0.1, 0.9),
    Ke = range(0.001, 0.1),
    V = range(30, 120),
    Tlag1 = range(0, 4)
  ),
  cov = list("WT", "AFRICA", "AGE", "GENDER", "HEIGHT"),
  lag = list("Tlag(1) = Tlag1"),
  out = list(
    Y1 = list(
      value = "X(2)/V",
      err = list(
        model = proportional(5),
        assay = c(0.02, 0.05, -0.0002, 0)
      )
    )
  )
))
mod1$update(list(
  pri = list(
    Ka = range(0.001, 5)
  )
))
expect_equal(mod1$model_list$pri$Ka, range(0.001,5))
expect_equal(mod1$model_list$pri$Ka$max, 5)
expect_equal(mod1$model_list$pri$Ka$min, 0.001)
expect_equal(mod1$model_list$pri$Ka$mean, 2.5)
expect_equal(mod1$model_list$pri$Ka$sd, 0.833)
})


test_that("Fit object creation",{
  mod1 <- PM_model$new(list(
    pri = list(
      Ka = range(0.1, 0.9),
      Ke = range(0.001, 0.1),
      V = range(30, 120),
      Tlag1 = range(0, 4)
    ),
    cov = list("WT", "AFRICA", "AGE", "GENDER", "HEIGHT"),
    lag = list("Tlag(1) = Tlag1"),
    out = list(
      Y1 = list(
        value = "X(2)/V",
        err = list(
          model = proportional(5),
          assay = c(0.02, 0.05, -0.0002, 0)
        )
      )
    )
  ))
  exFit <- PM_fit$new(model = mod1, data = "ex.csv")
  expect_output(exFit$check(),"Excellent - there were no errors found in your model file.")
  expect_output(exFit$check(),"No data errors found.")
})

test_that("Basic model fitting", {
  skip_if(length(list.files("1")) != 0)
  mod1 <- PM_model$new(list(
    pri = list(
      Ka = range(0.1, 0.9),
      Ke = range(0.001, 0.1),
      V = range(30, 120),
      Tlag1 = range(0, 4)
    ),
    cov = list("WT", "AFRICA", "AGE", "GENDER", "HEIGHT"),
    lag = list("Tlag(1) = Tlag1"),
    out = list(
      Y1 = list(
        value = "X(2)/V",
        err = list(
          model = proportional(5),
          assay = c(0.02, 0.05, -0.0002, 0)
        )
      )
    )
  ))
  exFit <- PM_fit$new(model = mod1, data = "ex.csv")
  
  expect_output(exFit$run(intern = T),"The run did not converge before the last cycle.")

})

test_that("Load model",{
  exRes <- PM_load(1)
  expect_equal(exRes$data,PM_data$new(data = "ex.csv"))
  expect_equal(exRes$model$model_list,PM_model$new(list(
    pri = list(
      Ka = range(0.1, 0.9),
      Ke = range(0.001, 0.1),
      V = range(30, 120),
      Tlag1 = range(0, 4)
    ),
    cov = c("WT", "AFRICA", "AGE", "GENDER", "HEIGHT"),
    lag = c("Tlag(1) = Tlag1"),
    out = list(
      Y1 = list(
        value = "X(2)/V",
        err = list(
          model = proportional(5),
          assay = c(0.02, 0.05, -0.0002, 0)
        )
      )
    )
  ))$model_list)
  expect_true({exRes$success})
  expect_true(all(class(exRes$cov) == c("PM_cov", "R6")))
  expect_output(print(exRes$cov$summary()),"20 20   60 59.0      1  31      1    170 0.2160000 0.08366500  34.95000")
  expect_true(all(class(exRes$op) == c("PM_op", "R6")))
  expect_output(print(exRes$op$summary()),"Mean weighed squared prediction error: 0.99")
  expect_true(all(class(exRes$cycle) == c("PM_cycle", "R6")))
  expect_output(print(exRes$cycle$ll),"440.1974")
  # expect_output(print(exRes$cycle$bic),"464.8381")
  # expect_output(print(exRes$cycle$aic),"450.6168")
  # expect_output(print(exRes$cycle$median),"100 1.020956 0.9975805 0.9711862 0.8552348")
  # expect_output(print(exRes$cycle$sd),"100 1.130509 0.9829713 1.0694383 1.011984")
})