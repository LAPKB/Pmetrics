# generate data
library(Pmetrics)
PM_result <- NPex

PMmodel <- NPex$model$arg_list

# =================================
# Test Helper
# ================================
testthat::test_that("getODEfromLib returns adequate values", {
  ode <- getODEfromLib("two_comp_bolus")

  expect_type(ode, "list")
  expect_equal(length(ode), 2)
  expect_equal(names(ode), c("dx1", "dx2"))
  expect_equal(ode$dx1, "dx[1] = b[1] - ka * x[1]")
  expect_equal(ode$dx2, "dx[2] = r[1] + ka * x[1] - ke * x[2]")
})



# =================================
# Test extractPMPrimary
# ================================

testthat::test_that("extractPMPrimary", {
  pri <- extractPMPrimary(PMmodel)

  expect_type(pri, "list")
  expect_equal(length(pri), 4)
  expect_equal(names(pri), c("ka", "ke", "v", "tlag1"))
  expect_equal(names(pri$ka), c("type", "min", "max"))
})


# =================================
# Test extractPMCovariate
# ================================
testthat::test_that("extractPMCovariates return adequate values", {
  cov <- extractPMCovariates(PMmodel)

  expect_type(cov, "list")
  expect_equal(length(cov), 5)
  expect_equal(names(cov), c("wt", "africa", "age", "gender", "height"))
  expect_equal(cov$wt$interp, "linear")
  expect_equal(cov$gender$interp, "linear")
})

testthat::test_that("extractPMCovariates handles NULL", {
  cov <- NULL

  expect_null(extractPMCovariates(cov))
})


# =================================
# Test extractPMSecondary
# ================================

testthat::test_that("extractPMSecondary return adequate with ode", {
  PMmodel$sec <- function() {
    v = v0 * wt
    ke = ke0 * age
  }

  sec <- extractPMSecondary(PMmodel)

  expect_type(sec, "list")
  expect_equal(length(sec), 2)
  expect_equal(names(sec), c("v", "ke"))
  expect_equal(sec$v, "v = v0 * wt")
  expect_equal(sec$ke, "ke = ke0 * age")
})


testthat::test_that("extractPMSecondary", {
  cov <- extractPMSecondary(PMmodel)

  expect_null(cov)
})



# =================================
# Test extractPMequation
# ================================

testthat::test_that("extractPMequation return adequate with ode", {
  eqn <- extractPMequation(PMmodel)

  expect_type(eqn, "list")
  expect_equal(length(eqn), 2)
  expect_equal(names(eqn), c("dx1", "dx2"))
  expect_equal(eqn$dx1, "dx[1] = b[1] - ka * x[1]")
  expect_equal(eqn$dx2, "dx[2] = rateiv[1] + ka * x[1] - ke * x[2]")
})

testthat::test_that("extractPMequation return adequate values with mod_lib()", {
  PMmodel$eqn <- function() {
    two_comp_bolus
  }

  eqn <- extractPMequation(PMmodel)

  expect_type(eqn, "list")
  expect_equal(length(eqn), 2)
  expect_equal(names(eqn), c("dx1", "dx2"))
  expect_equal(eqn$dx1, "dx[1] = b[1] - ka * x[1]")
  expect_equal(eqn$dx2, "dx[2] = r[1] + ka * x[1] - ke * x[2]")
})



# =================================
# Test extractPMLag
# ================================

testthat::test_that("extractPMLag return adequate with ode", {
  lag <- extractPMLag(PMmodel)

  expect_type(lag, "list")
  expect_equal(length(lag), 1)
  expect_equal(names(lag), c("lag1"))
  expect_equal(lag$lag1, "lag[1] = tlag1")
})

testthat::test_that("extractPMLag : NULL lag block", {
  PMmodel$lag <- NULL
  lag <- extractPMLag(PMmodel)

  expect_null(lag)
})

# =================================
# Test extractPMFa
# ================================

testthat::test_that("extractPMFa handle fa function", {
  PMmodel$fa <- function() {
    fa[1] = 0.8
  }

  fa <- extractPMFa(PMmodel)

  expect_type(fa, "list")
  expect_equal(length(fa), 1)
  expect_equal(names(fa), c("fa1"))
  expect_equal(fa$fa1, "fa[1] = 0.8")
})


testthat::test_that("extractPMFa handle NULL", {
  fa <- extractPMFa(PMmodel)

  expect_null(fa)
})

# =================================
# Test extractPMInitialVal
# ================================

testthat::test_that("extractPMInitialVal handle NULL", {
  iv <- extractPMInitialVal(PMmodel)

  expect_null(iv)
})

testthat::test_that("extractPMInitialVal handle NULL", {
  PMmodel$ini <- function() {
    X[1] = 10
    X[2] = 10 * v
  }

  iv <- extractPMInitialVal(PMmodel)

  expect_equal(length(iv), 2)
  expect_equal(names(iv), c("X1", "X2"))
})


# =================================
# Test extractPMOuteq
# =================================

testthat::test_that("extractPMOuteq return adequate values", {
  outeq <- extractPMOuteq(PMmodel)

  expect_type(outeq, "list")
  expect_equal(length(outeq), 1)
  expect_equal(names(outeq), "y1")
  expect_equal(outeq$y1, "y[1] = x[2]/v")
})


testthat::test_that("extractPMOuteq handles NULL", {
  PMmodel$out <- NULL
  expect_error(
    extractPMOuteq(PMmodel),
    "The output block is NULL. Please check your PM model."
  )
})

# =================================
# Test extractPMError
# =================================

testthat::test_that("extractPMError return adequate values", {
  err <- extractPMError(PMmodel)

  expect_type(err, "list")  
  expect_equal(length(err), 3)
  expect_equal(names(err), c("type", "initial_value", "coefficient"))
  expect_equal(err$type, "proportional")
  expect_equal(err$initial_value, 5)
  expect_equal(err$coefficient, data.frame(c0 = 0.02, c1 = 0.05, c2 = -0.0002, c3 = 0.0))
})

testthat::test_that("extractPMError handles NULL", {
  PMmodel$err <- NULL
  expect_error(
    extractPMError(PMmodel),
    "The error block is NULL. Please check your PM model."
  )
})

# =================================
# Test createBDdescription
# =================================

testthat::test_that("createBDdescription return adequate values if cov is NULL", {
  description <- createBDdescription(PM_result)
  expected_desc_names <- c("drug", "route", "name", "pmx_path","version", "compartments", "target", "target_unit",  "dose_unit", "description", "reference", "covariates")
  expect_type(description, "list")  
  expect_equal(length(description), 12)
  expect_equal(names(description), expected_desc_names)
  expect_equal(description$drug, "Drug")
  expect_equal(description$route, data.frame(route = "IV", compartment = 1))
  expect_equal(description$name, "Drug.json")
  expect_null(description$pmx_path)
  expect_equal(description$version, 1.0)
  expect_equal(description$compartments, 1) 
  expect_equal(description$target, "concentration")
  expect_equal(description$target_unit, "mg/L")
  expect_equal(description$dose_unit, "mg")
  expect_equal(description$description, "This is a model imported from a PM model object.")
  expect_equal(description$reference, "This model was imported from a PM model object.")
  expect_null(description$covariates)
})

testthat::test_that("createBDdescription return adequate values if any cov is present", {
  ode <- extractPMequation(PMmodel)
  covariates <- list(
    wt = list(interp = "linear"),
    africa = list(interp = "none"),
    age = list(interp = "linear")
  )
  
  description <- createBDdescription(PM_result, covariates = covariates, ode = ode)
  
  expected_desc_names <- c("drug", "route", "name", "pmx_path","version", "compartments", "target", "target_unit",  "dose_unit", "description", "reference", "covariates")
  expect_type(description, "list")  
  expect_equal(length(description), 12)
  expect_equal(names(description), expected_desc_names)
  expect_equal(description$drug, "Drug")
  expect_equal(description$route, data.frame(route = "IV", compartment = 1))
  expect_equal(description$name, "Drug.json")
  expect_null(description$pmx_path)
  expect_equal(description$version, 1.0)
  expect_equal(description$compartments, 2) # should be 2 here but too many manipulation to get the right value
  expect_equal(description$target, "concentration")
  expect_equal(description$target_unit, "mg/L")
  expect_equal(description$dose_unit, "mg")
  expect_equal(description$description, "This is a model imported from a PM model object.")
  expect_equal(description$reference, "This model was imported from a PM model object.")

  # covariate checks
  expect_type(description$covariates, "list")
  expect_equal(description$covariates$number, 3)
  expect_equal(description$covariates$names, c("wt", "africa", "age"))
  expect_equal(description$covariates$label, c("wt", "africa", "age"))
  expect_equal(description$covariates$units, c("to update", "to update", "to update"))
  expect_equal(description$covariates$type, c("numeric", "numeric", "numeric"))
  
  # cov value
  expect_type(description$covariates$value, "list")
  expect_equal(length(description$covariates$value), 3)
  expect_equal(names(description$covariates$value), c("wt", "africa", "age"))
  expect_equal(description$covariates$value$wt, data.frame(min = 0, max = 200, default = 70))
  expect_equal(description$covariates$value$africa, data.frame(min = 0, max = 200, default = 70))
  expect_equal(description$covariates$value$age, data.frame(min = 0, max = 200, default = 70))

  # cov description
  expect_type(description$covariates$description, "list")
  expect_equal(length(description$covariates$description), 3)
  expect_equal(names(description$covariates$description), c("wt", "africa", "age"))
  expect_equal(description$covariates$description$wt, "Pls update the description as needed")
  expect_equal(description$covariates$description$africa, "Pls update the description as needed")
  expect_equal(description$covariates$description$age, "Pls update the description as needed")
})



# =================================
# Test createBDmodel
# =================================

testthat::test_that("createBDmodel return adequate values", {
  ode <- extractPMequation(PMmodel)
  bd_mod <- createBDmodel(PM_result)

  expect_type(bd_mod, "list")  
  expect_equal(length(bd_mod), 3)
  expect_equal(names(bd_mod), c("description", "model", "support_point"))
  
  # check description
  expect_type(bd_mod$description, "list")
  expect_equal(length(bd_mod$description), 12)
  expect_equal(names(bd_mod$description), c("drug", "route", "name", "pmx_path","version", "compartments", "target", "target_unit",  "dose_unit", "description", "reference", "covariates"))
  expect_equal(bd_mod$description$drug, "Drug")
  expect_equal(bd_mod$description$route, data.frame(route = "IV", compartment = 1))
  expect_equal(bd_mod$description$name, "Drug.json")
  expect_null(bd_mod$description$pmx_path)
  expect_equal(bd_mod$description$version, 1.0)
  expect_equal(bd_mod$description$compartments, 2) # should be
  expect_equal(bd_mod$description$target, "concentration")
  expect_equal(bd_mod$description$target_unit, "mg/L")
  expect_equal(bd_mod$description$dose_unit, "mg")
  expect_equal(bd_mod$description$description, "This is a model imported from a PM model object.")
  expect_equal(bd_mod$description$reference, "This model was imported from a PM model object.")
  expect_type(bd_mod$description$covariates, "list")

  # check model
  expect_type(bd_mod$model, "list")
  expect_equal(length(bd_mod$model), 9)
  expect_equal(names(bd_mod$model), c("primary", "covariates", "secondary", "initial_conditions", "fa", "lag", "equation", "out", "error"))

  # check support points
  expect_type(bd_mod$support_point, "list")
  expect_equal(ncol(bd_mod$support_point), 5)
  expect_equal(nrow(bd_mod$support_point), 19)
  expect_equal(names(bd_mod$support_point), c("ka", "ke", "v", "tlag1", "prob"))
})
