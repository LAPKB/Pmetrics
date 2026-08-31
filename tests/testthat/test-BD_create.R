# Checks for BD_create.R — run with testthat after sourcing the exporter:
#   source("docs/BD_create.R"); testthat::test_file("docs/test-BD_create.R")

# generate data
library(Pmetrics)
library(testthat)
PM_result <- NPex

PMmodel <- NPex$model$arg_list

# =================================
# Test Helper
# ================================

test_that("bdKey strips brackets and lowercases", {
  expect_equal(
    bdKey(c("dx[1]", "Y[2]", "Ke", "lag[10]")),
    c("dx1", "y2", "ke", "lag10")
  )
})

test_that("dropNulls removes the absent blocks", {
  expect_equal(dropNulls(list(a = 1, b = NULL, c = "x")), list(a = 1, c = "x"))
})

test_that("deparseLines keeps one statement per line whatever its length", {
  block <- function() {
    dx[1] <- r[1] -
      ke * x[1] -
      kcp * x[1] +
      kpc * x[2] -
      kcp2 * x[1] +
      kpc2 * x[3] -
      kcp3 * x[1]
    dx[2] <- kcp * x[1] - kpc * x[2]
  }

  lines <- deparseLines(block)

  expect_length(lines, 2)
  expect_equal(
    lines[1],
    "dx[1] <- r[1] - ke * x[1] - kcp * x[1] + kpc * x[2] - kcp2 * x[1] + kpc2 * x[3] - kcp3 * x[1]"
  )
  expect_equal(lines[2], "dx[2] <- kcp * x[1] - kpc * x[2]")
})


test_that("extractPMBlock reads any indexed block and handles NULL", {
  lag <- extractPMBlock(PMmodel$lag, "lag")

  expect_type(lag, "list")
  expect_equal(names(lag), "lag1")
  expect_equal(lag$lag1, "lag[1] = tlag1")

  expect_null(extractPMBlock(NULL, "fa"))
  expect_null(extractPMBlock(
    function() {
      x <- 1
    },
    "fa"
  ))
})

# =================================
# Test extractPMPrimary
# ================================

test_that("extractPMPrimary", {
  pri <- extractPMPrimary(PMmodel)

  expect_type(pri, "list")
  expect_equal(length(pri), 4)
  expect_equal(names(pri), c("ka", "ke", "v", "tlag1"))
  expect_equal(names(pri$ka), c("type", "min", "max"))
  expect_equal(pri$ka$type, "ab")
  expect_equal(pri$ka$min, 0.1)
  expect_equal(pri$ka$max, 0.9)
})

# =================================
# Test extractPMCovariate
# ================================

test_that("bdCovariateName maps the aliases BestDose rejects", {
  expect_equal(
    bdCovariateName(c("wt", "GFR", "gender", "age")),
    c("weight", "crcl", "sex", "age")
  )
})

test_that("extractPMCovariates return adequate values", {
  cov <- extractPMCovariates(PMmodel)

  expect_type(cov, "list")
  expect_equal(length(cov), 5)
  # wt and gender are aliases, renamed to the canonical BestDose names
  expect_equal(names(cov), c("weight", "africa", "age", "sex", "height"))
  expect_equal(cov$weight$interp, "linear")
  expect_equal(cov$sex$interp, "linear")
})

test_that("extractPMCovariates handles NULL", {
  cov <- NULL

  expect_null(extractPMCovariates(cov))
})

# =================================
# Test extractPMSecondary
# ================================

test_that("extractPMSecondary return adequate with ode", {
  pm <- PMmodel
  pm$sec <- function() {
    v <- v0 * wt
    ke <- ke0 * age
  }

  sec <- extractPMSecondary(pm)

  expect_type(sec, "list")
  expect_equal(length(sec), 2)
  expect_equal(names(sec), c("v", "ke"))
  expect_equal(sec$v, "v <- v0 * wt")
  expect_equal(sec$ke, "ke <- ke0 * age")
})

test_that("extractPMSecondary", {
  cov <- extractPMSecondary(PMmodel)

  expect_null(cov)
})


# =================================
# Test extractPMBlock on fa / ini / out
# =================================

test_that("extractPMBlock handle fa function", {
  pm <- PMmodel
  pm$fa <- function() {
    fa[1] <- 0.8
  }

  fa <- extractPMBlock(pm$fa, "fa")

  expect_type(fa, "list")
  expect_equal(length(fa), 1)
  expect_equal(names(fa), c("fa1"))
  expect_equal(fa$fa1, "fa[1] = 0.8")
})

test_that("extractPMBlock handle NULL fa and ini", {
  expect_null(extractPMBlock(PMmodel$fa, "fa"))
  expect_null(extractPMBlock(PMmodel$ini, "ini"))
})

test_that("extractPMBlock return adequate output equations", {
  outeq <- extractPMBlock(PMmodel$out, "y")

  expect_type(outeq, "list")
  expect_equal(length(outeq), 1)
  expect_equal(names(outeq), "y1")
  expect_equal(outeq$y1, "y[1] = x[2]/v")
})

# =================================
# Test extractPMError
# =================================

test_that("extractPMError return adequate values", {
  err <- extractPMError(PMmodel)

  expect_type(err, "list")
  expect_equal(length(err), 3)
  expect_equal(names(err), c("type", "initial_value", "coefficient"))
  expect_equal(err$type, "proportional")
  expect_equal(err$initial_value, 5)
  expect_equal(
    err$coefficient,
    data.frame(c0 = 0.0, c1 = 0.1, c2 = 0.0, c3 = 0.0)
  )
})

test_that("extractPMError translates the Pmetrics L/G types", {
  pm <- PMmodel
  pm$err <- list(list(type = "L", initial = 1, coeff = c(0.1, 0.2, 0, 0)))
  expect_equal(extractPMError(pm)$type, "additive")

  pm$err <- list(list(
    type = "unknown",
    initial = 1,
    coeff = c(0.1, 0.2, 0, 0)
  ))
  expect_error(extractPMError(pm), "Unsupported error model type")
})

# =================================
# Test extractBDroutes
# =================================

test_that("extractBDroutes falls back to IV in compartment 1", {
  routes <- extractBDroutes(list(dx1 = "dx[1] = -ke * x[1]"))

  expect_equal(nrow(routes), 1)
  expect_equal(routes$route, "IV")
  expect_equal(routes$compartment, 1L)
})

# =================================
# Test createBDcompartment
# =================================

test_that("createBDcompartment names the outputs and drops the absent peripheral", {
  one <- createBDcompartment(list(y1 = "y[1] = x[1]/v"))

  expect_equal(names(one), c("number", "central", "outputs"))
  expect_equal(one$number, 1)
  expect_equal(one$outputs, data.frame(name = "central", equation = 1L))

  two <- createBDcompartment(list(y1 = "y[1] = x[1]/v", y2 = "y[2] = x[2]/v"))

  expect_equal(names(two), c("number", "central", "peripheral", "outputs"))
  expect_equal(two$peripheral, 2L)
  expect_equal(two$outputs$name, c("central", "peripheral"))
  expect_equal(two$outputs$equation, c(1L, 2L))
})

test_that("createBDcompartment stops without an output block", {
  expect_error(createBDcompartment(NULL), "output block is NULL")
})

# =================================
# Test createBDcovariates
# =================================

test_that("createBDcovariates fills the known ranges and flags the others", {
  covariates <- list(
    weight = list(interp = "linear"),
    africa = list(interp = "none"),
    age = list(interp = "linear")
  )

  cov_desc <- createBDcovariates(covariates)

  expect_type(cov_desc, "list")
  expect_equal(cov_desc$number, 3)
  expect_equal(as.character(cov_desc$names), c("weight", "africa", "age"))
  expect_equal(as.character(cov_desc$label), c("Weight", "africa", "Age"))
  expect_equal(as.character(cov_desc$units), c("kg", "to update", "years"))
  expect_equal(as.character(cov_desc$types), c("numeric", "numeric", "numeric"))

  # cov value
  expect_type(cov_desc$value, "list")
  expect_equal(names(cov_desc$value), c("weight", "africa", "age"))
  expect_equal(
    cov_desc$value$weight,
    data.frame(min = 2, max = 650, default = 70)
  )
  expect_equal(
    cov_desc$value$africa,
    data.frame(min = 0, max = 200, default = 70)
  )
  expect_equal(cov_desc$value$age, data.frame(min = 0, max = 120, default = 50))

  # cov description
  expect_equal(names(cov_desc$description), c("weight", "africa", "age"))
  expect_equal(cov_desc$description$weight, "Patient weight in kilograms")
  expect_equal(
    cov_desc$description$africa,
    "Please update the description as needed"
  )
})

test_that("createBDcovariates returns NULL without covariates", {
  expect_null(createBDcovariates(NULL))
  expect_null(createBDcovariates(list()))
})

# =================================
# Test createBDdescription
# =================================

test_that("createBDdescription return adequate values if cov is NULL", {
  model_list <- list(
    equation = extractPMBlock(PMmodel$eqn, "dx"),
    out = extractPMBlock(PMmodel$out, "y")
  )

  description <- createBDdescription(model_list, "Amikacin", "AmikacinPM")

  expected_desc_names <- c(
    "drug",
    "route",
    "name",
    "compartment",
    "version",
    "description",
    "reference",
    "reference_url"
  )
  expect_type(description, "list")
  expect_equal(names(description), expected_desc_names)
  expect_equal(description$drug, "amikacin")
  expect_equal(
    description$route,
    data.frame(route = c("IV", "PO"), compartment = c(1L, 1L))
  )
  expect_equal(description$name, "AmikacinPM.json")
  expect_equal(description$version, 1L)
  expect_equal(description$compartment$number, 1)
  expect_equal(description$description, "")
  expect_equal(description$reference, "")
  expect_equal(description$reference_url, "")
  expect_null(description$covariates)
})

test_that("createBDdescription return adequate values if any cov is present", {
  model_list <- list(
    covariates = extractPMCovariates(PMmodel),
    equation = extractPMBlock(PMmodel$eqn, "dx"),
    out = extractPMBlock(PMmodel$out, "y")
  )

  description <- createBDdescription(
    model_list,
    "Amikacin",
    "AmikacinPM",
    description = "a model",
    reference = "a citation",
    reference_url = "https://example.org"
  )

  expected_desc_names <- c(
    "drug",
    "route",
    "name",
    "compartment",
    "version",
    "description",
    "reference",
    "reference_url",
    "covariates"
  )
  expect_equal(names(description), expected_desc_names)
  expect_equal(description$description, "a model")
  expect_equal(description$reference, "a citation")
  expect_equal(description$reference_url, "https://example.org")

  # covariate checks
  expect_type(description$covariates, "list")
  expect_equal(description$covariates$number, 5)
  expect_equal(
    as.character(description$covariates$names),
    c("weight", "africa", "age", "sex", "height")
  )
})

# =================================
# Test extractBDsupportPoints
# =================================

test_that("extractBDsupportPoints keeps the primary parameters and prob", {
  points <- extractBDsupportPoints(PM_result, c("ka", "ke", "v", "tlag1"))

  expect_s3_class(points, "data.frame")
  expect_equal(nrow(points), 17)
  expect_equal(names(points), c("ka", "ke", "v", "tlag1", "prob"))
})

test_that("extractBDsupportPoints stops on a missing parameter", {
  expect_error(
    extractBDsupportPoints(PM_result, c("ka", "cl")),
    "missing the primary parameter"
  )
})

# =================================
# Test createBDmodel
# =================================

test_that("createBDmodel return adequate values", {
  bd_mod <- createBDmodel(
    PM_result,
    drug_name = "Amikacin",
    model_name = "AmikacinPM"
  )

  expect_type(bd_mod, "list")
  expect_equal(length(bd_mod), 3)
  expect_equal(names(bd_mod), c("description", "model", "support_point"))

  # check description
  expect_type(bd_mod$description, "list")
  expect_equal(
    names(bd_mod$description),
    c(
      "drug",
      "route",
      "name",
      "compartment",
      "version",
      "description",
      "reference",
      "reference_url",
      "covariates"
    )
  )
  expect_equal(bd_mod$description$drug, "amikacin")
  expect_equal(bd_mod$description$name, "AmikacinPM.json")
  expect_equal(bd_mod$description$version, 1L)
  expect_equal(bd_mod$description$compartment$number, 1)
  expect_equal(
    bd_mod$description$compartment$outputs,
    data.frame(name = "central", equation = 1L)
  )
  expect_type(bd_mod$description$covariates, "list")

  # check model: the blocks NPex does not define (secondary, initial_conditions, fa) are dropped
  expect_type(bd_mod$model, "list")
  expect_equal(
    names(bd_mod$model),
    c("primary", "covariates", "lag", "equation", "out", "error")
  )

  # check support points
  expect_s3_class(bd_mod$support_point, "data.frame")
  expect_equal(ncol(bd_mod$support_point), 5)
  expect_equal(nrow(bd_mod$support_point), 17)
  expect_equal(names(bd_mod$support_point), c("ka", "ke", "v", "tlag1", "prob"))
})

# =================================
# Test checkBDmodel
# =================================

test_that("checkBDmodel accepts a model built by createBDmodel", {
  bd_mod <- createBDmodel(
    PM_result,
    drug_name = "Amikacin",
    model_name = "AmikacinPM"
  )

  expect_true(checkBDmodel(bd_mod))
})

test_that("checkBDmodel catches what BestDose would refuse", {
  bd_mod <- createBDmodel(
    PM_result,
    drug_name = "Amikacin",
    model_name = "AmikacinPM"
  )

  broken <- bd_mod
  broken$model$primary <- broken$model$primary["ka"]
  expect_error(checkBDmodel(broken), "At least two primary parameters")

  broken <- bd_mod
  broken$description$drug <- ""
  expect_error(checkBDmodel(broken), "A drug must be assigned")

  broken <- bd_mod
  broken$description$compartment$number <- 3
  expect_error(
    checkBDmodel(broken),
    "does not match the number of named compartments"
  )

  broken <- bd_mod
  broken$model$covariates$weight <- NULL
  expect_error(
    checkBDmodel(broken),
    "covariates in the description and in the model block do not match"
  )
})