library(Pmetrics)
library(testthat)

context("PMcheck")

setwd("/Users/Neely/LAPK/PmetricsSource/codetest/src")
mdata.1 <- PMreadMatrix("ex1.csv") #file with no errors

test_that("data is ok",{
  err <- PMcheck(mdata.1)
  expect_identical(attr(err,"error"),0)
})
