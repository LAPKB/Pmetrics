library(Pmetrics)
library(testthat)

context("cov")

setwd("/Users/Neely/LAPK/PmetricsSource/codetest/Runs")
PMload(1,2)


test_that("plot ok",{
  throws_error(plot(cov.1,Ke~age))
})

test_that("no formula",{
  expect_error(plot(cov.1))
})

test_that("sum mean",{
  expect_equal(sum(summary(cov.1,"mean")),8106.779178)
})

test_that("sum median",{
  expect_equal(sum(summary(cov.1,"median")),8099.512549)
})

test_that("PMstep OK",{
  expect_equal(sum(PMstep(cov.1),na.rm=T),0.2718037169)
})

test_that("PMstep no cov",{
  throws_error(PMstep(cov.2),"There are no covariates in the data.")
})