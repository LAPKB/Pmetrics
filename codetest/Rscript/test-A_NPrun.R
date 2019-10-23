library(Pmetrics)
library(testthat)

context("NPrun")

setwd("/Users/Neely/LAPK/PmetricsSource/codetest/Runs")
unlink("?",recursive=T)


test_that("Linear, 5 cov, 1 in, 1 out",{
  file.copy(from=c("../src/model1.txt","../src/ex1.csv"),to=getwd(),overwrite=T)
  NPrun(data="ex1.csv",model="model1.txt",cycles=10,intern=T)
  expect_true(file.exists("1/outputs/NPAGout.Rdata"))
})

test_that("Linear, 0 cov, 1 in, 1 out",{
  file.copy(from=c("../src/model2.txt","../src/ex2.csv"),to=getwd(),overwrite=T)
  NPrun(data="ex2.csv",model="model2.txt",cycles=10,intern=T)
  expect_true(file.exists("2/outputs/NPAGout.Rdata"))
})
