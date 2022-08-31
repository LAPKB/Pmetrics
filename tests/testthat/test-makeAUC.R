
test_that("makeAUC PM_sim", {
  sim <- readRDS("simtest.rds")
  auc <- sim$auc()
  summary_auc <- summary(auc$tau)
  expect_equal(as.numeric(summary_auc), c(52.71472, 118.07382, 170.04533, 
                                          192.47084, 241.90776, 575.20889))
})

test_that("makeAUC PM_load",{
  res <- PM_load("preRun")
  auc1 <- res$op$auc()
  summary_auc1 <- summary(auc1$tau)
  expect_equal(as.numeric(summary_auc1), c(118.1156, 273.87325, 354.4164, 
                                           359.709915, 419.0703, 746.6104))
  
  auc2 <- res$post$auc()
  summary_auc2 <- summary(auc2$tau)
  expect_equal(as.numeric(summary_auc2), c(867.824375569, 1390.992604111, 
                                           1906.729297505, 1954.921511965,
                                           2384.402302823, 3542.787775232))
  
  auc3 <- res$pop$auc()
  summary_auc3 <- summary(auc3$tau)
  expect_equal(as.numeric(summary_auc3), c(1409.06867601, 1878.75748050,
                                           1880.13551509, 1832.93723579,
                                           1880.13831216, 1885.53829003))
  
  df1 <- tibble(id = 1, time = 1:24, out = 100 * exp(-0.1 * time))
  df2 <- tibble(id = 2, time = 1:24, out = 75 * exp(-0.07 * time))
  df <- bind_rows(df1, df2)
  
  auc4 <- makeAUC(df, out~time)
  summary_auc4 <- summary(auc4$tau)
  expect_equal(as.numeric(summary_auc4), c(799.633332601, 803.424445598,
                                           807.215558595, 807.215558595,
                                           811.006671592, 814.797784589))
})
