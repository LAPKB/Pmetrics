test_that("makeAUC PM_sim", {
  sim <- readRDS("simtest.rds")
  auc <- sim$auc()
  summary_auc <- summary(auc$tau)
  expect_equal(round(as.numeric(summary_auc), 2), c(
    52.71, 118.07, 170.05,
    192.47, 241.91, 575.21
  ))
})

# test_that("makeAUC PM_load",{
#   res <- PM_load(1)
#   auc1 <- res$op$auc()
#   summary_auc1 <- summary(auc1$tau)
#   expect_equal(round(as.numeric(summary_auc1),2), c(89.90, 177.93, 235.54,
#                                                     243.92, 289.45, 471.35))

#   auc2 <- res$post$auc()
#   summary_auc2 <- summary(auc2$tau)
#   expect_equal(round(as.numeric(summary_auc2),2), c(458.98,  738.67, 1052.52,
#                                                     1054.27, 1246.86, 1957.90))

#   auc3 <- res$pop$auc()
#   summary_auc3 <- summary(auc3$tau)
#   expect_equal(round(as.numeric(summary_auc3),2), c(817.26, 1089.68, 1090.02,
#                                                     1062.72, 1090.02, 1091.380))

#   df1 <- tibble(id = 1, time = 1:24, out = 100 * exp(-0.1 * time))
#   df2 <- tibble(id = 2, time = 1:24, out = 75 * exp(-0.07 * time))
#   df <- bind_rows(df1, df2)

#   auc4 <- makeAUC(df, out~time)
#   summary_auc4 <- summary(auc4$tau)
#   expect_equal(round(as.numeric(summary_auc4),2), c(799.63, 803.42, 807.22,
#                                                     807.22, 811.01, 814.80))
# })
