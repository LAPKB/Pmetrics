test_that("is_converged_status recognises old and PMcore status text", {
  expect_true(is_converged_status("Converged"))
  expect_true(is_converged_status("Stopped (converged)"))
  expect_false(is_converged_status("Stopped (maximum cycles reached)"))
  expect_false(is_converged_status("Posterior"))
  expect_false(is_converged_status(character(0)))
})
