test_that("the options app uses a 20 percent smaller base font", {
  theme <- Pmetrics:::.pmoptions_theme()

  expect_identical(
    unname(bslib::bs_get_variables(theme, "font-size-base")),
    "0.8rem"
  )
})

test_that("options app dropdowns can escape card boundaries", {
  css <- Pmetrics:::.pmoptions_dropdown_css()

  expect_match(css, "\\.bslib-card > \\.card-body")
  expect_match(css, "overflow: visible")
  expect_match(css, "\\.selectize-dropdown")
  expect_match(css, "z-index: 1060")
})
